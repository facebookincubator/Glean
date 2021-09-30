-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Query.Derive
  ( pollDerivation
  , deriveStored
  ) where

import Control.DeepSeq
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Data.Default
import Data.Either
import Data.Foldable
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import Data.List ((\\))
import Data.Maybe
import qualified Data.Text as Text
import Data.Traversable (for)
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Util.Control.Exception
import Util.Log

import Glean.Angle.Types as A
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Schema.Types
import Glean.Database.Storage as Storage
import Glean.Database.Open
import Glean.Database.Types as Database
import Glean.Database.Writes
import Glean.Internal.Types hiding (Predicate)
import qualified Glean.Query.UserQuery as UserQuery
import Glean.Query.Typecheck (tcQueryDeps)
import Glean.Query.Codegen
import Glean.Schema.Util
import qualified Glean.ServerConfig.Types as ServerConfig
import Glean.Types as Thrift hiding (Byte, Nat, Exception)
import qualified Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.Mutex
import Glean.Util.Time
import Glean.Util.Warden

-- | Check the progress of a derivation
pollDerivation :: Database.Env -> Thrift.Handle -> IO Thrift.DerivationProgress
pollDerivation env handle = do
  (repo, pred, _) <- derivationForHandle env handle
  let query = def
        { derivePredicateQuery_predicate = predicateRef_name pred
        , derivePredicateQuery_predicate_version =
            Just $ predicateRef_version pred
        }
  derivation@Derivation{..} <- deriveStoredImpl env noLogging repo query
  case derivationError of
    Just (_, err) -> throwIO err
    Nothing -> do
      elapsed <- getElapsedTime derivationStart
      return $ if isFinished derivation
        then Thrift.DerivationProgress_complete derivationStats
        else Thrift.DerivationProgress_ongoing derivationStats
          { userQueryStats_elapsed_ns = fromIntegral $ toDiffMicros elapsed }

type LogResult =
  Either (DiffTimePoints, SomeException) Thrift.UserQueryStats -> IO ()

noLogging :: LogResult
noLogging = const mempty

derivationForHandle
  :: Database.Env
  -> Thrift.Handle
  -> IO (Repo, PredicateRef, Derivation)
derivationForHandle env handle = do
  derivations <- HashMap.toList <$> readTVarIO (envDerivations env)
  case find ((handle ==) . derivationHandle . snd) derivations of
    Nothing -> throwIO Thrift.UnknownDerivationHandle
    Just ((repo, pred), d) -> return (repo, pred, d)

deriveStored
  :: Database.Env
  -> LogResult
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> IO Thrift.DerivationStatus
deriveStored env log repo query = do
  d@Derivation{..} <- deriveStoredImpl env log repo query
  case derivationError of
    Just (_, err) -> throwIO err
    Nothing -> return $
      if isFinished d
         then Thrift.DerivationStatus_complete def
            { Thrift.derivationComplete_stats = derivationStats }
         else Thrift.DerivationStatus_ongoing def
            { Thrift.derivationOngoing_stats = derivationStats }

logResult :: LogResult -> Derivation -> IO ()
logResult log Derivation{..} =
  log $ case derivationError of
    Nothing -> Right derivationStats
    Just (errTime, err) ->
      let duration = diffTimePoints derivationStart errTime in
      Left (duration, err)

-- | Start predicate derivation or return a completed/ongoing one if it exists
deriveStoredImpl
  :: Database.Env
  -> LogResult
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> IO Derivation
deriveStoredImpl env@Env{..} log repo Thrift.DerivePredicateQuery{..} =
  readDatabase env repo $ \odb _ -> do
  let schema = odbSchema odb
  pred <- getPredicateRef schema
  handle <- UUID.toText <$> UUID.nextRandom
  now <- getTimePoint

  mask $ \unmask -> do
  join $ unmask $ atomically $ do
    running <- HashMap.lookup (repo, pred) <$> readTVar envDerivations
    case running of
      Just derivation -> return $ return derivation
      Nothing -> do
        checkConstraints schema pred
        let new = newDerivation now handle
        save env repo pred new
        return $ handleAll (onErr pred) $ unmask $ do
          kickOff pred
          return new
  where
    newDerivation now handle = Derivation
      { derivationStart = now
      , derivationQueryingFinished = False
      , derivationStats = def
      , derivationPendingWrites = []
      , derivationError = Nothing
      , derivationHandle = handle
      }

    kickOff :: PredicateRef -> IO ()
    kickOff pred = do
      spawn_ envWarden $ handleAll (onErr pred) $ do
        runDerivation env repo pred $ query pred
        enqueueCheckpoint env repo $ void $ finishDerivation env log repo pred

    onErr :: PredicateRef -> SomeException -> IO a
    onErr pred e = do
      logError $ "Failed derivation of " <>
        Text.unpack (showPredicateRef pred) <> ": " <> show e
      now <- getTimePoint
      void $ overDerivation env repo pred
        (\d -> d { derivationError = Just (now, e) })
      throwIO e

    getPredicateRef :: DbSchema -> IO PredicateRef
    getPredicateRef schema = do
      dbSchemaVersion <- getDbSchemaVersion env repo
      let
        schemaVersion =
          maybe LatestSchemaAll SpecificSchemaAll $
            envSchemaVersion <|> dbSchemaVersion

        mdetails = lookupPredicate
            (SourceRef derivePredicateQuery_predicate
              derivePredicateQuery_predicate_version)
            schemaVersion
            schema

      case mdetails of
        Nothing -> throwIO Thrift.UnknownPredicate
        Just details -> return $ predicateRef details

    checkConstraints :: DbSchema -> PredicateRef -> STM ()
    checkConstraints schema pred = do
      unless (isDerivedAndStored schema pred) $
        throwSTM Thrift.NotAStoredPredicate

      ServerConfig.Config{..} <- Observed.get envServerConfig
      unless config_disable_predicate_dependency_checks $ do
        completePreds <-
          metaCompletePredicates <$> Catalog.readMeta envCatalog repo
        let complete = isCompletePred completePreds schema
            dependencies = transitive (predicateDeps schema) pred
            incomplete = filter (not . complete) dependencies
        unless (null incomplete) $
          throwSTM $ Thrift.IncompleteDependencies incomplete

    query pred = def
      { userQuery_predicate = derivePredicateQuery_predicate
      , userQuery_predicate_version = derivePredicateQuery_predicate_version
      , userQuery_query = Text.encodeUtf8 $ showPredicateRef pred <> " _"
      , userQuery_options = Just opts
      , userQuery_encodings = []
      }

    opts = def
      { userQueryOptions_expand_results = False
      , userQueryOptions_syntax = QuerySyntax_ANGLE
      , userQueryOptions_store_derived_facts = True
      , userQueryOptions_max_bytes = maxBytes
      , userQueryOptions_max_results = maxResults
      , userQueryOptions_max_time_ms = maxTime
      , userQueryOptions_omit_results = True
      , userQueryOptions_continuation = Nothing
      }

    maxBytes = derivePredicateQuery_options
      >>= derivePredicateOptions_max_bytes_per_query
    maxResults = derivePredicateQuery_options
      >>= derivePredicateOptions_max_results_per_query
    maxTime = derivePredicateQuery_options
      >>= derivePredicateOptions_max_time_ms_per_query

overDerivation
  :: Database.Env
  -> Repo
  -> PredicateRef
  -> (Derivation -> Derivation)
  -> IO Derivation
overDerivation env repo pred f = atomically $ do
  derivation <- f <$> getDerivation env repo pred
  save env repo pred derivation
  return derivation

save :: Database.Env -> Repo -> PredicateRef -> Derivation -> STM ()
save Env{..} repo pred derivation@Derivation{..} = do
  case derivationError of
    Just e -> markDbBroken e
    Nothing -> when (isFinished derivation) markPredicateAsComplete
  modifyTVar' envDerivations $ HashMap.insert (repo, pred) (force derivation)
  where
    markPredicateAsComplete = void
      $ Catalog.modifyMeta envCatalog repo
      $ \meta -> return meta
          { metaCompletePredicates =
            insertUnique pred $ metaCompletePredicates meta
          }

    insertUnique x xs = x : filter (/= x) xs

    markDbBroken err = void
      $ Catalog.modifyMeta envCatalog repo
      $ \meta -> return meta
          { metaCompleteness = Broken (DatabaseBroken task reason)
          }
      where
        task = "derivation of " <> showPredicateRef pred
        reason = Text.pack (show err)

isCompletePred
  :: [PredicateRef]
  -> DbSchema
  -> PredicateRef
  -> Bool
isCompletePred completePreds schema pred =
  case predicateDeriving $ getPredicateDetails schema pred of
    NoDeriving -> True
    Derive DeriveIfEmpty _ -> False
    Derive DeriveOnDemand _ -> True
    Derive DerivedAndStored _ -> pred `elem` completePreds

isDerivedAndStored :: DbSchema -> PredicateRef -> Bool
isDerivedAndStored schema pred =
  case predicateDeriving $ getPredicateDetails schema pred of
    Derive DerivedAndStored _ -> True
    _ -> False

transitive :: Ord a => (a -> [a]) -> a -> [a]
transitive next root = Set.elems $ go (next root) mempty
  where
    go [] visited = visited
    go (x:xs) visited
      | x `Set.member`visited = go xs visited
      | otherwise = go xs $ go (next x) $ Set.insert x visited

-- | predicates which are queried to derive this predicate
predicateDeps :: DbSchema -> PredicateRef -> [PredicateRef]
predicateDeps schema pred =
  case predicateDeriving of
    Derive _ QueryWithInfo{..} -> toList $ tcQueryDeps qiQuery
    _ -> mempty
  where
    PredicateDetails{..} = getPredicateDetails schema pred

getPredicateDetails :: DbSchema -> PredicateRef -> PredicateDetails
getPredicateDetails schema pred =
  case lookupPredicateRef pred schema of
    Just details -> details
    Nothing -> error $
      "unknown predicate: " <> Text.unpack (showPredicateRef pred)

-- | exhaust a query (until there is no more continuation)
runDerivation
  :: Database.Env
  -> Thrift.Repo
  -> PredicateRef
  -> Thrift.UserQuery
  -> IO ()
runDerivation env repo pred query = do
  config <- Observed.get (envServerConfig env)
  readDatabase env repo $ \schema lookup -> do
  let
    loop q = do
      result <- try $ UserQuery.userQueryWrites env schema config lookup repo q
      case result of
        Left Thrift.Retry{..} -> retry retry_seconds (loop q)
        Right res@(_, mcont, _) -> do
          addProgress res
          case mcont of
            Just cont -> loop $ query `withCont`cont
            Nothing -> return ()
  loop query
  where
    retry :: Double -> IO a -> IO a
    retry secs action = do
      threadDelay $ round $ max secs 1 * 1000000
      action

    withCont q cont = q
      { userQuery_options = Just (fromMaybe def $ userQuery_options q)
        { userQueryOptions_continuation = Just cont }
      }

    addProgress (stats, mcont, mWriteHandle) =
      void $ overDerivation env repo pred $ \d@Derivation{..} -> d
        { derivationQueryingFinished = isNothing mcont
        , derivationStats = mergeStats derivationStats stats
        , derivationPendingWrites = maybeToList mWriteHandle
            ++ derivationPendingWrites
        }

    mergeStats a b =
      let add f = f a + f b
          addMaybe f = liftA2 (+) (f a) (f b) <|> f a <|> f b
      in
      -- bytecode_size does not accumulate. We take the last one.
      b
        { userQueryStats_compile_time_ns =
            addMaybe userQueryStats_compile_time_ns
        , userQueryStats_num_facts = add userQueryStats_num_facts
        , userQueryStats_allocated_bytes = add userQueryStats_allocated_bytes
        , userQueryStats_facts_searched = userQueryStats_facts_searched a
            <> userQueryStats_facts_searched b
        , userQueryStats_execute_time_ns =
            addMaybe userQueryStats_execute_time_ns
        , userQueryStats_result_count = add userQueryStats_result_count
        }

-- | Update envDerivations and the Catalog metadata to reflect the current
-- derivation status
finishDerivation
  :: Database.Env
  -> LogResult
  -> Repo
  -> PredicateRef
  -> IO Derivation
finishDerivation env@Env{..} log repo pred = do
  finished <- finishedWrites =<< atomically (getDerivation env repo pred)
  now <- getTimePoint
  derivation <- overDerivation env repo pred $ withProgress now finished
  logResult log derivation
  when (isNothing (derivationError derivation)) finishOwnership
  return derivation
  where
    finishedWrites :: Derivation -> IO (Either SomeException [Thrift.Handle])
    finishedWrites deriv = do
      results <- for (derivationPendingWrites deriv) $ \writeHandle -> do
        fmap (writeHandle,) <$> isWriteFinished env writeHandle
      return $ map fst . filter snd <$> sequence results

    withProgress now finished d =
      let pendingWrites = derivationPendingWrites d \\ fromRight [] finished in
      d { derivationPendingWrites = pendingWrites
        , derivationError =
            derivationError d
            <|> case finished of
                  Left err -> Just (now, err)
                  Right _ -> Nothing
            <|> if null pendingWrites
                then Nothing
                else Just (now, toException FinishedWithPendingWrites)
        , derivationStats = (derivationStats d)
            { userQueryStats_elapsed_ns = fromIntegral
                $ toDiffNanos
                $ diffTimePoints (derivationStart d) now
            }
        }

    finishOwnership = do
      withOpenDatabase env repo $ \OpenDB{..} -> do
        writing <- case odbWriting of
          Nothing -> throwIO $ Thrift.Exception "finishOwnership: read only"
          Just writing -> return writing
        details <- case lookupPredicateRef pred odbSchema of
          Nothing -> throwIO $ Thrift.Exception "finishOwnership: no pid"
          Just details -> return details
        maybeOwnership <- readTVarIO odbOwnership
        -- we need the write lock while computing the final ownership,
        -- because we might write new ownership sets to the DB. This
        -- step should hopefully be fast though (TOOD: measure)
        forM_ maybeOwnership $ \ownership ->
          withMutex (wrLock writing) $ \_ -> do
            computed <- Storage.computeDerivedOwnership odbHandle ownership
              (predicatePid details)
            Storage.storeOwnership odbHandle computed

getDerivation :: Database.Env -> Repo -> PredicateRef -> STM Derivation
getDerivation env repo pred = do
  ds <- readTVar (envDerivations env)
  case HashMap.lookup (repo, pred) ds of
    Nothing -> throwSTM $ toException UnknownDerivation
    Just d -> return d

data UnknownDerivation = UnknownDerivation deriving (Show)
instance Exception UnknownDerivation

data DerivationFailed = FinishedWithPendingWrites
  deriving (Show)
instance Exception DerivationFailed

isFinished :: Derivation -> Bool
isFinished Derivation{..} =
    isJust derivationError ||
    (derivationQueryingFinished && null derivationPendingWrites)

isWriteFinished
  :: Database.Env
  -> Thrift.Handle
  -> IO (Either SomeException Bool)
isWriteFinished env writeHandle = do
  res <- tryAll $ pollBatch env writeHandle
  return $ case res of
    Right (Thrift.FinishResponse_subst _) -> Right True
    Right _ -> Right False
    Left exc -> case fromException exc of
      -- caused by concurrent calls to pollDerivation?
      Just Thrift.UnknownBatchHandle -> Right True
      Nothing -> Left exc
