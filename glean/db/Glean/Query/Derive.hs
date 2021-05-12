module Glean.Query.Derive
  ( derivePredicate
  , pollDerivation
  , deriveStored
  ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Control.Applicative
import Control.Monad
import Data.Default
import Data.Either
import Data.Foldable
import qualified Data.Set as Set
import Data.Set (Set)
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

import Glean.RTS.Types as RTS
import Glean.Angle.Types as A
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Schema.Types
import Glean.Database.Stuff
import Glean.Database.Types as Database
import Glean.Database.Writes
import qualified Glean.Query.UserQuery as UserQuery
import Glean.Query.Typecheck.Types
import Glean.Query.Codegen
import Glean.Schema.Util
import Glean.Types as Thrift hiding (Byte, Nat)
import Glean.Util.Observed as Observed
import Glean.Util.Time
import Glean.Util.Warden

-- | Compute and store the specified derived predicate
derivePredicate
  :: Database.Env
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> IO Thrift.DerivePredicateResponse
derivePredicate env repo query = do
  Derivation{..} <- deriveStoredImpl env repo query
  return $ Thrift.DerivePredicateResponse derivationHandle

-- | Check the progress of a derivation
pollDerivation :: Database.Env -> Thrift.Handle -> IO Thrift.DerivationProgress
pollDerivation env handle = do
  (repo, pred) <- atomically $ predicateForHandle env handle
  derivation@Derivation{..} <- updateProgress env repo pred
  case derivationError of
    Just err -> throwIO err
    Nothing -> if isFinished derivation
      then return $ Thrift.DerivationProgress_complete derivationStats
      else do
        elapsed <- getElapsedTime derivationStart
        return $ Thrift.DerivationProgress_ongoing derivationStats
          { userQueryStats_elapsed_ns = fromIntegral $ toDiffMicros elapsed }

predicateForHandle
  :: Database.Env
  -> Thrift.Handle
  -> STM (Repo, PredicateRef)
predicateForHandle env handle = do
  derivations <- HashMap.toList <$> readTVar (envDerivations env)
  case find ((handle ==) . derivationHandle . snd) derivations of
    Nothing -> throwSTM Thrift.UnknownDerivationHandle
    Just (k, _) -> return k

deriveStored
  :: Database.Env
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> IO Thrift.DerivationStatus
deriveStored env repo query = do
  d@Derivation{..} <- deriveStoredImpl env repo query
  case derivationError of
    Just err -> throwIO err
    Nothing -> return $
      if isFinished d
         then Thrift.DerivationStatus_complete def
         else Thrift.DerivationStatus_ongoing def

-- | Start predicate derivation or return a completed/ongoing one if it exists
deriveStoredImpl
  :: Database.Env
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> IO Derivation
deriveStoredImpl env repo Thrift.DerivePredicateQuery{..} = do
  pred <- passingConstraints
  handle <- UUID.toText <$> UUID.nextRandom
  now <- getTimePoint
  mask $ \unmask -> do
  started <- unmask $ atomically $ startDerivation env repo pred now handle
  handleAll (onErr pred) $ unmask $ case started of
    -- already started
    Left old -> return old
    -- new derivation
    Right new -> do
        kickOff pred
        return new
  where
    kickOff :: PredicateRef -> IO ()
    kickOff pred = do
      spawn_ (envWarden env) $ handleAll (onErr pred) $ do
        runDerivation env repo pred $ query pred
        enqueueCheckpoint env repo $ void $ updateProgress env repo pred

    onErr :: PredicateRef -> SomeException -> IO a
    onErr pred e = failDerivation env repo pred e >> throwIO e

    passingConstraints :: IO PredicateRef
    passingConstraints = readDatabase env repo $ \schema _ -> do
      let mdetails = lookupPredicate
            (SourceRef derivePredicateQuery_predicate
              derivePredicateQuery_predicate_version)
            (envSchemaVersion env)
            schema
      pred <- case mdetails  of
        Nothing -> throwIO Thrift.UnknownPredicate
        Just details -> return $ predicateRef details

      unless (isDerivedAndStored schema pred) $
        throwIO Thrift.NotAStoredPredicate

      completePreds <- atomically $
        metaCompletePredicates <$> Catalog.readMeta (envCatalog env) repo

      let complete = isCompletePred completePreds schema
      let dependencies = transitive (predicateDeps schema) pred
      case filter (not . complete) dependencies of
        [] -> return pred
        incomplete -> throwIO $ Thrift.IncompleteDependencies incomplete

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

markPredicateAsComplete :: Database.Env -> Repo -> PredicateRef -> STM ()
markPredicateAsComplete env repo pred = void
  $ Catalog.modifyMeta (envCatalog env) repo
  $ \meta -> return meta
      { metaCompletePredicates =
        insertUnique pred $ metaCompletePredicates meta
      }
  where
    insertUnique x xs = x : filter (/= x) xs

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
predicateDeps schema pred = maybe mempty (toList . mconcat) $ do
  let PredicateDetails{..} = getPredicateDetails schema pred
  Derive _ QueryWithInfo{..} <- return predicateDeriving
  let TcQuery _ _ _ stmts = qiQuery
  return [ typeDeps ty | TcStatement ty _ _ <- stmts ]

getPredicateDetails :: DbSchema -> PredicateRef -> PredicateDetails
getPredicateDetails schema pred =
  case lookupPredicateRef pred schema of
    Just details -> details
    Nothing -> error $
      "unknown predicate: " <> Text.unpack (showPredicateRef pred)

typeDeps :: RTS.Type -> Set PredicateRef
typeDeps = \case
  Byte -> mempty
  Nat -> mempty
  String -> mempty
  Array ty -> typeDeps ty
  Record fields -> foldMap (typeDeps . fieldDefType) fields
  Sum fields -> foldMap (typeDeps . fieldDefType) fields
  Predicate (PidRef _ pred) -> Set.singleton pred
  NamedType (ExpandedType _ ty) -> typeDeps ty
  Maybe ty -> typeDeps ty
  Enumerated _ -> mempty
  Boolean -> mempty

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
          addProgress env repo pred res
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

startDerivation
  :: Database.Env
  -> Repo
  -> PredicateRef
  -> TimePoint
  -> Thrift.Handle
  -> STM (Either Derivation Derivation) -- ^ existing derivation or new one
startDerivation env repo pred now handle = do
  running <- HashMap.lookup (repo, pred) <$> readTVar (envDerivations env)
  case running of
    Just derivation -> return $ Left derivation
    Nothing -> do
      let new = Derivation
            { derivationStart = now
            , derivationQueryingFinished = False
            , derivationStats = def
            , derivationPendingWrites = []
            , derivationError = Nothing
            , derivationHandle = handle
            }
      modifyTVar' (envDerivations env) $ HashMap.insert (repo, pred) new
      return $ Right new

failDerivation
  :: Database.Env
  -> Repo
  -> PredicateRef
  -> SomeException
  -> IO ()
failDerivation env repo pred e = do
  logError $ "Failed derivation of "
    <> Text.unpack (showPredicateRef pred) <> ": " <> show e
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.adjust (\d -> d { derivationError = Just e }) (repo, pred)

addProgress
  :: Database.Env
  -> Repo
  -> PredicateRef
  -> (Thrift.UserQueryStats, Maybe Thrift.UserQueryCont, Maybe Thrift.Handle)
  -> IO ()
addProgress env repo pred (stats, mcont, mWriteHandle) =
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.adjust adjust (repo, pred)
  where
    adjust derivation@Derivation{..} = derivation
      { derivationQueryingFinished = isNothing mcont
      , derivationStats = mergeStats derivationStats stats
      , derivationPendingWrites = maybeToList mWriteHandle
          ++ derivationPendingWrites
      }

    mergeStats a b =
      let add f = f a + f b in
      -- compile_time and bytecode_size do not accumulate. We take the last one.
      b
        { userQueryStats_num_facts = add userQueryStats_num_facts
        , userQueryStats_allocated_bytes = add userQueryStats_allocated_bytes
        , userQueryStats_facts_searched = userQueryStats_facts_searched a
            <> userQueryStats_facts_searched b
        , userQueryStats_execute_time_ns = Just $ add
            $ fromMaybe 0 . userQueryStats_execute_time_ns
        , userQueryStats_result_count = add userQueryStats_result_count
        }

-- | Update envDerivations and the Catalog metadata to reflect the current
-- derivation status
updateProgress
  :: Database.Env
  -> Repo
  -> PredicateRef
  -> IO Derivation
updateProgress env@Env{..} repo pred = do
  deriv <- atomically getDerivation
  if isFinished deriv
    then return deriv
    else do
      finished <- finishedWrites deriv
      now <- getTimePoint
      atomically $ commitProgress now finished
  where
    finishedWrites :: Derivation -> IO (Either SomeException [Thrift.Handle])
    finishedWrites deriv = do
      results <- for (derivationPendingWrites deriv) $ \writeHandle -> do
        fmap (writeHandle,) <$> isWriteFinished env writeHandle
      return $ map fst . filter snd <$> sequence results

    commitProgress now finished = do
      let addProgress d = d
            { derivationPendingWrites =
                derivationPendingWrites d \\ fromRight [] finished
            , derivationError =
                derivationError d <|> either Just (const Nothing) finished
            , derivationStats = (derivationStats d)
                { userQueryStats_elapsed_ns = fromIntegral
                    $ toDiffMicros
                    $ diffTimePoints (derivationStart d) now
                }
            }
      withProgress@Derivation{..} <- addProgress <$> getDerivation

      modifyTVar' envDerivations $ case derivationError of
        Just _ -> HashMap.delete (repo, pred)
        Nothing -> HashMap.insert (repo, pred) withProgress

      when (isFinished withProgress && isNothing derivationError)
        $ markPredicateAsComplete env repo pred

      return withProgress

    getDerivation = do
      ds <- readTVar envDerivations
      case HashMap.lookup (repo, pred) ds of
        Nothing -> error "unknown derivation"
        Just d -> return d

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
