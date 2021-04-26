module Glean.Query.Derive
  ( derivePredicate
  , pollDerivation
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Default
import Data.Either
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

import Glean.Angle.Types
import Glean.Database.Stuff
import Glean.Database.Types as Database
import Glean.Database.Writes
import qualified Glean.Query.UserQuery as UserQuery
import Glean.Schema.Util
import Glean.Types as Thrift
import Glean.Util.Observed as Observed
import Glean.Util.Time
import Glean.Util.Warden

-- | Compute and store the specified derived predicate
derivePredicate
  :: Database.Env
  -> Thrift.Repo
  -> Thrift.DerivePredicateQuery
  -> IO Thrift.DerivePredicateResponse
derivePredicate env repo Thrift.DerivePredicateQuery{..} = do
  handle <- UUID.toText <$> UUID.nextRandom
  startDerivation env handle
  let handleFailure e = failDerivation env ref handle e >> throwIO e
  spawn_ (envWarden env)
    $ handleAll handleFailure
    $ runDerivation env repo handle query
  return $ Thrift.DerivePredicateResponse handle
  where
    ref = SourceRef
      derivePredicateQuery_predicate
      derivePredicateQuery_predicate_version

    query = def
      { userQuery_predicate = derivePredicateQuery_predicate
      , userQuery_predicate_version = derivePredicateQuery_predicate_version
      , userQuery_query = Text.encodeUtf8 $ showSourceRef ref <> " _"
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

runDerivation
  :: Database.Env
  -> Thrift.Repo
  -> Handle
  -> Thrift.UserQuery
  -> IO ()
runDerivation env repo handle query = do
  config <- Observed.get (envServerConfig env)
  readDatabase env repo $ \schema lookup -> do
  let
    loop q = do
      result <- try $ UserQuery.userQueryWrites env schema config lookup repo q
      case result of
        Left Thrift.Retry{..} -> retry retry_seconds (loop q)
        Right res@(_, mcont, _) -> do
          updateDerivationProgress env handle res
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
  -> Thrift.Handle
  -> IO ()
startDerivation env handle = do
  now <- getTimePoint
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.insert handle Derivation
        { derivationStart = now
        , derivationQueryingFinished = False
        , derivationStats = def
        , derivationPendingWrites = []
        , derivationError = Nothing
        }

failDerivation
  :: Database.Env
  -> SourceRef
  -> Thrift.Handle
  -> SomeException
  -> IO ()
failDerivation env ref handle e = do
  logError $ "Failed derivation of "
    <> Text.unpack (showSourceRef ref) <> ": " <> show e
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.adjust (\d -> d { derivationError = Just e }) handle

updateDerivationProgress
  :: Database.Env
  -> Thrift.Handle
  -> (Thrift.UserQueryStats, Maybe Thrift.UserQueryCont, Maybe Thrift.Handle)
  -> IO ()
updateDerivationProgress env handle (stats, mcont, mWriteHandle) = do
  now <- getTimePoint
  atomically
    $ modifyTVar' (envDerivations env)
    $ HashMap.adjust (adjust now) handle
  where
    adjust now derivation@Derivation{..} = derivation
      { derivationQueryingFinished = isNothing mcont
      , derivationStats = (mergeStats derivationStats stats)
          { userQueryStats_elapsed_ns = fromIntegral
              $ toDiffMicros
              $ diffTimePoints derivationStart now
          }
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

pollDerivation
  :: Database.Env
  -> Thrift.Handle
  -> IO Thrift.DerivationProgress
pollDerivation env@Env{..} handle = do
  completed <- do
    derivations <- readTVarIO envDerivations
    Derivation{..} <- maybe (throwIO Thrift.UnknownDerivationHandle) return
      $ HashMap.lookup handle derivations

    finished <- for derivationPendingWrites $ \writeHandle -> do
      eFinished <- isWriteFinished env writeHandle
      return $ (writeHandle,) <$> eFinished

    case partitionEithers finished of
      ([], noerror) -> return $ map fst $ filter snd noerror
      (err:_, _) -> do
        atomically
          $ modifyTVar' envDerivations
          $ HashMap.delete handle
        throwIO err

  mask $ \unmask -> do
  -- remove completed writes
  d@Derivation{..} <- unmask $ atomically $ do
    derivations <- readTVar envDerivations
    withoutCompleted <- case HashMap.lookup handle derivations of
      Nothing -> throwSTM Thrift.UnknownDerivationHandle
      Just d -> return d
        { derivationPendingWrites =
            derivationPendingWrites d \\ completed
        }
    writeTVar envDerivations $
      if isFinished withoutCompleted
         then HashMap.delete handle derivations
         else HashMap.insert handle withoutCompleted derivations
    return withoutCompleted

  if isFinished d
    then case derivationError of
      Nothing -> return $ Thrift.DerivationProgress_complete derivationStats
      Just err -> throwIO err
    else do
      elapsed <- getElapsedTime derivationStart
      return $ Thrift.DerivationProgress_ongoing derivationStats
        { userQueryStats_elapsed_ns = fromIntegral $ toDiffMicros elapsed }

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
