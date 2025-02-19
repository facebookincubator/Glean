{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.CompletePredicates
  ( completePredicates
  , syncCompletePredicates
  ) where

import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import System.Timeout

import Util.Control.Exception
import Util.Defer
import Util.Log.Text
import Util.Logger
import Util.STM

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Open
import Glean.Database.Schema
import Glean.Database.Schema.Types
  ( lookupPredicateSourceRef
  , SchemaSelector(..)
  )
import qualified Glean.Database.Storage as Storage
import Glean.Database.Types
import Glean.Internal.Types as Thrift
import Glean.Logger
import Glean.RTS.Foreign.Ownership
import Glean.RTS.Types (Pid)
import Glean.Schema.Util (convertRef)
import Glean.Types as Thrift
import qualified Glean.Util.Warden as Warden
import Glean.Util.Mutex


-- | Client API: kick off the completion process for a DB asynchronously
completePredicates
  :: Env
  -> Repo
  -> Thrift.CompletePredicates
  -> IO CompletePredicatesResponse
completePredicates env repo complete =
  case complete of
    Thrift.CompletePredicates_axiom _ ->
      completeAxiomPredicates env repo
    Thrift.CompletePredicates_derived (Thrift.CompleteDerivedPredicate pred) ->
      completeDerivedPredicate env repo pred

-- For internal use: actually perform completion for a DB
syncCompletePredicates :: Env -> Repo -> IO ()
syncCompletePredicates env repo =
  -- log this, because the completePredicates request itself is async
  -- and doesn't record the total time spent.
  loggingAction
    (runLogRepo "completePredicates(server)" env repo) (const mempty) $ do
  maybeBase <- repoParent env repo
  let withBase repo f =
        readDatabase env repo $ \_ lookup -> f (Just lookup)
  maybe ($ Nothing) withBase maybeBase $ \base -> do
  withOpenDatabase env repo $ \OpenDB{..} -> do
    own <- Storage.computeOwnership odbHandle base
      (schemaInventory odbSchema)
    withWriteLock odbWriting $ \lock ->
      Storage.storeOwnership odbHandle lock own
    maybeOwnership <- readTVarIO odbOwnership
    forM_ maybeOwnership $ \ownership -> do
      stats <- getOwnershipStats ownership
      logInfo $ "ownership propagation complete: " <> showOwnershipStats stats

completeAxiomPredicates :: Env -> Repo -> IO CompletePredicatesResponse
completeAxiomPredicates env@Env{..} repo = do
  let
    doCompletion = -- we are masked in here
      (`finally` deregister) $ do
      r <- tryAll $ syncCompletePredicates env repo
      case r of
        Left (e :: SomeException) -> do
          setBroken repo envCatalog "completePredicates" e
          throwIO e
        Right{} -> setComplete

    deregister =
      atomically $ modifyTVar envCompleting $ HashMap.delete repo

    setComplete = void $ atomically $
      Catalog.modifyMeta envCatalog repo $ \meta ->
        return meta { metaAxiomComplete = True }

    isInProgress = do
      completing <- now $ readTVar envCompleting
      return (HashMap.lookup repo completing)

    storeComputation async =
      modifyTVar envCompleting (HashMap.insert repo async)

  scheduleCompletion
    env repo SkipIfComplete doCompletion isInProgress storeComputation

-- | Propagate ownership information for an externally derived predicate.
syncCompleteDerivedPredicate :: Env -> Repo -> Pid -> IO ()
syncCompleteDerivedPredicate env repo pid =
  withOpenDatabase env repo $ \OpenDB{..} -> do
  maybeOwnership <- readTVarIO odbOwnership
  forM_ maybeOwnership $ \ownership -> do
    maybeBase <- repoParent env repo
    let withBase repo f = readDatabase env repo $ \_ lookup -> f (Just lookup)
    maybe ($ Nothing) withBase maybeBase $ \base ->
      withWriteLock odbWriting $ \lock -> do
        computed <- Storage.computeDerivedOwnership
          odbHandle lock ownership base pid
        Storage.storeOwnership odbHandle lock computed

withWriteLock
  :: Maybe Writing
  -> (forall w . Storage.WriteLock w -> IO b)
  -> IO b
withWriteLock Nothing f = f (undefined :: Storage.WriteLock ())
  -- if there's no write lock, we must be in the finalization
  -- phase and the DB has already been marked read-only. We have
  -- exclusive write access at this point so it's safe to
  -- continue.
withWriteLock (Just writing) f =
  withMutexSafe (wrLock writing) f

-- | Kick off completion of externally derived predicate asynchronously
completeDerivedPredicate
  :: Env
  -> Repo
  -> PredicateRef
  -> IO CompletePredicatesResponse
completeDerivedPredicate env@Env{..} repo pred = do
  details <- withOpenDatabase env repo $ \odb ->
    predicateDetails (odbSchema odb) pred
  completing <- readTVarIO envCompletingDerived
  let
      doCompletion = do -- we are masked in here
        r <- tryAll $
          syncCompleteDerivedPredicate env repo (predicatePid details)
        case r of
          Left (e :: SomeException) -> do
            setBroken repo envCatalog "completeDerivedPredicate" e
            throwIO e
          Right{} -> return ()

      derivations = HashMap.lookupDefault mempty repo completing

      predId = predicateId details

      isInProgress = do
        return (HashMap.lookup predId derivations)

      storeComputation async =
        modifyTVar envCompletingDerived $
          HashMap.insert repo $
          HashMap.insert predId async derivations

  scheduleCompletion
    env repo FailIfNotComplete doCompletion isInProgress storeComputation
  where
  predicateDetails schema pred =
    case lookupPredicateSourceRef (convertRef pred) LatestSchema schema of
      Right details -> return details
      Left err ->
        throwIO $ Thrift.Exception $ "completeDerivedPredicate: " <> err

data OnAxiomComplete
  = SkipIfComplete
  | FailIfNotComplete

scheduleCompletion
  :: Env
  -> Repo
  -> OnAxiomComplete
  -> IO ()
  -> Defer IO STM (Maybe (Async ()))
  -> (Async () -> STM ())
  -> IO CompletePredicatesResponse
scheduleCompletion Env{..} repo onAxiomComplete
  doCompletion isInProgress storeComputation = do
    mask_ $ do
    -- speculatively spawn a thread to do the completion, we'll cancel
    -- this if we don't need it. This is so that we can atomically
    -- start the job and update the Env state together.
    tmvar <- newEmptyTMVarIO
    async <- Warden.spawn envWarden $
      atomically (takeTMVar tmvar) >> doCompletion
    join $ immediately $ do
      meta <- now $ Catalog.readMeta envCatalog repo
      if
        | SkipIfComplete <- onAxiomComplete, metaAxiomComplete meta -> do
          later $ cancel async -- already done
          return (return CompletePredicatesResponse{})
        | FailIfNotComplete <- onAxiomComplete, not $ metaAxiomComplete meta ->
          now $ throwSTM $ Thrift.Exception "DB is not complete."
        | Broken b <- metaCompleteness meta -> do
          later $ cancel async
          now $ throwSTM $ Exception $ databaseBroken_reason b
        | envReadOnly -> do
          later $ cancel async
          now $ throwSTM $ Exception "DB is read-only"
        | otherwise -> do
          inProgress <- isInProgress
          case inProgress of
            Just existingAsync -> do  -- in progress
              later $ cancel async
              return $ waitFor existingAsync CompletePredicatesResponse{}
            Nothing -> now $ do  -- start async completion
              void $ tryPutTMVar tmvar ()
              storeComputation async
              return $ waitFor async CompletePredicatesResponse{}

-- If a synchronous exception is thrown during completion, this is
-- not recoverable, so mark the DB as broken. The exception is also
-- propagated to the caller.
setBroken :: Repo -> Catalog.Catalog -> Text.Text -> SomeException -> IO ()
setBroken repo catalog context e = void $ atomically $
  Catalog.modifyMeta catalog repo $ \meta ->
    return meta {
      metaCompleteness = Broken
        (DatabaseBroken context (Text.pack (show e)))
      }

waitFor :: Async () -> b -> IO b
waitFor async result = do
  r <- timeout 1000000 $ wait async
  case r of
    Nothing -> throwIO $ Retry 1
    Just _ -> return result
