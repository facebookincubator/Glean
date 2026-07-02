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
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word
import System.Timeout

import Util.Control.Exception
import Util.Defer
import Util.Log.Text
import Util.Logger
import Util.STM

import qualified Glean.Database.Catalog as Catalog
import qualified Glean.Database.Data as Data
import Glean.Database.Open
import Glean.Database.Repo (inRepo)
import Glean.Database.Schema
import Glean.Database.Writes (enqueueCheckpoint)
import Glean.Database.Meta (getACLMode, showACLMode, isACLEnabled)
import Glean.Database.AclKnobs (aclCalculateEnabled)
import Glean.Database.Schema.Types
  ( lookupPredicateSourceRef
  , SchemaSelector(..)
  )
import qualified Glean.Database.Storage as Storage
import Glean.Database.Types
import Glean.Database.ACLOwnership (augmentOwnershipWithACL)
import Glean.Database.ACLConfig (ACL(ACL), Path(Path), getAllGroupIds)
import Glean.Internal.Types as Thrift
import Glean.Logger
import Glean.Repo.Text (repoToText)
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
  -- Wait for all pending async writes to finish before starting the completion
  waitForWrites env repo
  maybeBase <- repoParent env repo
  let withBase repo f =
        readDatabase env repo $ \_ lookup -> f (Just lookup)
  maybe ($ Nothing) withBase maybeBase $ \base -> do
  withOpenDatabase env repo $ \OpenDB{..} -> do
    meta <- atomically $ Catalog.readMeta (envCatalog env) repo
    let dbAclEnabled = isACLEnabled (metaProperties meta)
    calculateEnabled <- aclCalculateEnabled
    let aclProcessingEnabled = dbAclEnabled && calculateEnabled

    mAclConfig <-
      if aclProcessingEnabled
        then Data.retrievePathACLConfig odbHandle
        else return Nothing

    let typedPathConfig config = HashMap.fromList
          [ (Path p, map ACL acls) | (p, acls) <- HashMap.toList config ]
        aclGroupUnitNames config =
          [ "acl:" <> Text.encodeUtf8 g
          | ACL g <- getAllGroupIds (typedPathConfig config) ]

    -- Register ACL group names as ACL units *before* computing
    -- ownership. Doing this once here, after all batches are written
    -- (waitForWrites above), rather than per batch, guarantees the ACL
    -- units are allocated UnitIds above every regular ownership unit.
    -- The returned id is the firstACLID boundary: every UnitId below it
    -- is a regular ownership unit, every UnitId at or above it is an ACL
    -- group unit. The units own no facts; the query server resolves
    -- groups via getUnitId("acl:<name>").
    --
    -- For an ACL-enabled DB with no ACL groups (an empty shard, or an
    -- all-public config) we register zero units. registerACLUnits
    -- returns the boundary (= next free UnitId) without allocating, so we
    -- still obtain a firstACLID. Storing it unconditionally is what makes
    -- ACL filtering engage at query time: 'buildLayerACLSlice' skips
    -- filtering entirely (fail-open) when firstACLID is absent.
    mFirstACLID <-
      if aclProcessingEnabled
        then do
          let names = maybe [] aclGroupUnitNames mAclConfig
          UnitId firstId <- withWriteLock odbWriting $ \lock ->
            Storage.registerACLUnits odbHandle lock names
          logInfo $ "ACL augmentation: registered "
            <> Text.pack (show (length names)) <> " ACL group units"
          return (Just (UsetId firstId))
        else return Nothing

    own <- Storage.computeOwnership odbHandle base
      (schemaInventory odbSchema)

    -- ACL augmentation: AND-join ACL constraints into ownership when there
    -- is a non-empty config; an empty/absent config has no constraints to
    -- apply. Either way, record the firstACLID boundary and the (possibly
    -- empty) group mapping for an ACL-enabled DB, so firstACLID is never
    -- absent at query time (which would silently disable ACL filtering).
    when aclProcessingEnabled $ do
      groupMapping <- case mAclConfig of
        Just config | not (HashMap.null config) -> do
          logInfo $ "ACL augmentation: processing "
            <> Text.pack (show (HashMap.size config))
            <> " ACL config entries"
          augmentOwnershipWithACL odbHandle own (typedPathConfig config)
        _ -> do
          logInfo "ACL augmentation: no ACL groups; storing empty boundary"
          return HashMap.empty
      forM_ mFirstACLID $ \firstACLID -> do
        Data.storeFirstACLID odbHandle firstACLID
        Data.storeACLGroupMapping odbHandle
          (buildGroupMappingJson
            (HashMap.fromList
              [ (t, w)
              | (ACL t, UnitId w) <- HashMap.toList groupMapping ]))
        logInfo $ "ACL augmentation complete: firstACLID="
          <> Text.pack (show firstACLID)
          <> ", groups="
          <> Text.pack (show (HashMap.size groupMapping))
    when (dbAclEnabled && not calculateEnabled) $
      logInfo "ACL augmentation skipped: calculate_acls knob is off"

    withWriteLock odbWriting $ \lock ->
      Storage.storeOwnership odbHandle lock own
    maybeOwnership <- readTVarIO odbOwnership
    forM_ maybeOwnership $ \ownership -> do
      stats <- getOwnershipStats ownership
      logInfo $ "ownership propagation complete: " <> showOwnershipStats stats

completeAxiomPredicates :: Env -> Repo -> IO CompletePredicatesResponse
completeAxiomPredicates env@Env{..} repo = do
  -- Log raw glean.acl property and ACL mode for this complete operation
  meta <- atomically $ Catalog.readMeta envCatalog repo
  let aclMode = getACLMode (metaProperties meta)
      rawAclProp = HashMap.lookup "glean.acl" (metaProperties meta)
  vlog 1 $ Text.pack $ inRepo repo $
    "[glean complete] raw glean.acl property: " ++
    show rawAclProp
  logInfo $ Text.pack $ inRepo repo $
    "[glean complete] " ++ showACLMode aclMode
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

-- | Wait for all pending async writes to complete for a repo.
-- This enqueues a checkpoint on the write queue and blocks until
-- all previous writes have finished.
-- If the db is readonly, it's guaranteed there is no pending writes
-- and no writes will be enqueued.
waitForWrites :: Env -> Repo -> IO ()
waitForWrites env repo = do
  shouldWait <- withOpenDatabase env repo $ \OpenDB{..} -> case odbWriting of
    Nothing -> return False
    Just writing -> do
      count <- readTVarIO $ writeQueueCount $ wrQueue writing
      return (count > 0)
  when shouldWait $ do
    let repoText = repoToText repo
    logInfo $ "Waiting for pending writes to complete for " <> repoText
    done <- newEmptyMVar
    enqueueCheckpoint env repo (putMVar done ())
    takeMVar done
    logInfo $ "All pending writes completed for " <> repoText

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
          return (return emptyCompletePredicatesResponse)
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
              return $ waitFor existingAsync emptyCompletePredicatesResponse
            Nothing -> now $ do  -- start async completion
              void $ tryPutTMVar tmvar ()
              storeComputation async
              return $ waitFor async emptyCompletePredicatesResponse
  where
    emptyCompletePredicatesResponse = CompletePredicatesResponse
      { completePredicatesResponse_auth_status = Nothing
      , completePredicatesResponse_auth_message = Nothing
      }

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

-- | Build a JSON ByteString from the group→index mapping.
-- Format: {"group_name": index, ...}
--
-- Uses 'Aeson.encode' so that group names containing characters which are
-- significant in JSON (@\"@, @\\@, control characters) are correctly escaped.
buildGroupMappingJson :: HashMap.HashMap Text.Text Word32 -> BS.ByteString
buildGroupMappingJson = BSL.toStrict . Aeson.encode
