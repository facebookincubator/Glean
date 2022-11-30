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
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import System.Timeout

import Util.Control.Exception
import Util.Defer
import Util.Log.Text

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Open
import Glean.Database.Schema
import qualified Glean.Database.Storage as Storage
import Glean.Database.Types
import Glean.Internal.Types as Thrift
import Glean.RTS.Foreign.Ownership
import Glean.Types as Thrift
import qualified Glean.Util.Warden as Warden
import Glean.Util.Mutex

-- For internal use: actually perform completion for a DB
syncCompletePredicates :: Env -> Repo -> IO ()
syncCompletePredicates env repo =
  withOpenDatabase env repo $ \OpenDB{..} -> do
    own <- Storage.computeOwnership odbHandle
      (schemaInventory odbSchema)
    withWriteLock odbWriting $ Storage.storeOwnership odbHandle own
    maybeOwnership <- readTVarIO odbOwnership
    forM_ maybeOwnership $ \ownership -> do
      stats <- getOwnershipStats ownership
      logInfo $ "ownership propagation complete: " <> showOwnershipStats stats
  where
  withWriteLock Nothing f = f
    -- if there's no write lock, we must be in the finalization
    -- phase and the DB has already been marked read-only. We have
    -- exclusive write access at this point so it's safe to
    -- continue.
  withWriteLock (Just writing) f =
    withMutex (wrLock writing) $ const f


-- | Client API: kick off the completion process for a DB asynchronously
completePredicates :: Env -> Repo -> IO CompletePredicatesResponse
completePredicates env@Env{..} repo = do
  let
    doCompletion = -- we are masked in here
      (`finally` deregister) $ do
      r <- tryAll $ syncCompletePredicates env repo
      case r of
        Left (e :: SomeException) -> do setBroken e; throwIO e
        Right{} -> setComplete

    deregister =
      atomically $ modifyTVar envCompleting $ HashMap.delete repo

    setComplete = void $ atomically $
      Catalog.modifyMeta envCatalog repo $ \meta ->
        return meta { metaAxiomComplete = True }

    -- If a synchronous exception is thrown during completion, this is
    -- not recoverable, so mark the DB as broken. The exception is also
    -- propagated to the caller of completePredicates.
    setBroken e = void $ atomically $
      Catalog.modifyMeta envCatalog repo $ \meta ->
        return meta { metaCompleteness =
          Broken (DatabaseBroken "completePredicates" (Text.pack (show e))) }

    waitFor async = do
      r <- timeout 1000000 $ wait async
      case r of
        Nothing -> throwIO $ Retry 1
        Just _ -> return CompletePredicatesResponse{}

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
        | metaAxiomComplete meta -> do
          later $ cancel async -- already done
          return (return CompletePredicatesResponse{})
        | Broken b <- metaCompleteness meta -> do
          later $ cancel async
          now $ throwSTM $ Exception $ databaseBroken_reason b
        | envReadOnly -> do
          later $ cancel async
          now $ throwSTM $ Exception "DB is read-only"
        | otherwise -> do
          completing <- now $ readTVar envCompleting
          case HashMap.lookup repo completing of
            Just existingAsync -> do  -- in progress
              later $ cancel async
              return $ waitFor existingAsync
            Nothing -> now $ do  -- start async completion
              void $ tryPutTMVar tmvar ()
              modifyTVar envCompleting (HashMap.insert repo async)
              return $ waitFor async
