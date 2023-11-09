{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module LifecycleTest (main) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception hiding (assert)
import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Maybe
import Data.Proxy
import GHC.Stack (HasCallStack)
import Test.HUnit
import Test.QuickCheck.IO ()

import TestRunner

import Glean (fillDatabase, enqueueJsonBatch)
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import Glean.Database.Catalog.Test
import Glean.Database.Config
import Glean.Database.Close
import Glean.Database.Create
import Glean.Database.Open
import Glean.Database.Delete
import Glean.Database.Storage.Memory (newStorage)
import Glean.Database.Test
import Glean.Database.Types
import Glean.Init
import Glean.Internal.Types
import Glean.Test.HUnit
import Glean.Test.Mock
import Glean.Types hiding (Exception)

data TEnv = TEnv
  { tEnv :: Env
  , tStore :: MockStore
  }

withTEnv :: (TEnv -> IO a) -> IO a
withTEnv f = do
  catalog <- memStore
  storage <- newStorage
  withTestEnv
    [\cfg -> cfg
        { cfgDataStore = DataStore
            { withDataStore = \_ f -> f catalog storage
            , dataStoreTag = "test"
            }
        }]
    $ \env -> do
      x <- f $ TEnv env catalog
      checkActive env False
      return x

data TestError = TestError
  deriving(Eq,Show)

instance Exception TestError

data InconsistentDBError = InconsistentDBError Repo String
  deriving(Show)

instance Exception InconsistentDBError

mkDB :: HasCallStack => Env -> Repo -> IO ()
mkDB env repo =
  fillDatabase env repo "" Nothing
    (fail "database already exists") (return ())

checkActive :: HasCallStack => Env -> Bool -> IO ()
checkActive Env{..} allow_deleting = do
  checkConsistency envCatalog
  atomically $ do
    active <- readTVar envActive
    forM_ (HashMap.toList active) $ \(repo, DB{..}) -> do
      ex <- Catalog.exists envCatalog [Local] repo
      when (not ex) $ throwSTM $ InconsistentDBError repo
        "in envActive but not in Catalog"
      users <- readTVar dbUsers
      when (users == 0) $ do
        state <- readTVar dbState
        case state of
          Open{} -> return ()
          Closed -> do
            meta <- Catalog.readMeta envCatalog repo
            case metaCompleteness meta of
              Incomplete{} -> return ()
              _ -> throwSTM $ InconsistentDBError repo
                "in envActive but not active"
          Opening -> throwSTM $ InconsistentDBError repo "no users but Opening"
          Closing -> throwSTM $ InconsistentDBError repo "no users but Closing"
    deleting <- readTVar envDeleting
    if allow_deleting
      then case HashMap.keys $ HashMap.intersection active deleting of
        [] -> return ()
        repo : _ -> throwSTM $ InconsistentDBError repo
          "in envActive and envDeleting"
      else case HashMap.keys deleting of
        [] -> return ()
        repo : _ -> throwSTM $ InconsistentDBError repo
          "in envDeleting"

kickOff :: HasCallStack => Env -> Repo -> IO Bool
kickOff env repo = do
  KickOffResponse ex <- kickOffDatabase env def
    { kickOff_repo = repo
    , kickOff_fill = Just $ KickOffFill_writeHandle ""
    }
  return ex

repo1 :: Repo
repo1 = Repo "foo" "1"

repo2 :: Repo
repo2 = Repo "foo" "2"

kickOffTwice :: Test
kickOffTwice = TestCase $ withTEnv $ \TEnv{..} -> do
  assert $ not <$> kickOff tEnv repo1
  assert $ isJust <$> lookupActiveDatabase tEnv repo1
  assert $ kickOff tEnv repo1
  assert $ isJust <$> lookupActiveDatabase tEnv repo1

kickOffFail :: Test
kickOffFail = TestCase $ withTEnv $ \TEnv{..} -> do
  prepare (storeCreate tStore) [ doBefore $ throwIO TestError ]
  assertThrows "" TestError $ kickOff tEnv repo1
  assert $ isNothing <$> lookupActiveDatabase tEnv repo1

kickOffRace :: Test
kickOffRace = TestCase $ withTEnv $ \TEnv{..} -> do
  v1 <- newEmptyMVar
  v2 <- newEmptyMVar
  prepare (storeCreate tStore) [ doBefore $ do putMVar v1 () ; takeMVar v2 ]
  withAsync (kickOff tEnv repo1) $ \k -> do
    takeMVar v1
    assert $ kickOff tEnv repo1
    assert $ isNothing <$> lookupActiveDatabase tEnv repo1
    putMVar v2 ()
    assert $ not <$> wait k
  assert $ isJust <$> lookupActiveDatabase tEnv repo1

kickOffConcurrently :: Test
kickOffConcurrently = TestCase $ withTEnv $ \TEnv{..} -> do
  v1 <- newEmptyMVar
  v2 <- newEmptyMVar
  prepare (storeCreate tStore) [ doBefore $ do putMVar v1 () ; takeMVar v2 ]
  withAsync (kickOff tEnv repo1) $ \k -> do
    takeMVar v1
    assert $ not <$> kickOff tEnv repo2
    assert $ isNothing <$> lookupActiveDatabase tEnv repo1
    assert $ isJust <$> lookupActiveDatabase tEnv repo2
    putMVar v2 ()
    assert $ not <$> wait k
  assert $ isJust <$> lookupActiveDatabase tEnv repo1

closeIdle :: Test
closeIdle = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  withOpenDatabase tEnv repo1 $ \_ -> return ()
  assert $ not <$> isDatabaseClosed tEnv repo1
  -- make sure we release the use count correctly
  assertThrows "" TestError $
    withOpenDatabase tEnv repo1 (\_ -> throwIO TestError)

  closeIdleDatabase tEnv repo1 0
  assert $ isDatabaseClosed tEnv repo1

closeUsed :: Test
closeUsed = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  withOpenDatabase tEnv repo1 $ \_ -> return ()
  assert $ not <$> isDatabaseClosed tEnv repo1

  withOpenDatabase tEnv repo1 $ \_ -> closeIdleDatabase tEnv repo1 0
  assert $ not <$> isDatabaseClosed tEnv repo1

deleteOpen :: Test
deleteOpen = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  withOpenDatabase tEnv repo1 $ \_ -> return ()
  assert $ not <$> isDatabaseClosed tEnv repo1

  deleteDatabase tEnv repo1
  atomically $ do
    ex <- Catalog.exists (envCatalog tEnv) [Local] repo1
    when ex retry
  assertThrowsType "" (Proxy @ UnknownDatabase) $
    withOpenDatabase tEnv repo1 $ \_ -> return ()

deleteWhileUsing :: Test
deleteWhileUsing = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  del <- withOpenDatabase tEnv repo1 $ \_ -> do
    del <- asyncDeleteDatabase tEnv repo1
    atomically $ do
      deleting <- readTVar (envDeleting tEnv)
      when (not $ repo1 `HashMap.member` deleting) retry
    checkActive tEnv True
    return del
  wait del

kickOffWhileDeleting :: Test
kickOffWhileDeleting = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  withOpenDatabase tEnv repo1 $ \_ -> return ()
  assert $ not <$> isDatabaseClosed tEnv repo1

  v1 <- newEmptyMVar
  v2 <- newEmptyMVar

  prepare (storeDelete tStore) [ doBefore $ do putMVar v1 () ; takeMVar v2 ]
  concurrently_ (deleteDatabase tEnv repo1) $ do
    takeMVar v1
    True <- kickOff tEnv repo1
    putMVar v2 ()

useWhileDeleting :: Test
useWhileDeleting = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  withOpenDatabase tEnv repo1 $ \_ -> return ()
  assert $ not <$> isDatabaseClosed tEnv repo1

  v1 <- newEmptyMVar
  v2 <- newEmptyMVar

  prepare (storeDelete tStore) [ doBefore $ do putMVar v1 () ; takeMVar v2 ]
  concurrently_ (deleteDatabase tEnv repo1) $ do
    takeMVar v1
    assertThrowsType "" (Proxy @ UnknownDatabase) $
      withOpenDatabase tEnv repo1 $ \_ -> return ()
    putMVar v2 ()

writeAfterFinished :: Test
writeAfterFinished = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  r <- try $ enqueueJsonBatch tEnv repo1 (def { sendJsonBatch_batches = [] })
  assertBool "writeAfterFinished" $ case r of
    Left err -> "read-only" `isInfixOf` show (err :: SomeException)
    _ -> False

stacked :: Repo -> Dependencies
stacked (Repo name hash) = Dependencies_stacked $
  Stacked name hash Nothing

kickOffStacked :: Test
kickOffStacked = TestCase $ withTEnv $ \TEnv{..} -> do
  mkDB tEnv repo1
  waitUntilComplete tEnv repo1
  KickOffResponse ex <- kickOffDatabase tEnv def
    { kickOff_repo = repo2
    , kickOff_fill = Just $ KickOffFill_writeHandle ""
      , kickOff_dependencies = Just $ stacked repo1
    }
  assert $ not ex
  withOpenDatabase tEnv repo2 $ \_ -> return ()

kickOffStackedIncomplete :: Test
kickOffStackedIncomplete = TestCase $ withTEnv $ \TEnv{..} -> do
  assert $ not <$> kickOff tEnv repo1
  assertThrowsType "" (Proxy :: Proxy InvalidDependency)
    $ kickOffDatabase tEnv def
      { kickOff_repo = repo2
      , kickOff_fill = Just $ KickOffFill_writeHandle ""
      , kickOff_dependencies = Just $ stacked repo1
      }

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "kickOffTwice" kickOffTwice
  , TestLabel "kickOffFail" kickOffFail
  , TestLabel "kickOffRace" kickOffRace
  , TestLabel "kickOffConcurrently" kickOffConcurrently
  , TestLabel "closeIdle" closeIdle
  , TestLabel "closeUsed" closeUsed
  , TestLabel "deleteOpen" deleteOpen
  , TestLabel "deleteWhileUsing" deleteWhileUsing
  , TestLabel "kickOffWhileDeleting" kickOffWhileDeleting
  , TestLabel "useWhileDeleting" useWhileDeleting
  , TestLabel "writeAfterFinished" writeAfterFinished
  , TestLabel "kickOffStacked" kickOffStacked
  , TestLabel "kickOffStackedIncomplete" kickOffStackedIncomplete
  ]
