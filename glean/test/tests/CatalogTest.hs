{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module CatalogTest (main) where

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception hiding (assert)
import Control.Monad
import Data.Default
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List
import Data.Proxy
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Test.HUnit
import Test.QuickCheck
import Test.QuickCheck.IO ()

import TestRunner
import Util.Testing

import Glean.Database.Catalog (Catalog)
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import qualified Glean.Database.Catalog.Store as Store
import Glean.Database.Catalog.Test
import Glean.Init
import Glean.Test.HUnit
import Glean.Repo.Text
import Glean.Test.Mock
import Glean.Internal.Types
import Glean.Types hiding (Exception)

repos :: [Repo]
repos = [Repo "foo" (Text.pack $ show n) | n <- [1 .. 12 :: Int]]

data CreateDelete = Create | Delete
  deriving(Eq,Ord,Enum,Bounded,Read,Show)

instance Arbitrary CreateDelete where
  arbitrary = elements [Create,Delete]

newtype ARepo = ARepo Repo
  deriving(Eq)

instance Show ARepo where
  show (ARepo repo) = showRepo repo

instance Arbitrary ARepo where
  arbitrary = ARepo <$> elements repos

data CreateDeleteAction = CreateDeleteAction
  { cdaWhat :: CreateDelete
  , cdaRepo :: Repo
  , cdaExists :: Bool
  , cdaAfter :: HashSet Repo
  }

populate :: [(CreateDelete, ARepo)] -> [CreateDeleteAction]
populate = snd . mapAccumL mk mempty
  where
    mk before (what, ARepo repo) =
      let after = (if what == Create then HashSet.insert else HashSet.delete)
            repo
            before
      in
      ( after
      , CreateDeleteAction
          { cdaWhat = what
          , cdaRepo = repo
          , cdaExists = repo `HashSet.member` before
          , cdaAfter = after
          } )

createDeleteTest :: Test
createDeleteTest = TestCase $ assertProperty "" $ test . populate
  where
    test trace = withMemCatalog $ \_ cat ->
      forM_ trace $ \CreateDeleteAction{..} -> do
        exec cdaWhat cdaExists cat cdaRepo
        xs <- atomically $ Catalog.list cat [Local] everythingF
        assertEqual "" cdaAfter $ HashSet.fromList (map itemRepo xs)
        checkConsistency cat

    exec Create False = createNew
    exec Create True = createExisting
    exec Delete False = deleteUnknown
    exec Delete True = deleteExisting

data TestError = TestError
  deriving(Eq, Show)

instance Exception TestError

createFailureTest :: Test
createFailureTest = TestCase $ withMemCatalog $ \store cat -> do
  prepare (storeCreate store) [ doBefore $ throwIO TestError ]
  called <- newTVarIO False
  assertThrows "" TestError $
    Catalog.create cat repo1 def $ writeTVar called True
  assert $ not <$> readTVarIO called
  assert $ not <$> Catalog.exists cat [Local] repo1
  checkConsistency cat
  where
    repo1 = head repos

nextMeta :: Meta -> Meta
nextMeta !meta = meta
  { metaProperties = HashMap.insert
      "counter"
      (Text.pack $ show $ metaCounter meta + 1)
      (metaProperties meta)
  }

metaCounter :: Meta -> Int
metaCounter =
  maybe 0 (read . Text.unpack) . HashMap.lookup "counter" . metaProperties

writeMetaTest :: Test
writeMetaTest = TestCase $ withMemCatalog $ \store cat -> do
  Catalog.create cat repo1 def $ return ()

  let writes = [5,1,64,1024]

  val <- newTVarIO 0
  max <- newTVarIO 0

  augment (storePut store) $ \impl repo meta -> do
    let n = metaCounter meta
    m <- readTVarIO max
    assert $ n <= m
    b <- impl repo meta
    atomically $ writeTVar val n
    return b

  forM_ writes $ \n -> when (n /= 0) $ do
    k <- atomically $ metaCounter <$> Catalog.readMeta cat repo1
    atomically $ writeTVar max $ k+n
    replicateM_ n $ void $ atomically $
        metaCounter <$> Catalog.modifyMeta cat repo1 (return . nextMeta)
    atomically $ do
      m <- readTVar val
      when (m /= k+n) retry
  where
    repo1 = head repos

stackedDbsTest :: Test
stackedDbsTest = TestCase $ withMemCatalog $ \_ cat -> do
  Catalog.create cat repoA metaA $ return ()
  Catalog.create cat repoC metaC $ return ()

  assertEqual "" DatabaseStatus_Complete =<< status cat repoA
  assertThrows "" TestError $ status cat repoB
  assertEqual "" DatabaseStatus_Missing =<< status cat repoC

  Catalog.create cat repoB metaB $ return ()

  assertEqual "" DatabaseStatus_Complete =<< status cat repoA
  assertEqual "" DatabaseStatus_Complete =<< status cat repoB
  assertEqual "" DatabaseStatus_Complete =<< status cat repoC

  Catalog.delete cat repoB

  assertEqual "" DatabaseStatus_Complete =<< status cat repoA
  assertThrows "" TestError $ status cat repoB
  assertEqual "" DatabaseStatus_Missing =<< status cat repoC

  atomically $ Catalog.startRestoring cat repoB metaB

  assertEqual "" DatabaseStatus_Complete =<< status cat repoA
  assertEqual "" DatabaseStatus_Restoring =<< status cat repoB
  assertEqual "" DatabaseStatus_Restoring =<< status cat repoC

  Catalog.finishRestoring cat repoB

  assertEqual "" DatabaseStatus_Complete =<< status cat repoA
  assertEqual "" DatabaseStatus_Complete =<< status cat repoB
  assertEqual "" DatabaseStatus_Complete =<< status cat repoC

  where
    -- Dependencies: C -> B -> A
    repoA = Repo "base" "repoA"
    repoB = Repo "stacked" "repoB"
    repoC = Repo "stacked2" "repoC"
    metaA = meta
    metaB = meta{metaDependencies=Just $ Dependencies_stacked repoA}
    metaC = meta{metaDependencies=Just $ Dependencies_stacked repoB}
    meta = def
      {metaCompleteness=Complete $ DatabaseComplete (PosixEpochTime 0) Nothing}

    status cat repo = do
      result <- atomically $ Catalog.getLocalDatabase cat repo
      case result of
        Just GetDatabaseResult{getDatabaseResult_database=db} ->
          return (database_status db)
        Nothing -> throwIO TestError

writeWhileCommitting :: Test
writeWhileCommitting = TestCase $ withMemCatalog $ \store cat -> do
  Catalog.create cat repo1 def $ return ()

  start1 <- newEmptyMVar
  finish1 <- newEmptyMVar
  finish2 <- newEmptyMVar
  prepare (storePut store)
    [ doBefore (putMVar start1 ()) . doFinally (takeMVar finish1)
    , doFinally (putMVar finish2 ()) ]
  void $ atomically $ Catalog.modifyMeta cat repo1 (return . nextMeta)
  takeMVar start1
  void $ atomically $ Catalog.modifyMeta cat repo1 (return . nextMeta)
  putMVar finish1 ()
  takeMVar finish2
  Just m <- Store.get store repo1
  assertEqual "" 2 $ metaCounter m
  where
    repo1 = head repos

createNew :: HasCallStack => Catalog -> Repo -> IO ()
createNew cat repo = do
  called <- newTVarIO False
  Catalog.create cat repo def $ writeTVar called True
  assert $ readTVarIO called

createExisting :: HasCallStack => Catalog -> Repo -> IO ()
createExisting cat repo = do
  called <- newTVarIO False
  assertThrows "" (Catalog.EntryAlreadyExists repo)
    $ Catalog.create cat repo def $ writeTVar called True
  assert $ not <$> readTVarIO called

deleteExisting :: HasCallStack => Catalog -> Repo -> IO ()
deleteExisting cat repo = Catalog.delete cat repo

deleteUnknown :: HasCallStack => Catalog -> Repo -> IO ()
deleteUnknown cat repo = assertThrowsType "" (Proxy @ UnknownDatabase)
  $ Catalog.delete cat repo

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "createDelete" createDeleteTest
  , TestLabel "createFailureTest" createFailureTest
  , TestLabel "writeMeta" writeMetaTest
  , TestLabel "stackedDbsTest" stackedDbsTest
  , TestLabel "writeWhileCommitting" writeWhileCommitting
  ]
