{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Catalog.Test
  ( MockStore(..)
  , memStore
  , withMemCatalog
  , checkConsistency
  ) where

import Control.Concurrent.STM
import Control.Exception hiding (assert)
import Control.Monad
import Data.Default
import Data.IORef
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List ((\\))
import qualified Data.Text as Text
import Test.HUnit

import Glean.Database.Catalog (Catalog)
import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Catalog.Filter
import qualified Glean.Database.Catalog.Store as Store
import Glean.Database.Meta (Meta)
import Glean.Test.HUnit
import Glean.Test.Mock
import Glean.Types (Repo(..), UnknownDatabase(..))

data MockStore = MockStore
  { storeList :: Mock (IO (HashMap Repo Meta))
  , storeCreate :: Mock (Repo -> Meta -> IO Bool)
  , storeDelete :: Mock (Repo -> IO Bool)
  , storePut :: Mock (Repo -> Meta -> IO Bool)
  , storeGet :: Mock (Repo -> IO (Maybe Meta))
  }

instance Store.Store MockStore where
  list = call . storeList
  create = call . storeCreate
  delete = call . storeDelete
  put = call . storePut
  get = call . storeGet

memStore :: IO MockStore
memStore = do
  var <- newIORef HashMap.empty
  storeList <- implement "Store.list" $ readIORef var
  storeCreate <- implement "Store.create" $ \repo meta ->
    atomicModifyIORef' var $ \xs ->
    if repo `HashMap.member` xs
      then (xs, False)
      else (HashMap.insert repo meta xs, True)
  storeDelete <- implement "Store.delete" $ \repo ->
    atomicModifyIORef' var $ \xs ->
    if repo `HashMap.member` xs
      then (HashMap.delete repo xs, True)
      else (xs, False)
  storePut <- implement "Store.put" $ \repo meta ->
    atomicModifyIORef' var $ \xs ->
    if repo `HashMap.member` xs
      then (HashMap.insert repo meta xs, True)
      else (xs, False)
  storeGet <- implement "Store.get" $ \repo ->
    HashMap.lookup repo <$> readIORef var
  return MockStore{..}

withMemCatalog :: (MockStore -> Catalog -> IO a) -> IO a
withMemCatalog f = do
  store <- memStore
  bracket (Catalog.open store) Catalog.close $ f store


checkConsistency :: Catalog -> IO ()
checkConsistency cat = do
  xs <- map itemRepo <$> atomically (Catalog.list cat [Local] everythingF)
  mapM_ live xs
  mapM_ not_live
    $ take 4
    $ map (Repo "test" . Text.pack . show) [1::Int ..] \\ xs
  where
    live repo = do
      assert $ Catalog.exists cat [Local] repo
      assert $ void $ Catalog.readMeta cat repo
      assert $ Catalog.writeMeta cat repo def
      assert $ void $ Catalog.modifyMeta cat repo return

    not_live repo = do
      assert $ not <$> Catalog.exists cat [Local] repo
      assertThrows "" (UnknownDatabase repo)
        $ atomically $ Catalog.readMeta cat repo
      assertThrows "" (UnknownDatabase repo)
        $ atomically $ Catalog.writeMeta cat repo def
      assertThrows "" (UnknownDatabase repo)
        $ atomically $ Catalog.modifyMeta cat repo return
