{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Catalog.Local.Memory
  ( Memory
  , memoryCatalog
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef

import Glean.Database.Catalog.Store
import Glean.Database.Meta (Meta)
import Glean.Types (Repo(..))

newtype Memory = Memory (IORef (HashMap Repo Meta))

memoryCatalog :: IO Memory
memoryCatalog = Memory <$> newIORef mempty

instance Store Memory where
  list (Memory v) = readIORef v

  create (Memory v) repo meta = atomicModifyIORef' v $ \xs ->
    if repo `HashMap.member` xs
      then (xs, False)
      else (HashMap.insert repo meta xs, True)

  delete (Memory v) repo = atomicModifyIORef' v $ \xs ->
    if repo `HashMap.member` xs
      then (HashMap.delete repo xs, True)
      else (xs, False)

  put (Memory v) repo meta = atomicModifyIORef' v $ \xs ->
    if repo `HashMap.member` xs
      then (HashMap.insert repo meta xs, True)
      else (xs, False)

  get (Memory v) repo = HashMap.lookup repo <$> readIORef v
