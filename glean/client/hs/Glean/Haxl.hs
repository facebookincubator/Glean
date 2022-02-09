{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ConstraintKinds #-}
module Glean.Haxl
  ( runHaxl
  , runHaxlWithWrites
  , Haxl
  , get
  , getRec
  , getKey
  , getKeyRec
  , getOfId
  , getRecOfId
  , getKeyOfId
  , getKeyRecOfId
  , keyOf
  , search
  , search_
  , Query
  , query
  , recursive
  , limit
  , trySyncHaxl
  , HaxlQuery
  , getFirstResult
    -- * re-export
  , Backend.initGlobalState
  ) where

import Control.Exception (SomeException)
import Data.Maybe (listToMaybe)
import Data.Typeable

import qualified Haxl.Core as Haxl
import Haxl.Core (GenHaxl)
import Haxl.Core.Monad ( catchIf )
import Haxl.DataSource.Glean
import Haxl.DataSource.Glean.Backend as Backend
import Util.Control.Exception ( isSyncException )

import Glean.Backend.Remote
import Glean.Query.Thrift
import Glean.Types
import Glean.Typed

type Haxl = GenHaxl Repo

-- | Constraints needed for 'query' or 'search' or 'search_' from
-- "Haxl.DataSource.Glean"
type HaxlQuery p =
  ( Typeable p
  , Show p
  , ThriftQuery p
  )

-- | Initialize for Glean queries
initHaxlEnv :: Backend be => be -> u -> IO (Haxl.Env u w)
initHaxlEnv backend e = do
  (state1,state2) <- initGlobalState backend
  let st = Haxl.stateSet state1 $ Haxl.stateSet state2 Haxl.stateEmpty
  Haxl.initEnv st e

runHaxl :: Backend be => be -> u -> GenHaxl u w a -> IO a
runHaxl backend u h = do
  e <- initHaxlEnv backend u
  Haxl.runHaxl e h

runHaxlWithWrites :: Backend be => be -> u -> GenHaxl u w a -> IO (a, [w])
runHaxlWithWrites backend u h = do
  e <- initHaxlEnv backend u
  Haxl.runHaxlWithWrites e h

-- | if the Fact has a key, return it, otherwise fetch it with 'getKey'
keyOf
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p )
  => p
  -> GenHaxl Repo w (KeyType p)
keyOf f = case getFactKey f of
  Nothing -> getKey f
  Just k -> return k

-- | Catch non-asyncronous exceptions inside Haxl, very good for
-- thrift calls that throw interesting exceptions.
trySyncHaxl :: GenHaxl u w b -> GenHaxl u w (Either SomeException b)
trySyncHaxl act = catchIf isSyncException (Right <$> act) (return . Left)

-- | Search for at most 1 result and return it or Nothing
getFirstResult :: (Typeable a, Show a) => Query a -> GenHaxl Repo w (Maybe a)
getFirstResult = fmap (listToMaybe . fst) . search . limit 1
