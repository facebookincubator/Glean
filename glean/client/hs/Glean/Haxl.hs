{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP, ConstraintKinds #-}
module Glean.Haxl
  ( runHaxl
  , runHaxlWithWrites
  , Haxl
  , haxlRepo
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
  , recursive
  , limit
  , trySyncHaxl
  , getFirstResult
    -- * re-export
  , initGlobalState
  ) where

import Control.Arrow
import Control.Exception (SomeException)
import Data.Maybe (listToMaybe)
import Data.Typeable

import qualified Haxl.Core as Haxl
import Haxl.Core.Monad
import Haxl.DataSource.Glean
import Util.Control.Exception ( isSyncException )

import Glean.Backend.Types
import Glean.Query.Thrift
import Glean.Types
import Glean.Typed

#ifdef GLEAN_FACEBOOK
#define MIN_VERSION_haxl(a,b,c) 1
#endif

#if MIN_VERSION_haxl(2,5,0)
type HaxlWrite w = WriteTree w
flatten :: WriteTree w -> [w]
flatten = flattenWT
#else
type HaxlWrite w = w
flatten :: [w] -> [w]
flatten = id
#endif

type Haxl w = GenHaxl Repo (HaxlWrite w)

-- | Initialize for Glean queries
initHaxlEnv :: Backend b => b -> u -> IO (Haxl.Env u (HaxlWrite w))
initHaxlEnv backend e = do
  (state1,state2) <- initGlobalState backend
  let st = Haxl.stateSet state1 $ Haxl.stateSet state2 Haxl.stateEmpty
  Haxl.initEnv st e

runHaxl :: Backend b => b -> u -> GenHaxl u (HaxlWrite w) a -> IO a
runHaxl backend u h = do
  e <- initHaxlEnv backend u
  Haxl.runHaxl e h

runHaxlWithWrites
  :: Backend b
  => b
  -> u
  -> GenHaxl u (HaxlWrite w) a
  -> IO (a, [w])
runHaxlWithWrites backend u h = do
  e <- initHaxlEnv backend u
  second flatten <$> Haxl.runHaxlWithWrites e h

-- | if the Fact has a key, return it, otherwise fetch it with 'getKey'
keyOf
  :: ( Typeable p, Typeable (KeyType p)
     , Show p, Show (KeyType p)
     , Predicate p, HasRepo u )
  => p
  -> GenHaxl u w (KeyType p)
keyOf f = case getFactKey f of
  Nothing -> getKey f
  Just k -> return k

-- | Catch non-asyncronous exceptions inside Haxl, very good for
-- thrift calls that throw interesting exceptions.
trySyncHaxl :: GenHaxl u w b -> GenHaxl u w (Either SomeException b)
trySyncHaxl act = catchIf isSyncException (Right <$> act) (return . Left)

-- | Search for at most 1 result and return it or Nothing
getFirstResult
  :: (Typeable a, Show a, HasRepo u)
  => Query a -> GenHaxl u w (Maybe a)
getFirstResult = fmap (listToMaybe . fst) . search . limit 1
