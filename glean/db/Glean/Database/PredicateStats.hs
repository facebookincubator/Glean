{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.PredicateStats
  ( predicateStats
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Coerce (coerce)

import Glean.Database.Open (withOpenDatabaseStack, withOpenDatabase)
import Glean.RTS.Types (Pid(..))
import Glean.Database.Types (OpenDB(..), Env)
import Glean.Types (Repo, PredicateStats(..), Id)
import Glean.Backend.Remote (StackedDbOpts(..))
import qualified Glean.Database.Storage as Storage

predicateStats
  :: Env
  -> Repo
  -> StackedDbOpts
  -> IO (Map Id PredicateStats)
predicateStats env repo opts = case opts of
  ExcludeBase -> do
    withOpenDatabase env repo $ \OpenDB{..} ->
      Map.fromList . coerce <$> Storage.predicateStats odbHandle

  IncludeBase -> do
    let combineStats x y = PredicateStats
          { predicateStats_count
            = predicateStats_count x + predicateStats_count y
          , predicateStats_size
            = predicateStats_size x + predicateStats_size y
          }
    statsList <- withOpenDatabaseStack env repo $ \OpenDB{..} ->
      Storage.predicateStats odbHandle
    return $ Map.fromListWith combineStats $ coerce $ concat statsList
