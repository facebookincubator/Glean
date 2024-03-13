{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Indexer.HackWithDeriver (indexer) where

import Derive.Env (withEnv)
import Derive.HackDeclarationTarget (deriveHackDeclarationTarget)
import qualified Derive.Types as DT
import Glean.Derive
import Glean.Indexer
import qualified Glean.Indexer.Hack as Hack
import Glean.Write

indexer :: Indexer Hack.Hack
indexer =
  Hack.indexer `indexerThen` \_opts env repo _params -> do
    withEnv (DT.defaultConfig repo) env
      deriveHackDeclarationTarget
    derivePredicate env repo Nothing Nothing
      (parseRef "hack.DeclarationSource") Nothing
