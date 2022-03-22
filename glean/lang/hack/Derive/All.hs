{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Derive.All
  (derive)
  where

import Glean (Backend)
import Derive.Types (Config)
import Derive.Env (withEnv)
import Derive.HackDeclarationTarget (deriveHackDeclarationTarget)

-- | Programatically derive predicates after an indexer run
derive :: Backend e => e -> Config -> IO ()
derive backend config =
  withEnv config backend $ \env -> do
    deriveHackDeclarationTarget env
