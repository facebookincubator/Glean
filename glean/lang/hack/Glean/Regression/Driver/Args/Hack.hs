{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.Args.Hack ( args ) where

import Data.List ( intercalate )

-- | How to run the hack (hhvm) external indexer from regression tests
args :: FilePath -> [String]
args path =
    ["--binary", "hh_server"
    ,"--json"
    ,"--args"
    ,"${TEST_ROOT} --write-symbol-info ${JSON_BATCH_DIR}"
    ,"--root", path
    ,"--derive", derives
    ]
  where
    derives = intercalate ","
      [ "hack.DeclarationSource"
      , "hack.NameLowerCase"
      , "hack.AttributeToDefinition"
      , "hack.AttributeHasParameter"
      , "hack.NamespaceMember"
      , "hack.ContainerParent"
      , "hack.ContainerChild"
      , "hack.MethodOverridden"
      , "hack.TargetUses"
      ]
