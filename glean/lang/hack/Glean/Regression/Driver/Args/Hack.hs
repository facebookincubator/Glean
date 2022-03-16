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
    ,"${TEST_ROOT} --write-symbol-info ${JSON_BATCH_DIR} " <> hhvmConfig
    ,"--root", path
    ,"--derive", derives
    ]
  where
    derives = intercalate ","
      [ "hack.NameLowerCase"
      , "hack.AttributeToDefinition"
      , "hack.AttributeHasParameter"
      , "hack.NamespaceMember"
      , "hack.ContainerParent"
      , "hack.ContainerChild"
      , "hack.MethodOverridden"
      , "hack.TargetUses"
      , "hack.DeclarationSource"
      ]

    -- Required hhvm configuration to match Meta defaults
    hhvmConfig = unwords . map ("--config " <>) $
      [ "symbol_write_include_hhi=false"
      , "symbolindex_search_provider=NoIndex"
      , "use_mini_state=true"
      , "lazy_decl=true"
      , "lazy_parse=true"
      , "lazy_init2=true"
      , "enable_enum_classes=true"
      , "enable_enum_supertyping=true"
      ]
