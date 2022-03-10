{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.Args.Flow ( args ) where

import Data.List ( intercalate )

-- | How to run the flow external indexer from regression tests
args :: FilePath -> [String]
args path =
    ["--binary", "flow"
    ,"--json"
    ,"--args"
    ,"glean ${TEST_ROOT} --output-dir ${JSON_BATCH_DIR} --write-root test"
    ,"--root", path
    ,"--derive", derives
    ]
  where
    derives = intercalate ","
        ["flow.StringToFileModule"
        ,"flow.FileXRef"
        ,"flow.FileDeclaration"
        ,"flow.FlowEntityImportUses"
        ,"flow.FlowTypeEntityImportUses"
        ]
