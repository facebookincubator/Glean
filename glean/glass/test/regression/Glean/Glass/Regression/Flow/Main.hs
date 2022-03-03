{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
module Glean.Glass.Regression.Flow.Main ( main ) where

import Data.List ( intercalate )
import System.Environment
import qualified Glean.Glass.Regression.Flow as Flow ( main )

main :: IO ()
main = withArgs flowArgs Flow.main
  where
    flowArgs =
        ["--binary", "flow"
        ,"--json"
        ,"--args", "glean ${TEST_ROOT} --output-dir ${JSON_BATCH_DIR} --write-root test"
        ,"--root", "glean/lang/codemarkup/tests/flow/cases/xrefs"
        ,"--derive", flowDerives
        ]
    flowDerives = intercalate ","
        ["flow.StringToFileModule"
        ,"flow.FileXRef"
        ,"flow.FileDeclaration"
        ,"flow.FlowEntityImportUses"
        ,"flow.FlowTypeEntityImportUses"
        ]
