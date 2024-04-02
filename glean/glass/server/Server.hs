{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Server ( main ) where

import Glean.Glass.Env
import qualified Glean.Glass.Main ( mainWith )
import Glean.Glass.SnapshotBackend ( snapshotBackendParser )

main :: IO ()
main = Glean.Glass.Main.mainWith $ setSnapshotBackend <$> snapshotBackendParser
