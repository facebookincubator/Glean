{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module Glean.Glass.Server ( main ) where

import qualified Glean.Glass.Main ( main )

main :: IO ()
main = Glean.Glass.Main.main
