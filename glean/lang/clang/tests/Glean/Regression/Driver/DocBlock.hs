{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Regression.Driver.DocBlock (main) where

import Control.Monad

import qualified Glean.Clang.Test as Clang
import qualified Glean.DocBlock.Test as DocBlock (driver)
import Glean.Init (withUnitTestOptions)
import Glean.Regression.Config
import Glean.Regression.Test
import Glean.Derive (derivePredicate)
import Glean.Write (parseRef)

combined :: Clang.Options -> Driver
combined opts = (Clang.driver opts)
  { driverGenerator = \ test backend repo -> do
      driverGenerator (Clang.driver opts) test backend repo
      driverGenerator DocBlock.driver test backend repo
      forM_ passes $ \predicate ->
        derivePredicate backend repo Nothing Nothing predicate Nothing
  }
  where
  passes = map parseRef
    [ "docmarkup.EntityByDocAttrKey"
    ]

main :: IO ()
main =
  withUnitTestOptions (optionsWith Clang.extOptions) $ \ (mkcfg, ext) -> do
    cfg <- mkcfg
    testAll cfg (combined ext)
