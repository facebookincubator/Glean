{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE QuasiQuotes #-}
module Angle.SetTest (main) where

import Glean.Angle.Parser
import Glean.Angle.Types
import Glean.Init
import Test.HUnit
import TestRunner
import Util.String.Quasi

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "set" setTest
  ]

setTest :: Test
setTest = TestList
    [ TestLabel "schema parses" $ TestCase $
        case parseSchemaWithVersion (AngleVersion 8)
            [s|
            schema foo {
            type Foo = set maybe nat
            }
            |]
        of
            Left err ->
              assertFailure $ "Parsing schema failed with error:\n" <> err
            Right _ -> return ()
    , TestLabel "query parses" $ TestCase $
          case parseQueryWithVersion  (AngleVersion 8)
            [s| set(1,set(X,all(Y)))
            |]
          of
            Left err -> assertFailure $ "Parsing query failed with error:\n" <> err
            Right _ -> return ()
    ]
