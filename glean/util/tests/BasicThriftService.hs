-- Copyright (c) Facebook, Inc. and its affiliates.

module BasicThriftService (main) where

import Test.HUnit
import TestRunner

-- Just test that it compiles for now
main :: IO ()
main = testRunner $ TestList []
