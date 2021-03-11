module TestShard (main) where

import Test.HUnit

import TestRunner

import Glean.Backend.Remote
import Glean.Types

main :: IO ()
main = testRunner $ TestList
  [ TestLabel "testShard" $ TestCase $
      assertEqual "shard"
        (dbShard (Repo "test" "1234"))
        "6825422262439797754"
  ]
