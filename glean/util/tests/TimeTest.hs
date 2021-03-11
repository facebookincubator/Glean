-- | I do not trust leap seconds and other time issues, so I test them
module TimeTest
  ( main
  ) where

import Data.Text
import System.Clock
import Test.HUnit

import TestRunner

import Glean.Init
import Glean.Util.Time

tests :: Test
tests =
  let test1 = -- Good test values from https://www.unixtimestamp.com/index.php
        let e1 = EpochClock (TimeSpec 1500000000 0)
            ok1 = pack "2017-07-14T02:40:00Z"
        in TestCase (assertEqual "1.5Gs" ok1 (showEpochTime e1))
      test2 =
        let e2 = EpochClock (TimeSpec 1500000000 1)
            ok2 = pack "2017-07-14T02:40:00.000000001Z"
        in TestCase (assertEqual "1.5Gs+1ns" ok2 (showEpochTime e2))
  in TestList [ TestLabel "test 1.5Gs" test1
              , TestLabel "test 1.5Gs+1ns" test2 ]

main :: IO ()
main = withUnitTest $ do
  testRunner tests
