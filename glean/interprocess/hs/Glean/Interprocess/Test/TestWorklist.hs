-- Copyright 2004-present Facebook. All Rights Reserved.

module Glean.Interprocess.Test.TestWorklist
  ( main
  ) where

import Control.Monad
import Test.HUnit

import TestRunner ( testRunner )

import Glean.Init
import Glean.Interprocess.Worklist


testRanges :: Int -> Test
testRanges n = TestCase $ do
  let initial =
        [ Range lo hi
        | i <- [ 0 .. pred n ]
        , let lo = 10 * i
        , let hi = 10 * succ i ]
  withTemp initial $ \ workfile _ -> do
    forM_ (zip [0..] initial) $ \ (worker, r1) -> do
      r2 <- peek workfile worker
      assertEqual ("initial " <> show (worker, r1, r2)) r1 r2
    forM_ (zip [0..] initial) $ \ (worker, r1) -> do
      (r2, _) <- doNext workfile worker
      assertEqual ("next " <> show (worker, r1, r2)) r1 r2
      let expect = r1{rangeStart = succ (rangeStart r1)}
      r3 <- peek workfile worker
      assertEqual ("peek " <> show (worker, expect, r3)) expect r3

main :: IO ()
main = withUnitTest $ do
  testRunner $ TestList
    [ TestLabel ("worker_count="<>show n) $ testRanges n
    | n <- [1 .. 10] ]
