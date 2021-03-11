module WardenTest (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import System.Mem
import System.Mem.Weak (addFinalizer)
import Test.HUnit

import TestRunner

import Glean.Init
import qualified Glean.Util.Warden as Warden

leakTest :: Test
leakTest = TestCase $ Warden.withWarden $ \warden -> do
  counter <- newTVarIO (0 :: Int)
  forConcurrently_ [1 .. 1000 :: Int] $ \i -> do
    t <- Warden.spawn warden $ do
      atomically $ modifyTVar' counter (+1)
      v <- newTVarIO i
      addFinalizer v $ atomically $ modifyTVar' counter (subtract 1)
      return v
    wait t
  performGC
  atomically $ do
    n <- readTVar counter
    when (n /= 0) retry

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "leakTest" leakTest
  ]
