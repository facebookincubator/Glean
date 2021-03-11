module TestRunner (module TestRunner) where

import Control.Monad
import System.Exit
import Test.HUnit

testRunner :: Test -> IO ()
testRunner t = do
  Counts{..} <- runTestTT t
  when (errors + failures > 0) $ exitWith (ExitFailure 1)
