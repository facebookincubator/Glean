module ConfigProviderTest (main) where

import Control.Concurrent
import Data.IORef

import Test.HUnit
import TestRunner

import Glean.Init
import Glean.Impl.TestConfigProvider
import Glean.Util.ConfigProvider

testConfigProvider :: Test
testConfigProvider = TestCase $
  withConfigProvider defaultConfigOptions $ \(cfg :: TestConfigAPI) -> do
    setTestConfig cfg "a" "b"
    result <- get cfg "a" Right
    assertEqual "get" "b" result

    ref <- newIORef []
    done <- newEmptyMVar
    let fn contents = modifyIORef ref (contents:) >> putMVar done ()
    _ <- subscribe cfg "a" fn Right
    takeMVar done
    result <- readIORef ref
    assertEqual "subscribe" ["b"] result

    setTestConfig cfg "a" "c"
    takeMVar done
    result <- readIORef ref
    assertEqual "subscribe" ["c","b"] result


main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "TestConfigProvider" testConfigProvider
  ]
