module CacherTest
  ( main
  ) where

import Glean.Init
import Glean.Util.Cacher

import Control.Exception
import Data.IORef
import Data.Word
import Test.HUnit
import TestRunner

ccOf :: Word64 -> Word64 -> CacherConf
ccOf high low | high >= low = CacherConf{ cc_item_high_water_mark = high
                                        , cc_item_low_water_mark = low }
              | otherwise = undefined


letters :: [String]
letters = [ q:"" | q <- ['a'..'z'] ]

args :: [String]
args = letters ++ reverse letters ++ letters ++ reverse letters

-- | Repeat first 10 entries, to check if they are cached
argsPre :: [String]
argsPre = take 10 args ++ args

-- | Our test operation is pretty boring, but not identity
expectResult :: [String]
expectResult = map ("f " ++)  argsPre

cacherRunsOnce :: Test
cacherRunsOnce =
  let tc = TestCase $ do
        c <- newCacher (ccOf 26 26)
        ref <- newIORef ([] :: [String]) -- hold 'f' args in reverse order
        let f s = modifyIORef ref (s:) >> return ("f " ++ s)
            expectRef = reverse letters
        result <- mapM (withCacher c f)  argsPre
        assertEqual "result" expectResult result
        r <- readIORef ref
        assertEqual "ref" expectRef r
  in TestLabel "cacherRunsOnce" tc

cacherDropsToTen :: Test
cacherDropsToTen =
  let tc = TestCase $ do
        c <- newCacher (ccOf 25 10)
        ref <- newIORef ([] :: [String]) -- hold 'f' args in reverse order
        let f s = modifyIORef ref (s:) >> return ("f " ++ s)
            expectRef = reverse $ letters -- all 26 are fresh and run
                        -- at 26th letter, cache is dropped to last 10 letters
              ++ drop 10 (reverse letters)   -- 10 recent ones cached, 16 run
              ++ drop 10 letters             -- 10 recent ones cached, 16 run
              ++ drop 10 (reverse letters)   -- 10 recent ones cached, 16 run
        result <- mapM (withCacher c f)  argsPre
        assertEqual "result" expectResult result
        r <- readIORef ref
        assertEqual "ref" expectRef r
  in TestLabel "cacherDropsToTen" tc

cacherDropsToZero :: Test
cacherDropsToZero =
  let tc = TestCase $ do
        c <- newCacher (ccOf 25 0)
        ref <- newIORef ([] :: [String]) -- hold 'f' args in reverse order
        let f s = modifyIORef ref (s:) >> return ("f " ++ s)
            -- On each section, at 26th letter, cache is dropped to zero letters
            expectRef = reverse args
        result <- mapM (withCacher c f)  argsPre
        assertEqual "result" expectResult result
        r <- readIORef ref
        assertEqual "ref" expectRef r
  in TestLabel "cacherDropsToZero" tc

cacherTen :: Test
cacherTen =
  let tc = TestCase $ do
        c <- newCacher (ccOf 10 10)
        ref <- newIORef ([] :: [String]) -- hold 'f' args in reverse order
        let f s = modifyIORef ref (s:) >> return ("f " ++ s)
            expectRef = reverse $ letters
              ++ drop 10 (reverse letters)
              ++ drop 10 letters
              ++ drop 10 (reverse letters)
        result <- mapM (withCacher c f)  argsPre
        assertEqual "result" expectResult result
        r <- readIORef ref
        assertEqual "ref" expectRef r
  in TestLabel "cacherTen" tc

cacherZero :: Test
cacherZero =
  let tc = TestCase $ do
        c <- newCacher (ccOf 0 0)
        ref <- newIORef ([] :: [String]) -- hold 'f' args in reverse order
        let f s = modifyIORef ref (s:) >> return ("f " ++ s)
            expectRef = reverse  argsPre
        result <- mapM (withCacher c f)  argsPre
        assertEqual "result" expectResult result
        r <- readIORef ref
        assertEqual "ref" expectRef r
  in TestLabel "cacherZero" tc

-- | This ran in 200 ms, so the withCacher is taking less than 200 ns each
-- time, with a singleton map.
cacherPerfTest :: Test
cacherPerfTest =
  let tc = TestCase $ do
        c <- newCacher (ccOf 1 1)
        ref <- newIORef ([] :: [String]) -- hold 'f' args in reverse order
        let f s = modifyIORef ref (s:) >> return ("f " ++ s)
            many = replicate (1000 * 1000) "x"
            expectRef = ["x"]
        mapM_ (withCacher c f) many
        r <- readIORef ref
        assertEqual "ref" expectRef r
  in TestLabel "cacherPerfTest" tc

cacherInsertTest :: Test
cacherInsertTest =
  let tc = TestCase $ do
        c <- newCacher (ccOf 10 10)
        m1 <- checkCacher c "x"
        assertEqual "starts empty" Nothing m1
        insertCacher c ("x" :: String) ("a" :: String)
        m2 <- checkCacher c "x"
        assertEqual "holds a" (Just "a") m2
        insertCacher c "x" "b"
        m3 <- checkCacher c "x"
        assertEqual "still holds a, not b" (Just "a") m3
  in TestLabel "cacherInsertTest" tc

cacherFailureTest :: Test
cacherFailureTest =
  let tc = TestCase $ do
        c <- newCacher (ccOf 10 10)
        e1 <- try $ withCacher c (\ _ -> throwIO $ ErrorCall "boom a") "y"
        assertEqual "failure in withCacher"
          (Left (ErrorCall "boom a")) e1
        m1 <- checkCacher c ("y" :: String)
        assertEqual "failure does not set the cache"
          Nothing m1
        e2 <- try $ withCacher c (\ _ -> throwIO $ ErrorCall "boom b") "y"
        assertEqual "failure gets retried in withCacher"
          (Left (ErrorCall "boom b")) e2
        v3 <- withCacher c (\_ -> return "a") "y"
        assertEqual "success in withCacher after failure, get a"
          ("a" :: String) v3
        m3 <- checkCacher c "y"
        assertEqual "success sets the cache"
          (Just "a") m3
        v4 <- withCacher c (\_ -> return "b") "y"
        assertEqual "success does not get retried, get a not b"
          "a" v4
        m4 <- checkCacher c "y"
        assertEqual "still a, not b"
          (Just "a") m4
  in TestLabel "cacherFailureTest" tc

tests :: Test
tests = TestList [ cacherRunsOnce, cacherDropsToTen, cacherDropsToZero
                 , cacherTen, cacherZero, cacherPerfTest
                 , cacherInsertTest, cacherFailureTest ]

main :: IO ()
main =  withUnitTest $ do
  testRunner tests
