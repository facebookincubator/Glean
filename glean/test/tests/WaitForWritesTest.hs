{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module WaitForWritesTest (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.Default
import Data.Proxy
import qualified Data.ByteString.Char8 as BC
import Test.HUnit

import TestRunner

import Glean.Database.Test
import Glean.Database.Types (Env)
import Glean.Database.Writes (pollWaitForWrites, enqueueJsonBatch)
import Glean.Init
import Glean.Test.HUnit
import Glean.Typed (getName)
import Glean.Types as Thrift
import qualified Glean.Schema.Sys.Types as Sys

mkBlob :: String -> JsonFactBatch
mkBlob val = JsonFactBatch
  { jsonFactBatch_predicate = getName (Proxy @Sys.Blob)
  , jsonFactBatch_facts = [BC.pack ("{ \"key\": \"" <> val <> "\" }")]
  , jsonFactBatch_unit = Nothing
  }

mkBatch :: [JsonFactBatch] -> SendJsonBatch
mkBatch bs = def
  { sendJsonBatch_batches = bs
  , sendJsonBatch_options = Just def
      { sendJsonBatchOptions_no_base64_binary = True }
  }

-- | Poll until pending writes reach 0, with a bounded retry to avoid
-- hanging in CI if writes never drain due to a bug.
pollUntilDone :: Env -> Thrift.Repo -> IO ()
pollUntilDone env repo = go (500 :: Int)
  where
    go 0 = assertFailure "timed out waiting for pending writes to drain"
    go n = do
      resp <- pollWaitForWrites env repo
      when (waitForWritesResponse_pending_writes_count resp > 0) $ do
        threadDelay 100000 -- 100ms
        go (n - 1)

-- | On a fresh writable DB with no pending writes,
-- pollWaitForWrites should return 0.
waitForWritesNoWrites :: Test
waitForWritesNoWrites = TestCase $
  withEmptyTestDB [] $ \env repo -> do
    resp <- pollWaitForWrites env repo
    assertEqual "no pending writes on fresh DB"
      0 (waitForWritesResponse_pending_writes_count resp)

-- | Enqueue a single async write, wait for it to drain,
-- and verify the count reaches 0.
waitForWritesWithAsyncWrite :: Test
waitForWritesWithAsyncWrite = TestCase $
  withEmptyTestDB [] $ \env repo -> do
    _ <- enqueueJsonBatch env repo (mkBatch [mkBlob "hello"])
    pollUntilDone env repo
    resp <- pollWaitForWrites env repo
    assertEqual "all writes complete after polling"
      0 (waitForWritesResponse_pending_writes_count resp)

-- | Enqueue many async batches to exercise the write queue under
-- heavier load, and verify all writes eventually drain.
waitForWritesMultipleBatches :: Test
waitForWritesMultipleBatches = TestCase $
  withEmptyTestDB [] $ \env repo -> do
    let batches = [mkBatch [mkBlob ("val" <> show i)] | i <- [1..10 :: Int]]
    mapM_ (enqueueJsonBatch env repo) batches
    pollUntilDone env repo
    resp <- pollWaitForWrites env repo
    assertEqual "all writes eventually complete"
      0 (waitForWritesResponse_pending_writes_count resp)

-- | On a completed (read-only) DB, odbWriting is Nothing so
-- pollWaitForWrites should return 0.
waitForWritesCompletedDB :: Test
waitForWritesCompletedDB = TestCase $
  withEmptyTestDB [] $ \env repo -> do
    completeTestDB env repo
    resp <- pollWaitForWrites env repo
    assertEqual "no pending writes on completed DB"
      0 (waitForWritesResponse_pending_writes_count resp)

-- | On an unknown repo, pollWaitForWrites should throw UnknownDatabase.
waitForWritesUnknownDB :: Test
waitForWritesUnknownDB = TestCase $
  withTestEnv [] $ \env -> do
    let unknownRepo = Thrift.Repo "does-not-exist" "0"
    assertThrowsType "unknown DB throws UnknownDatabase"
      (Proxy :: Proxy UnknownDatabase)
      (pollWaitForWrites env unknownRepo)

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "waitForWritesNoWrites" waitForWritesNoWrites
  , TestLabel "waitForWritesWithAsyncWrite" waitForWritesWithAsyncWrite
  , TestLabel "waitForWritesMultipleBatches" waitForWritesMultipleBatches
  , TestLabel "waitForWritesCompletedDB" waitForWritesCompletedDB
  , TestLabel "waitForWritesUnknownDB" waitForWritesUnknownDB
  ]
