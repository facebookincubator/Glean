{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module SendAndRebaseQueueTest
  ( main
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.List
import Test.HUnit

import TestRunner

import Glean
import Glean.Backend.Types (loadPredicates)
import Glean.Backend.Local (Env, loadDbSchema)
import Glean.Database.Ownership
import Glean.Database.Schema (schemaInventory)
import Glean.Database.Test
import Glean.Init
import Glean.Query.Angle
import qualified Glean.Schema.GleanTest as Glean.Test
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Typed (buildBatch)

mkGraph
  :: Env
  -> Repo
  -> (forall m. NewFact m => m ())
  -> IO ()
mkGraph env repo m =
  writeFactsIntoDB env repo [ Glean.Test.allPredicates ] m

mkD :: NewFact m => m Glean.Test.Node
mkD =
  withUnit "D" $
    makeFact @Glean.Test.Node (Glean.Test.Node_key "d")

mkC :: NewFact m => Glean.Test.Node -> m Glean.Test.Node
mkC d = withUnit "C" $ do
  c <- makeFact @Glean.Test.Node (Glean.Test.Node_key "c")
  makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key c d)
  return c

mkB :: NewFact m => Glean.Test.Node -> m Glean.Test.Node
mkB d = withUnit "B" $ do
  b <- makeFact @Glean.Test.Node (Glean.Test.Node_key "b")
  makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key b d)
  return b

mkA :: NewFact m => Glean.Test.Node -> Glean.Test.Node -> m ()
mkA b c =
  withUnit "A" $ do
    a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
    makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a b)
    makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)

mkA2 :: NewFact m => Glean.Test.Node -> Glean.Test.Node -> m ()
mkA2 b c =
  withUnit "X" $ do
    a <- makeFact @Glean.Test.Node (Glean.Test.Node_key "a")
    makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a b)
    makeFact_ @Glean.Test.Edge (Glean.Test.Edge_key a c)

settings :: (SendQueueEvent -> IO ()) -> SendAndRebaseQueueSettings
settings log = SendAndRebaseQueueSettings {
  sendAndRebaseQueueSendQueueSettings = def {
    sendQueueLog = log },
  sendAndRebaseQueueFactCacheSize = 1024*1024,
  sendAndRebaseQueueSenders = 1
  }

sendAndRebaseQueueTest :: Test
sendAndRebaseQueueTest = TestCase $
  withTestEnv [] $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base (void mkD)

    can_send <- newEmptyMVar

    let fakeLog event = case event of
          SendQueueSending _batch -> do
            putStrLn "sending: wait"
            takeMVar can_send
            putStrLn "sending: go"
          SendQueueSent size _t -> putStrLn $  "sent " <> show size
          SendQueueFinished -> putStrLn "finished"
          SendQueueFailed ex -> putStrLn $ "failed: " <> show ex

    dbSchema <- loadDbSchema env base
    let inventory = schemaInventory dbSchema
    predicates <- loadPredicates env base [ Glean.Test.allPredicates ]
    withSendAndRebaseQueue env base inventory (settings fakeLog) $ \queue -> do
      batch <- buildBatch predicates Nothing $ do
        d <- mkD
        void $ mkB d
        void $ mkC d
      writeSendAndRebaseQueue queue batch (const $ return ())
      batch2 <- buildBatch predicates Nothing $ do
        d <- mkD
        b <- mkB d
        c <- mkC d
        mkA b c
      writeSendAndRebaseQueue queue batch2 (const $ return ())
      putMVar can_send ()
      batch3 <- buildBatch predicates Nothing $ do
        d <- mkD
        b <- mkB d
        c <- mkC d
        mkA2 b c
      writeSendAndRebaseQueue queue batch3 (const $ return ())
      putMVar can_send ()

    completeTestDB env base

    -- check the owners of Node "b"
    results <- runQuery_ env base $ query $
      predicate @Glean.Test.Node (rec $ field @"label" (string "b") end)
    case results of
      [Glean.Test.Node id _] -> do
        ownerExpr <- factOwnership env base (Fid id)
        print ownerExpr
        assertBool "owners" $
          case ownerExpr of
            Just (OrOwners [Unit x, Unit y, Unit z]) ->
              sort [x,y,z] == ["A","B","X"]
            _otherwise -> False
      _ -> assertFailure "query failed"

-- | Check that errors from the sender get propagated properly
sendAndRebaseQueueFailureTest :: Test
sendAndRebaseQueueFailureTest = TestCase $
  withTestEnv [] $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base (void mkD)

    let
        msg = "failing in SendQueueSending"
        fakeLog event = case event of
          SendQueueSending _batch -> throwIO $ ErrorCall msg
          SendQueueSent size _t -> putStrLn $  "sent " <> show size
          SendQueueFinished -> putStrLn "finished"
          SendQueueFailed ex -> putStrLn $ "failed: " <> show ex

    dbSchema <- loadDbSchema env base
    let inventory = schemaInventory dbSchema
    predicates <- loadPredicates env base [ Glean.Test.allPredicates ]
    r <- try $ withSendAndRebaseQueue
           env base inventory (settings fakeLog) $ \queue -> do
      batch <- buildBatch predicates Nothing $ do
        d <- mkD
        void $ mkB d
        void $ mkC d
      writeSendAndRebaseQueue queue batch (const $ return ())
      batch2 <- buildBatch predicates Nothing $ do
        d <- mkD
        b <- mkB d
        c <- mkC d
        mkA b c
      writeSendAndRebaseQueue queue batch2 (const $ return ())
      batch3 <- buildBatch predicates Nothing $ do
        d <- mkD
        b <- mkB d
        c <- mkC d
        mkA2 b c
      writeSendAndRebaseQueue queue batch3 (const $ return ())

    assertBool "failure" $ case r of
      Left (ErrorCall m) -> m == msg
      _ -> False

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "sendAndRebaseQueueTest" sendAndRebaseQueueTest
  , TestLabel "sendAndRebaseQueueFailureTest" sendAndRebaseQueueFailureTest
  ]
