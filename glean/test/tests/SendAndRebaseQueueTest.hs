{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}
module SendAndRebaseQueueTest
  ( main
  ) where

import Control.Exception
import Control.Monad
import Data.Default
import Data.List
import Data.ByteString (ByteString)
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Test.HUnit

import TestRunner

import Glean
import Glean.Backend.Types (loadPredicates)
import Glean.Backend.Local (Env, loadDbSchema)
import Glean.Database.Ownership
import Glean.Database.Schema (schemaInventory)
import Glean.Database.Schema.Types (DbSchema)
import Glean.Database.Test
import Glean.Init
import Glean.Query.Angle
import qualified Glean.Schema.GleanTest as Glean.Test
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Typed (buildBatch)
import qualified Glean.Types as Thrift
import Glean.Write.JSON ( buildJsonBatch )

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
  sendAndRebaseQueueSenders = 1,
  sendAndRebaseQueueAllowRemoteReferences = True
  }

sendAndRebaseQueueTest :: Test
sendAndRebaseQueueTest = TestCase $
  withTestEnv [] $ \env -> do
    let base = Repo "base" "0"
    kickOffTestDB env base id
    mkGraph env base (void mkD)

    let fakeLog event = case event of
          SendQueueSending _batch -> putStrLn "sending"
          SendQueueSent size _t -> putStrLn $  "sent " <> show size
          SendQueueFinished -> putStrLn "finished"
          SendQueueFailed ex -> putStrLn $ "failed: " <> show ex

    dbSchema <- loadDbSchema env base
    let inventory = schemaInventory dbSchema
    predicates <- loadPredicates env base [ Glean.Test.allPredicates ]
    withSendAndRebaseQueue env base inventory (settings fakeLog) $ \queue -> do
      -- I tried to make this test deterministically stimulate the
      -- rebase behaviour in the queue, but wasn't able to make
      -- something that worked without deadlocking.
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

    withSendAndRebaseQueue env base inventory (settings mempty) $ \queue -> do
      batch <- mkReferencingBase env base dbSchema
      writeSendAndRebaseQueue queue batch (const $ return ())

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
  where
    mkReferencingBase env repo schema = do
      id_ <- baseNodeId env repo
      let id = show $ fromFid $ idOf id_
      mkBatch schema (PredicateRef "glean.test.Edge" 5)
        [ bs [i| { "key": { "parent": #{id}, "child": #{id} } }|]]

    bs = Text.encodeUtf8 . Text.pack

    baseNodeId env repo = do
      [fact] <- runQuery_ env repo $ angle @Glean.Test.Node $ Text.pack
        [i| glean.test.Node { label = "d" } |]
      return $ getId fact

    mkBatch :: DbSchema -> PredicateRef -> [ByteString] -> IO Thrift.Batch
    mkBatch schema ref facts = buildJsonBatch schema Nothing [jsonBatch]
      where
        jsonBatch = JsonFactBatch
          { jsonFactBatch_predicate = ref
          , jsonFactBatch_facts = facts
          , jsonFactBatch_unit = Nothing
          }

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
