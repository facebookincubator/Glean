{-# LANGUAGE TypeApplications, QuasiQuotes #-}
module JSONWriteTest (main) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Default
import Data.Proxy
import Test.HUnit

import TestRunner
import Thrift.Protocol.JSON
import Util.String.Quasi

import Glean hiding (query)
import Glean.Angle
import Glean.Init
import Glean.Database.Test
import Glean.Write.JSON (syncWriteJsonBatch)
import Glean.Types
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import qualified Glean.Schema.Sys.Types as Sys

import TestData

mkBatch :: PredicateRef -> [ByteString] -> JsonFactBatch
mkBatch ref facts =
  JsonFactBatch
    { jsonFactBatch_predicate = ref
    , jsonFactBatch_facts = facts
    , jsonFactBatch_unit = Nothing
    }

writeJsonBatchTest :: Test
writeJsonBatchTest = TestCase $ withEmptyTestDB [] $ \env repo -> do
  let
    batches =
      [ mkBatch (getName (Proxy @Sys.Blob))
        [ "{ \"id\": 1025, \"key\": \"hello\"}"
        , "{ \"id\": 1026, \"key\": \"bye\"}"
        ]
      ]
    options = Just SendJsonBatchOptions
      { sendJsonBatchOptions_no_base64_binary = True
      }

    options' = Just SendJsonBatchOptions
      { sendJsonBatchOptions_no_base64_binary = False
      }
    mkSendJsonBatch batches options = SendJsonBatch batches options False

  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options

  results <- runQuery_ env repo $ query $
    predicate @Sys.Blob (byteArray "hello")
  case results of
    [Sys.Blob{..}] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - sys.Blob"

  -- Write another fact into the DB
  let
    batches =
      [ mkBatch (getName (Proxy @Sys.Blob))
        [ "{ \"id\": 1025, \"key\": \"test\"}" -- overlaps with previous
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Sys.Blob (byteArray "test")
  test_blob_id <- case results of
    [Sys.Blob{..}] -> return blob_id
    _ -> do _ <- assertFailure "syncWriteJsonBatch - sys.Blob 2"; return 0

  -- Test a predicate ref to an earlier fact
  let
    batches =
      [ mkBatch (getName (Proxy @Glean.Test.Predicate))
        [ "{ \"id\": 1025, \"key\": { \"sum_\" : { \"d\": " <>
          BC.pack (show test_blob_id) <> "}, \"pred\" : " <>
          BC.pack (show test_blob_id) <> "}}"
            -- most of the fields are defaulted
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate wild
  test_predicate_id <- case results of
    [Glean.Test.Predicate{..}] -> return predicate_id
    _ -> do
      _ <- assertFailure "syncWriteJsonBatch - glean.test.Predicate";
      return 0

  -- insert the same fact again, ensure we still have just one
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate wild
  assertBool "syncWriteJsonBatch - glean.test.Predicate 2" $ case results of
    [Glean.Test.Predicate{..}] -> predicate_id == test_predicate_id
    _ -> False

  -- Test a predicate ref to a fact in the same batch
  let
    batches =
      [ mkBatch (getName (Proxy @Sys.Blob))
        [ "{ \"id\": 1025, \"key\": \"test2\"}"
        ]
      , mkBatch (getName (Proxy @Glean.Test.Predicate))
        [ "{ \"id\": 1026, \"key\": { \"sum_\" : { \"d\": 1025" <>
          "}, \"pred\" : 1025}}"
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate $
      rec $ field @"pred" (byteArray "test2") end
  test_pred <- case results of
    [fact@Glean.Test.Predicate{..}] -> return fact
    _ -> do
      _ <- assertFailure "syncWriteJsonBatch - glean.test.Predicate 3"
      return def

  -- test a more interesting value:
  let
    batches =
      [ mkBatch (getName (Proxy @Glean.Test.Predicate_1))
        [ serializeJSON $ Glean.Test.Predicate_1 1027 $
            Just $ kitchenSink1 {
              Glean.Test.kitchenSink_1_pred = Sys.Blob 1028 (Just "blobby") }
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options'
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate_1 $
      rec $ field @"pred" (byteArray "blobby") end
  case results of
    [_] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - glean.test.Predicate.1"

  -- nested facts can have duplicate Ids, provided we don't refer to them
  let
    batches =
      [ mkBatch (getName (Proxy @Glean.Test.Predicate))
        -- Construct a value using "def", which will set all the IDs to 0.
        -- Note that we *must* define the nested facts, otherwise they
        -- will default to a reference to fact 0, which will be an error.
        [ serializeJSON $ def
            { Glean.Test.predicate_key = Just $ def
                { Glean.Test.kitchenSink_pred =
                    def { Sys.blob_key = Just "abc" }
                , Glean.Test.kitchenSink_sum_ =
                  Glean.Test.KitchenSink_sum__d $
                    def { Sys.blob_key = Just "def" }
                }
            }
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options'
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate $
      rec $ field @"pred" (byteArray "abc") end
  case results of
    [_] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - glean.test.Predicate.1"

  -- Round-trip test: serialize the fact and insert it again
  let
    batches =
      [ mkBatch (getName (Proxy @Glean.Test.Predicate))
        [ serializeJSON test_pred
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate $
      rec $ field @"pred" (byteArray "test2") end
  void $ case results of
    [fact@Glean.Test.Predicate{..}] -> return fact
    _ -> do
      _ <- assertFailure "syncWriteJsonBatch - glean.test.Predicate 4"
      return def

  -- Test for ids very far apart
  let
    batches =
      [ mkBatch (getName (Proxy @Sys.Blob))
        [ "{ \"id\": 1024, \"key\": \"test4\"}"
        , "{ \"id\": 4611686018427387903, \"key\": \"test5\"}"
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Sys.Blob (byteArray "test5")
  print results
  case results of
    [_] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - sparse ids"

  -- Test for missing ids
  let
    batches =
      [ mkBatch (getName (Proxy @Sys.Blob))
        [ "{\"key\": " <> BC.pack (show s) <> "}"
        | s <- sequence ["ab","bc","cd"]
        ]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Sys.Blob (byteArray "bcd")
  print results
  case results of
    [_] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - missing ids"

  -- Test for mixture of named + anon facts
  let
    batches =
      [ mkBatch (getName (Proxy @Sys.Blob)) $
        [ "{\"key\": " <> BC.pack (show s) <> "}"
        | s <- sequence ["ab","bc","cd"]
        ] ++
        [[s| {"id": 10000, "key": "abba"} |]]
      , mkBatch (getName (Proxy @Glean.Test.Predicate))
        [[s|
          { "key": { "sum_" : { "d": 10000 }, "pred" : 10000}}
        |]]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate $
      rec $ field @"pred" (byteArray "abba") end
  print results
  case results of
    [_] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - mixture of named + anon"

  -- Test for nested facts
  let
    batches =
      [ mkBatch (getName (Proxy @Glean.Test.Predicate))
        [[s|
        { "key":
          { "sum_": { "d": { "id": 10000, "key": "abba" }}
          , "pred": 10000
          }
        }
        |]]  --  id 10000 is defined and used in the same fact
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate $
      rec $ field @"pred" (byteArray "abba") end
  print results
  case results of
    [_] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - nested facts"

  -- Named facts that refer to anon facts
  let
    batches =
      [ mkBatch (getName (Proxy @Glean.Test.Predicate))
        [[s|
        { "id": 3
        , "key":
          { "sum_": { "d": { "key": "abba" }}
          , "pred": { "key": "baab" }
          }
        }
        |]
        ,[s|
        { "id": 4
        , "key":
          { "sum_": { "c": 3 }
          , "pred": { "key": "baab" }
          }
        }
        |]]
      ]
  _ <- syncWriteJsonBatch env repo $ mkSendJsonBatch batches options
  results <- runQuery_ env repo $ query $
    predicate @Glean.Test.Predicate $
      rec $ field @"pred" (byteArray "baab") end
  print results
  case results of
    [_,_] -> return ()
    _ -> assertFailure "syncWriteJsonBatch - named fact that refers to anon facts"

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "writeJsonBatchTest" writeJsonBatchTest
  ]
