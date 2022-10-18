{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module JSONQueryTest (main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as B
import Data.ByteString (ByteString)
import Data.Default
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit

import TestRunner
import Thrift.Protocol
import Thrift.Protocol.JSON
import Thrift.Protocol.Compact

import Util.String.Quasi

import Glean.Backend.Types
import Glean.Init
import Glean.Query.Thrift
import Glean.Query.Thrift.Internal
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Sys.Types as Sys
import qualified Glean.Schema.GleanTest.Types as Glean.Test
import qualified Glean.Schema.Query.Cxx1.Types as Query.Cxx
import qualified Glean.Schema.Query.Sys.Types as Query.Sys
import qualified Glean.Schema.Query.GleanTest.Types as Query.Glean.Test
import Glean.Types as Thrift
import Glean.Typed as Typed

import TestData

import TestDB

query :: forall q p . (Predicate p, ThriftSerializable q) => q -> Query p
query = mkQuery . serializeJSON

type TestQuery p =
  ( Typed.Predicate p
  , ThriftSerializable p
  )

-- | Perform a query using the Thrift query and result types, using
-- JSON transport for the results. This is only for testing, use
-- `runQuery` instead.
runQueryJSON_
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO [q]
runQueryJSON_ be repo q = fst <$> runQueryJSON be repo q


runQueryJSON
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO ([q], Bool)
runQueryJSON be repo (Query query) = do
  -- Only works for whole facts, but that's enforced by the TestQuery context.
  let
    query' = query
      { userQuery_options = Just (fromMaybe def (userQuery_options query))
        { userQueryOptions_expand_results = True } }
  UserQueryResults{..} <- userQuery be repo query'
  mapM_ reportUserQueryStats userQueryResults_stats
  results <- forM userQueryResults_facts $ \fact -> do
    case deserializeJSON fact of
      Left err -> throwIO $ ErrorCall $ "deserialization failure: " ++ err
      Right x -> return x
  return (results, isJust userQueryResults_continuation)

runQueryCompact_
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO [q]
runQueryCompact_ be repo q = fst <$> runQueryCompact be repo q

runQueryCompact
  :: (TestQuery q, Backend b)
  => b
  -> Repo
  -> Query q
  -> IO ([q], Bool)
runQueryCompact be repo (Query query) = do
  -- Only works for whole facts, but that's enforced by the TestQuery context.
  let
    query' = query
      { userQuery_encodings =
          [ UserQueryEncoding_compact UserQueryEncodingCompact
              { userQueryEncodingCompact_expand_results = True }
          ]
      }
  UserQueryResults{..} <- userQuery be repo query'
  UserQueryEncodedResults_compact UserQueryResultsCompact{..} <-
    return userQueryResults_results
  mapM_ reportUserQueryStats userQueryResults_stats
  results <- forM userQueryResultsCompact_facts $ \fact -> do
    case deserializeCompact fact of
      Left err -> throwIO $ ErrorCall $
        "deserialization failure: "
        ++ err
        ++ " " ++ show (B.toLazyByteString $ B.byteStringHex fact)
      Right x -> return x
  return (results, isJust userQueryResults_continuation)

userQueryFactTest :: Test
userQueryFactTest = dbTestCase $ \env repo -> do
  -- Test userQueryFacts and decode the JSON result
  results1 :: [Glean.Test.Predicate_1] <-
    runQuery_ env repo $ query $
      Query.Glean.Test.Predicate_1_with_key def {
        Query.Glean.Test.kitchenSink_1_byt = Just (Byte 33) }
  results2 <- runQuery_ env repo (allFacts :: Query Sys.Blob)

  let
    factIds =
      map (fromFid . idOf . getId) results1 ++
      map (fromFid . idOf . getId) results2
    factQuery =
      [ def { factQuery_id = id } | id <- factIds ]

  UserQueryResults{..} <-
    userQueryFacts env repo def { userQueryFacts_facts = factQuery }
  assertEqual "userQueryFacts" 3 (length userQueryResults_facts)
  let testPred = deserializeJSON $ head userQueryResults_facts
  assertEqual "userQueryFacts decode" (Right (Just (ignorePredK kitchenSink1)))
    (fmap (Glean.Test.predicate_1_key . ignorePredP) testPred)

  -- Test userQueryFacts and decode the binary result
  UserQueryResults{..} <-
    userQueryFacts env repo def
      { userQueryFacts_facts = factQuery
      , userQueryFacts_encodings = [UserQueryEncoding_bin def] }
  results <- case userQueryResults_results of
    UserQueryEncodedResults_bin UserQueryResultsBin{..} ->
      return userQueryResultsBin_facts
    _ -> assertFailure "wrong encoding"
  assertEqual "userQueryFacts" 3 $ Map.size results
  blobs :: [ByteString] <- mapM (decodeRts . fact_key) $ catMaybes
    [ Map.lookup id results
    | id <- map (fromFid . idOf . getId) results2 ]
  assertEqual "userQueryFacts decode" ["bye","hello"] (sort blobs)

  -- Check that querying for the same fact twice works
  UserQueryResults{..} <-
    userQueryFacts env repo def
      { userQueryFacts_facts = case factQuery of
         (one : two : _) -> [ one, one, two, two ]
         _ -> error "factQuery"
      , userQueryFacts_encodings = [UserQueryEncoding_json def] }
  results <- case userQueryResults_results of
    UserQueryEncodedResults_json UserQueryResultsJSON{..} ->
      return userQueryResultsJSON_facts
    _ -> assertFailure "wrong encoding"
  assertEqual "userQueryFacts" 4 (length results)
  return ()

ignorePredK :: Glean.Test.KitchenSink_1 -> Glean.Test.KitchenSink_1
ignorePredK k = k { Glean.Test.kitchenSink_1_pred = def }

ignorePredP :: Glean.Test.Predicate_1 -> Glean.Test.Predicate_1
ignorePredP p = p { Glean.Test.predicate_1_key =
  fmap ignorePredK (Glean.Test.predicate_1_key p)}

jsonQueryErrorCases :: Test
jsonQueryErrorCases = dbTestCase $ \env repo -> do
  -- invalid JSON
  r <- try $ userQuery env repo def
    { userQuery_predicate = "glean.test.Predicate"
    , userQuery_predicate_version = Just 1
    , userQuery_query = "this is not JSON" }
  print r
  assertBool "jsonQuery - invalid JSON" $
    case r of
      Left (BadQuery e)
        | "query is not valid JSON" `Text.isPrefixOf` e -> True
      _ -> False

  -- unknown predicate
  r <- try $ userQuery env repo def
    { userQuery_predicate = "no.such.thing"
    , userQuery_predicate_version = Just 1
    , userQuery_query = "{ \"key\" : 3 }" }
  print r
  assertBool "jsonQuery - invalid predicate" $
    case r of
      Left (BadQuery e)
        | "not in scope" `Text.isPrefixOf` e -> True
      _ -> False

  -- valid JSON, but not a valid query
  r <- try $ userQuery env repo def
    { userQuery_predicate = "glean.test.Predicate"
    , userQuery_predicate_version = Just 1
    , userQuery_query = "{ \"wrong\" : 3 }" }
  print r
  assertBool "jsonQuery - invalid query" $
    case r of
      Left (BadQuery e)
        | "Expecting a query for the predicate" `Text.isInfixOf` e -> True
      _ -> False

  -- query doesn't match schema
  r <- try $ userQuery env repo def
    { userQuery_predicate = "glean.test.Predicate"
    , userQuery_predicate_version = Just 1
    , userQuery_query = "{ \"key\" : 3 }" }
  print r
  assertBool "jsonQuery - type error" $
    case r of
      Left (BadQuery e)
        | "Expecting a query for the type" `Text.isInfixOf` e -> True
      _ -> False

  -- query doesn't match schema: unknown field in record
  r <- try $ userQuery env repo def
    { userQuery_predicate = "glean.test.Predicate"
    , userQuery_predicate_version = Just 1
    , userQuery_query = "{ \"key\" : { \"nofield\" : 3 }}" }
  print r
  assertBool "jsonQuery - unknown field" $
    case r of
      Left (BadQuery e)
        | "Expecting a query for the type" `Text.isInfixOf` e -> True
      _ -> False

  -- the innards of an "every" query must be irrefutable. Sum queries
  -- with "any":false are refutable if there are missing alts.
  r <- try $ userQuery env repo def
    { userQuery_predicate = "cxx1.FileXRefs"
    , userQuery_predicate_version = Just 5
    , userQuery_query =
       "{ \"key\": " <>
           "{ \"xmap\": { \"id\": 42 }" <>
           ", \"externals\": { \"every\": " <>
                "{ \"any\" : false" <>
                ", \"objcSelector\" : { \"get\" : {} }" <>
       "}}}}"
    }
  print r
  assertBool "jsonQuery - every must be irrefutable" $
    case r of
      Left (BadQuery e)
        | "must be irrefutable" `Text.isInfixOf` e -> True
      _ -> False


type QueryFun =
   forall p b .
    ( ThriftSerializable p
    , Backend b
    , Typed.Predicate p
    )
  => b
  -> Repo
  -> Query p
  -> IO [p]

jsonQueryTest :: QueryFun -> (forall a . Query a -> Query a) -> Test
jsonQueryTest queryFun modify = dbTestCase $ \env repo -> do
  let
    runQuery ::
      ( ThriftSerializable query
      , ThriftSerializable q
      , Typed.Predicate q
      )
      => query
      -> IO [q]
    runQuery q = queryFun env repo (modify (query q))

  -- match zero results
  results :: [Sys.Blob] <- runQuery (Query.Sys.Blob_with_key "nomatch")
  assertBool "userQuery - sys.Blob nomatch" $ null results

  -- match all results (one)
  results <- runQuery (Query.Sys.Blob_with_key "hello")
  sysBlobId <-
    case results of
      [Sys.Blob{..}] -> return blob_id
      _ -> assertFailure "userQuery - sys.Blob"

  -- using JSON with no base64
  r <- try $ userQuery env repo def
    { userQuery_predicate = "sys.Blob"
    , userQuery_predicate_version = Just 1
    , userQuery_query = "{ \"key\" : \"hello\"}"
    , userQuery_options =
      Just def { userQueryOptions_no_base64_binary = True } }
  assertBool "userQuery - no base64" $
    case r :: Either SomeException UserQueryResults of
      Right UserQueryResults { userQueryResults_facts = [txt] }
        | "hello" `ByteString.isInfixOf` txt -> True
      _ -> False

  -- query that matches everything
  results :: [Sys.Blob] <- runQuery (Query.Sys.Blob_with_get def)
  assertBool "userQuery - sys.Blob match all" $ length results == 2

  -- match one result of many
  results <- runQuery $
    Query.Glean.Test.Predicate_1_with_key
      def { Query.Glean.Test.kitchenSink_1_named_sum_ =
              Just (def { Query.Glean.Test.sum_tue =
                            Just  (Nat 37) })}
  assertBool "userQuery - glean.test.Predicate 1" $
    case results of
      [Glean.Test.Predicate_1{Glean.Test.predicate_1_key = Just k}] ->
        ignorePredK k == ignorePredK kitchenSink1
      _ -> False

  -- match all results (two)
  results :: [Glean.Test.Predicate_1] <-
    runQuery $ Query.Glean.Test.Predicate_1_with_key def
  assertBool "userQuery - glean.test.Predicate 2" $
    case results of
      [_, _] -> True
      _ -> False

  -- query by nested fact Id, should be two results
  results :: [Glean.Test.Predicate_1] <-
    runQuery $ Query.Glean.Test.Predicate_1_with_key
    def { Query.Glean.Test.kitchenSink_1_pred =
      Just (Query.Sys.Blob_with_id sysBlobId) }
  assertBool "userQuery - nested fact Id" $ length results == 1

  -- match two nested results
  results <- runQuery $ Query.Glean.Test.Predicate_1_with_key
    def { Query.Glean.Test.kitchenSink_1_pred =
      Just (Query.Sys.Blob_with_get def) }
  assertBool "userQuery - glean.test.Predicate deep" $
    case results of
      [f1, f2]
        | Just key1 <- Glean.Test.predicate_1_key f1
        , Just key2 <- Glean.Test.predicate_1_key f2
        , let blobs = map (Sys.blob_key . Glean.Test.kitchenSink_1_pred)
                        [key1,key2]
        , Just "hello" `elem` blobs
        , Just "bye" `elem` blobs -> True
      _ -> False

  -- match one nested pattern
  results <- runQuery $
    Query.Glean.Test.Predicate_1_with_key
      def { Query.Glean.Test.kitchenSink_1_pred =
        Just (Query.Sys.Blob_with_key "hello") }
  assertBool "userQuery - glean.test.Predicate nested pattern" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_key (Glean.Test.kitchenSink_1_pred key) == Just "hello" ->
          True
      _ -> False

  -- match a maybe that's missing
  results <- runQuery $
    Query.Glean.Test.Predicate_1_with_key def
      { Query.Glean.Test.kitchenSink_1_maybe_ =
        Just (def { Query.Glean.Test.kitchenSink_1_maybe__nothing = Just def })
      , Query.Glean.Test.kitchenSink_1_pred = Just (Query.Sys.Blob_with_get def)
      }
  assertBool "userQuery - maybe = nothing" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_key (Glean.Test.kitchenSink_1_pred key) == Just "hello" ->
          True
      _ -> False

  -- match a maybe that's present
  results <- runQuery $
    Query.Glean.Test.Predicate_1_with_key def
      { Query.Glean.Test.kitchenSink_1_maybe_ =
        Just (def { Query.Glean.Test.kitchenSink_1_maybe__just = Just def })
      , Query.Glean.Test.kitchenSink_1_pred = Just (Query.Sys.Blob_with_get def)
      }
  assertBool "userQuery - maybe = just" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_key (Glean.Test.kitchenSink_1_pred key) == Just "bye" -> True
      _ -> False

  -- match a maybe that's either present or missing
  results :: [Glean.Test.Predicate_1] <- runQuery $
    Query.Glean.Test.Predicate_1_with_key def
      { Query.Glean.Test.kitchenSink_1_maybe_ =
        Just (def { Query.Glean.Test.kitchenSink_1_maybe__any = True })
      , Query.Glean.Test.kitchenSink_1_pred = Just (Query.Sys.Blob_with_get def)
      }
  assertBool "userQuery - maybe = any" $
    case results of
      [_,_] -> True
      _ -> False

  -- unpack the facts inside a sum type
  results <- runQuery $
    Query.Glean.Test.Predicate_with_key def
      { Query.Glean.Test.kitchenSink_sum_ = Just def
        { Query.Glean.Test.kitchenSink_sum__any = True  -- match any alt
        , Query.Glean.Test.kitchenSink_sum__d = -- unpack d:sys.Blob
            Just (Query.Sys.Blob_with_get def)
        , Query.Glean.Test.kitchenSink_sum__c = -- unpack c:glean.test.Pred
            Just (Query.Glean.Test.Predicate_with_key def
              { Query.Glean.Test.kitchenSink_sum_ = Just def
                { Query.Glean.Test.kitchenSink_sum__any = True
                , Query.Glean.Test.kitchenSink_sum__d = -- unpack c.d:sys.Blob
                    Just (Query.Sys.Blob_with_get def) }})
        } }
  assertBool "userQuery - sum - unpack 1" $
    case [ r | r@Glean.Test.Predicate { Glean.Test.predicate_key = Just
                 Glean.Test.KitchenSink { Glean.Test.kitchenSink_sum_ =
                 Glean.Test.KitchenSink_sum__d
                 Sys.Blob { Sys.blob_key = Just "hello" }}}
                  <- results ] of
      [_one, _two] -> True
      _ -> False
  -- two levels deep through sums, with a recursive predicate
  -- This also tests that the nested match can return multiple results.
  assertBool "userQuery - sum - unpack 2" $
    case [ r | r@Glean.Test.Predicate { Glean.Test.predicate_key = Just
                 Glean.Test.KitchenSink { Glean.Test.kitchenSink_sum_ =
                 Glean.Test.KitchenSink_sum__c
                 Glean.Test.Predicate { Glean.Test.predicate_key = Just
                 Glean.Test.KitchenSink { Glean.Test.kitchenSink_sum_ =
                 Glean.Test.KitchenSink_sum__d
                 Sys.Blob { Sys.blob_key = Just "hello" }}}}}
                  <- results ] of
      [_one, _two] -> True
      _ -> False

  -- match multiple alternatives of a sum type.
  --
  --  If we get our generators wrong in the JSON->generator compiler,
  -- this will match the same fact multiple times and return too many
  -- results.
  results :: [Glean.Test.Predicate] <- runQuery $
    Query.Glean.Test.Predicate_with_key def
      { Query.Glean.Test.kitchenSink_sum_ = Just def
        { Query.Glean.Test.kitchenSink_sum__d =
            -- match d:sys.Blob (2 of these)
            Just (Query.Sys.Blob_with_key "hello")
        , Query.Glean.Test.kitchenSink_sum__c =
            -- match c:glean.test.Pred (2 of these)
            Just (Query.Glean.Test.Predicate_with_key def
              { Query.Glean.Test.kitchenSink_sum_ = Just def
                { Query.Glean.Test.kitchenSink_sum__d =
                      -- match c.d:sys.Blob (2 of these)
                    Just (Query.Sys.Blob_with_key "hello") }})
        } }
  print results
  assertBool "userQuery - sum - multiple alts" $
    case results of
      [_one, _two, _three, _four] -> True
      _ -> False

  -- sum type: one branch matches nothing
  results :: [Glean.Test.Predicate] <- runQuery $
    Query.Glean.Test.Predicate_with_key def
      { Query.Glean.Test.kitchenSink_sum_ = Just def
        { Query.Glean.Test.kitchenSink_sum__d =
            -- match d:sys.Blob (2 of these)
            Just (Query.Sys.Blob_with_key "hello")
        , Query.Glean.Test.kitchenSink_sum__c =
            -- match c:glean.test.Pred (2 of these)
            Just (Query.Glean.Test.Predicate_with_key def
              { Query.Glean.Test.kitchenSink_sum_ = Just def
                { Query.Glean.Test.kitchenSink_sum__d =
                    Just (Query.Sys.Blob_with_get def)
                , Query.Glean.Test.kitchenSink_sum__c =
                    -- matches nothing:
                    Just (Query.Glean.Test.Predicate_with_key def
                      { Query.Glean.Test.kitchenSink_nat = Just (toNat 99)
                      })
                }
              })
        } }
  print results
  assertBool "userQuery - sum - one branch matches nothing" $
    case results of
      [_one, _two, _three, _four] -> True
      _ -> False

  -- match mutliple alternatives of a sum type, with a refutable pattern
  -- in one alternative.
  results :: [Glean.Test.Predicate] <- runQuery $
    Query.Glean.Test.Predicate_with_key def
      { Query.Glean.Test.kitchenSink_sum_ = Just def
        { Query.Glean.Test.kitchenSink_sum__any = True
          -- should be two preds that match the d alt (missing)
          -- and just one pred that matches this:
        , Query.Glean.Test.kitchenSink_sum__c = Just $
            Query.Glean.Test.Predicate_with_key def
              { Query.Glean.Test.kitchenSink_nat = Just (toNat 42) }
      } }
  assertBool "userQuery - sum - multiple alts (refutable)" $
    case results of
      [_one, _two, _three] -> True
      _ -> False

  -- match an array
  results <- runQuery $
    Query.Glean.Test.Predicate_1_with_key def
      { Query.Glean.Test.kitchenSink_1_array_of_nat =
          Just $ Query.Glean.Test.KitchenSink_1_array_of_nat_array_exact
            [Nat 99, Nat 98]
      , Query.Glean.Test.kitchenSink_1_pred = Just (Query.Sys.Blob_with_get def)
      }
  assertBool "userQuery - array - exact" $
    case results of
      [f1] | Just key <- Glean.Test.predicate_1_key f1
        , Sys.blob_key (Glean.Test.kitchenSink_1_pred key) == Just "bye" -> True
      _ -> False

  -- fetch contents of array elements
  results <- runQuery $
    Query.Glean.Test.Predicate_with_key def
      { Query.Glean.Test.kitchenSink_array_of_pred =
          Just $ Query.Glean.Test.KitchenSink_array_of_pred_array_every $
            Query.Glean.Test.Predicate_with_key def
              { Query.Glean.Test.kitchenSink_pred = Just $
                  Query.Sys.Blob_with_get def }
      }
  assertBool "userQuery - array - every" $
    case [ r | r@Glean.Test.Predicate { Glean.Test.predicate_key = Just
                 Glean.Test.KitchenSink { Glean.Test.kitchenSink_array_of_pred =
                 [
                   Glean.Test.Predicate { Glean.Test.predicate_key = Just
                   Glean.Test.KitchenSink { Glean.Test.kitchenSink_pred =
                   Sys.Blob { Sys.blob_key = Just "hello" }}}
                 ] }} <- results ] of
      [_one, _two] -> True
      _ -> False

  -- match against a string
  results :: [Glean.Test.Predicate_1] <- runQuery $
    Query.Glean.Test.Predicate_1_with_key def
      { Query.Glean.Test.kitchenSink_1_string_ = Just "Hello\0world!\0" }
  assertBool "userQuery - string" $
    length results == 1

  -- string inside a sum type
  results :: [Cxx.FunctionName] <- runQuery $
    Query.Cxx.FunctionName_with_key $ def
      { Query.Cxx.functionName_key_name = Just (Query.Cxx.Name_with_key "abba")
      , Query.Cxx.functionName_key_operator_ = Just "abba"
      }
  assertBool "userQuery - string inside sum" $
    length results == 1

  -- match an empty set of alternatives using JSON
  r :: [Glean.Test.Predicate] <- queryFun env repo $ modify $ mkQuery
    ([s| { "key" : { "sum_" : {} }} |] :: String)
  assertBool "userQuery - empty alternatives" $ null r

jsonRecQueryTest :: QueryFun -> Test
jsonRecQueryTest queryFun = dbTestCase $ \env repo -> do
  let
    runQuery ::
      ( ThriftSerializable query
      , ThriftSerializable q
      , Typed.Predicate q
      )
      => query
      -> IO [q]
    runQuery q = queryFun env repo (recursive (query q))

  -- query for test predicate, expand recursively
  results <- runQuery $ Query.Glean.Test.Predicate_with_key def
    { Query.Glean.Test.kitchenSink_pred = Just (Query.Sys.Blob_with_key "bye") }
  assertBool "userQuery - rec" $
    case [ r |
          r@Glean.Test.Predicate { Glean.Test.predicate_key = Just
          Glean.Test.KitchenSink { Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__c
          Glean.Test.Predicate { Glean.Test.predicate_key = Just
          Glean.Test.KitchenSink { Glean.Test.kitchenSink_sum_ =
          Glean.Test.KitchenSink_sum__d
          Sys.Blob { Sys.blob_key = Just "hello" }}}}} <- results ] of
      [_one, _two] -> True
      _ -> False

  -- query for a single fact by Id, expanding recursively
  result1:_ <- runQuery $ Query.Cxx.FunctionName_with_get def
  [result2] <- runQuery $ Query.Cxx.FunctionName_with_id $
    Cxx.functionName_id result1
  assertEqual "userQuery - rec - id" result1 result2

type QueryFunLimited =
   forall q b .
    ( ThriftSerializable q
    , Backend b
    , Typed.Predicate q
    )
  => b
  -> Repo
  -> Query q
  -> IO ([q], Bool)

limitTest :: QueryFunLimited -> (forall a . Query a -> Query a) -> Test
limitTest queryFun modify = dbTestCase $ \env repo -> do
  (results :: [Glean.Test.Predicate], truncated) <-
    queryFun env repo $ limit 1 $ modify $ query $
      Query.Glean.Test.Predicate_with_get def
  assertEqual "length" 1 (length results)
  assertBool "truncated" truncated

  (results :: [Cxx.Name], truncated) <-
    queryFun env repo $ limitBytes 18  $ modify $ query $
      Query.Cxx.Name_with_get def
  assertEqual "length" 2 (length results)
  assertBool "truncated" truncated

jsonPrefixQueryTest :: Test
jsonPrefixQueryTest = dbTestCase $ \env repo -> do
  let
    getResults :: UserQueryResults -> [Text]
    getResults r =
      sort [ str | Cxx.FunctionName { functionName_key = Just
                   (Cxx.FunctionName_key_name
                     Cxx.Name { name_key = Just str })} <- fnames ]
      where
        fnames =
          [ f | Right f <- map deserializeJSON $ userQueryResults_facts r ]

    -- prefix queries are only supported in JSON right now, because it
    -- would be a pain to add the possibility of prefix queries to
    -- every string field in the Thrift query types.
    query = def
      { userQuery_predicate = "cxx1.FunctionName"
      , userQuery_predicate_version = Just 5
      , userQuery_query =
          "{ \"key\" :" <>
          "  { \"name\":" <>
          "    { \"key\":" <>
          "      { \"prefix\": \"ab\" }}}}" }

  r <- userQuery env repo query
  assertBool "jsonQueryPrefixQueryTest" $
    getResults r == [ "abba", "abcd"]

  -- Test paging prefix queries
  r <- userQuery env repo query
    { userQuery_options = Just def { userQueryOptions_max_results = Just 1 }
    }
  print r
  assertBool "jsonQueryPrefixQueryTest - limit 1" $
    getResults r == [ "abba" ] &&
    isJust (userQueryResults_continuation r)

  r <- userQuery env repo query
    { userQuery_options = Just def
      { userQueryOptions_max_results = Just 1
      , userQueryOptions_continuation = userQueryResults_continuation r }
    }
  assertBool "jsonQueryPrefixQueryTest - cont 1" $
    getResults r == [ "abcd" ] &&
    isJust (userQueryResults_continuation r)

  r <- userQuery env repo query
    { userQuery_options = Just def
      { userQueryOptions_max_results = Just 1
      , userQueryOptions_continuation = userQueryResults_continuation r }
    }
  assertBool "jsonQueryPrefixQueryTest - cont 2" $
    getResults r == [ ] &&
    isNothing (userQueryResults_continuation r)

-- | All of the test cases from AngleTest.derivedTest using Thrift query
-- types instead of Angle.
jsonDerivedTest :: (forall q. Query q -> Query q) -> Test
jsonDerivedTest modify = dbTestCase $ \env repo -> do
  r :: [Glean.Test.RevStringPair] <- runQuery_ env repo $ modify $ query $
    Query.Glean.Test.RevStringPair_with_get def
  assertEqual "json derived 1" 6 (length r)

  r :: [Glean.Test.RevStringPair] <- runQuery_ env repo $ modify $ query $
    Query.Glean.Test.RevStringPair_with_key def
      { Query.Glean.Test.revStringPair_key_fst = Just "a" }
  assertEqual "json derived 2" 1 (length r)

  r :: [Glean.Test.RevStringPair] <- runQuery_ env repo $ modify $ query $
    Query.Glean.Test.RevStringPair_with_key def
      { Query.Glean.Test.revStringPair_key_snd = Just "a" }
  assertBool "json derived 3" $ case r :: [Glean.Test.RevStringPair] of
    [one] | Just key <- getFactKey one ->
      "a" == Glean.Test.revStringPair_key_snd key
    _otherwise -> False

  -- case 4 not expressible: glean.test.RevStringPair (X,X)

  r :: [Glean.Test.RevRevStringPair] <- runQuery_ env repo $ modify $ query $
    Query.Glean.Test.RevRevStringPair_with_get def
  assertEqual "json derived 5" 6 (length r)

  r :: [Glean.Test.RevRevStringPair] <- runQuery_ env repo $ modify $ query $
    Query.Glean.Test.RevRevStringPair_with_key def
      { Query.Glean.Test.revRevStringPair_key_fst = Just "a" }
  assertBool "json derived 6" $ case r :: [Glean.Test.RevRevStringPair] of
    [one] | Just key <- getFactKey one ->
      "a" == Glean.Test.revRevStringPair_key_fst key
    _otherwise -> False

  -- case 7 not expressible: glean.test.ReflStringPair "x"..

  r :: [Glean.Test.DualStringPair] <-
    runQuery_ env repo $ modify $ recursive $ query $
    Query.Glean.Test.DualStringPair_with_get def
  print (r :: [Glean.Test.DualStringPair])
  assertBool "json derived 8" $ (==4) $ length
    [ fst
    | Just key <- map getFactKey r
    , Just fst <- [getFactKey $ Glean.Test.dualStringPair_key_fst key] ]

  -- slightly modified version of case 10 with a nested pattern
  r :: [Glean.Test.DualStringPair] <-
    runQuery_ env repo $ modify $ recursive $ query $
    Query.Glean.Test.DualStringPair_with_key def
      { Query.Glean.Test.dualStringPair_key_fst = Just $
        Query.Glean.Test.StringPair_with_key def
        { Query.Glean.Test.stringPair_key_snd = Just "a" }}
  print r
  assertEqual "json derived 10" 1 (length r)


-- | Test for 'runQueryEach' using 'unsafeQueryText'
jsonPrefixQueryEachTest :: (forall q. Query q -> Query q) -> Test
jsonPrefixQueryEachTest f = dbTestCase $ \env repo -> do
  let
    getResults :: [Cxx.FunctionName] -> [Text]
    getResults fnames =
      sort [ str | Cxx.FunctionName { functionName_key = Just
                   (Cxx.FunctionName_key_name
                     Cxx.Name { name_key = Just str })} <- fnames ]

    -- prefix queries are only supported in JSON right now, because it
    -- would be a pain to add the possibility of prefix queries to
    -- every string field in the Thrift query types.
    query :: Query Cxx.FunctionName
    query = f $ mkQuery text
      where
        text :: Text
        text =
          "{ \"key\" :" <>
          "  { \"name\":" <>
          "    { \"key\":" <>
          "      { \"prefix\": \"ab\" }}}}"

    query1 :: Query Cxx.FunctionName
    query1 = limit 1 query

  r <- runQueryEach env repo query [] (\ xs x -> return (x:xs))
  print r
  assertBool "jsonPrefixQueryEachTest" $
    getResults r == [ "abba", "abcd"]

  -- Test paging prefix queries by limiting max_results to 1
  -- Note that this receives 2 answers
  r <- runQueryEach env repo query1 [] (\ xs x -> return (x:xs))
  print r
  assertBool "jsonPrefixQueryEachTest - limit 1" $
    getResults r == [ "abba", "abcd" ]

keyValueTest :: (forall q. Query q -> Query q) -> Test
keyValueTest modify = dbTestCase $ \env repo -> do
  r <- runQuery_ env repo $ modify (allFacts :: Query Glean.Test.KeyValue)
  print r
  assertEqual "keyValue 1" 3 (length r)
  assertEqual "keyValue 2" [4,5,24] $
    sort [ fromNat keyValue_value_vnat
         | Just Glean.Test.KeyValue_value{..} <-
             map Glean.Test.keyValue_value r ]

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "userQueryFactTest" userQueryFactTest
  , TestLabel "jsonQueryErrorCases" jsonQueryErrorCases
  , TestLabel "jsonQueryTest (bin)" $ jsonQueryTest runQuery_ id
  , TestLabel "jsonQueryTest (bin,page)" $ jsonQueryTest runQuery_ (limit 1)
  , TestLabel "jsonQueryTest (json)" $ jsonQueryTest runQueryJSON_ id
  , TestLabel "jsonQueryTest (compact)" $ jsonQueryTest runQueryCompact_ id
  , TestLabel "jsonRecQueryTest (bin)" $ jsonRecQueryTest runQuery_
  , TestLabel "jsonRecQueryTest (json)" $ jsonRecQueryTest runQueryJSON_
  , TestLabel "jsonRecQueryTest (compact)" $
      jsonRecQueryTest runQueryCompact_
  , TestLabel "limitTest (bin)" $ limitTest runQuery id
  , TestLabel "limitTest (json)" $ limitTest runQuery recursive
  , TestLabel "jsonPrefixQueryTest" jsonPrefixQueryTest
  , TestLabel "jsonPrefixQueryEachTest" $
      jsonPrefixQueryEachTest id
  , TestLabel "jsonDerived" $ jsonDerivedTest id
  , TestLabel "keyValue" $ keyValueTest id
  ]
