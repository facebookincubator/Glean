{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module EncodingTest (main) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Char8 as ByteString
import Data.Default
import Data.IntMap.Strict (IntMap)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Test.HUnit

import TestRunner
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON
import Util.FFI

import Glean.Database.Config
import Glean.FFI (unsafeMallocedByteString, unsafeWithBytes)
import Glean.Init
import Glean.Query.JSON
import Glean.RTS
import Glean.RTS.Builder
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.Typed.Binary
import Glean.Types
import Glean.Schema.GleanTest.Types as Glean.Test
import Glean.Schema.Sys.Types (Blob(..))
import qualified Glean.Angle.Types as Schema

kitchenSink :: KitchenSink
kitchenSink = KitchenSink
  { kitchenSink_byt = Byte 42
  , kitchenSink_nat = Nat 70000
  , kitchenSink_bool_ = True
  , kitchenSink_string_ = "Hello, world!"
  , kitchenSink_pred = Blob 256789 Nothing
  , kitchenSink_maybe_ = Nothing
  , kitchenSink_record_ = def
  , kitchenSink_sum_ = KitchenSink_sum__c def
  , kitchenSink_enum_ = def
  , kitchenSink_named_record_ = Rec
      { rec_alpha = Enum__green
      , rec_beta = Sum_mon $ Byte 5
      }
  , kitchenSink_named_sum_ = Sum_wed False
  , kitchenSink_named_enum_ = Enum___UNKNOWN 10
  , kitchenSink_array_of_byte =
      BS.pack [0,5,127,minBound,maxBound]
  , kitchenSink_array_of_nat = Nat <$>
      [0,0xF6,0x12345,0x65434567,minBound,maxBound]
  , kitchenSink_array_of_bool = [True,False,True]
  , kitchenSink_array_of_string =
      ["abcd","","lzdkfhlsadhgl","asfdfs\0fasasfda\0faaf", "𐀀" ]
  , kitchenSink_array_of_pred =
      [ Glean.Test.Predicate 5432 Nothing
      , Glean.Test.Predicate 0x1234567 Nothing ]
  , kitchenSink_array_of_named_record =
      [ Rec Enum__red $ Sum_mon $ Byte 42
      , Rec Enum__green $ Sum_tue $ Nat 1234556
      , Rec Enum__blue $ Sum_wed False ]
  , kitchenSink_array_of_named_sum =
      [ Sum_mon $ Byte 65
      , Sum_tue $ Nat 32453
      , Sum_wed True ]
  , kitchenSink_array_of_named_enum =
      [ Enum__blue, Enum__green, Enum__red ]
  , kitchenSink_array2_of_byte =
      [ BS.pack [6,132,3,0]
      , BS.empty
      , BS.pack [89, 143] ]
  , kitchenSink_array2_of_nat = fmap Nat <$>
      [ []
      , [733, 323986598, 2364, 282383]
      , [0,0,0,0] ]
  , kitchenSink_array2_of_bool =
      [ [ True ]
      , [ False ] ]
  , kitchenSink_array2_of_string =
      [ [ "a", "bc", "def" ] ]
  }

data E = E
  { eTestPredicate :: PredicateDetails
  }

mkE :: IO E
mkE = do
  -- Build the DbSchema
  schema <- parseSchemaDir schemaSourceDir
  dbSchema <- newDbSchema Nothing schema LatestSchemaAll readWriteContent def

  -- glean.test.Predicate has glean.test.KitchenSink as its key
  Right testPred <- return $ lookupPredicateSourceRef
    (Schema.SourceRef "glean.test.Predicate" Nothing)
    LatestSchemaAll
    dbSchema

  return $ E
    { eTestPredicate = testPred
    }

data Enc = Enc
  { encFact
      :: IntMap ByteString
      -> PredicateDetails
      -> Fid
      -> ByteString
      -> ByteString
      -> IO ByteString
  , encReencode :: Maybe (ByteString -> IO ByteString)
  , encSerialize :: Glean.Test.KitchenSink -> ByteString
  , encDeserialize :: ByteString -> Either String Glean.Test.Predicate
  , encPrint :: ByteString -> IO ()
  }

roundTrip :: Enc -> E -> Glean.Test.KitchenSink -> Test
roundTrip Enc{..} E{..} val = TestCase $ do
  putStrLn "original:"
  print val
  putStrLn "Thrift serialised:"
  encPrint $ encSerialize val

  -- Build the RTS value
  term <- withBuilder $ \builder -> do
    buildRtsValue builder val
    finishBuilder builder

  -- Encode Term
  encoded <- encFact
    mempty
    eTestPredicate
    (Fid 10000)
    term
    mempty

  putStrLn "encoded:"
  encPrint encoded

  -- Deserialize encoded term to native Client type
  Right Glean.Test.Predicate
    { predicate_key = Just newval@Glean.Test.KitchenSink{} }
      <- return $ encDeserialize encoded
  print newval

  assertEqual "roundTrip" val newval

  case encReencode of
    Nothing -> return ()
    Just reencode -> do
      reencoded <- reencode encoded
      putStrLn "reencoded:"
      encPrint reencoded
      assertEqual "reencoded" reencoded encoded

jsonEnc :: Enc
jsonEnc = Enc
  { encFact = factToJSON False
  , encReencode = Nothing
  , encSerialize = serializeJSON
  , encDeserialize = deserializeJSON
  , encPrint = ByteString.putStrLn
  }

compactEnc :: Enc
compactEnc = Enc
  { encFact = factToCompact
  , encReencode = Just $ \encoded -> unsafeWithBytes encoded $ \ptr size -> do
      (p,n) <- invoke $ glean_test_compact_reencode ptr size
      unsafeMallocedByteString p n
  , encSerialize = serializeCompact
  , encDeserialize = deserializeCompact
  , encPrint = ByteString.putStrLn . Hex.encode
  }

foreign import ccall unsafe glean_test_compact_reencode
  :: Ptr () -> CSize -> Ptr (Ptr ()) -> Ptr CSize -> IO CString

binaryRoundTrip :: (Type a, Eq a, Show a) => a -> Test
binaryRoundTrip val = TestCase $ do
  putStrLn "original:"
  print val

  term <- withBuilder $ \builder -> do
    buildRtsValue builder val
    finishBuilder builder

  decoded <- decodeRts term
  assertEqual "binaryEncoding" val decoded

main :: IO ()
main = withUnitTest $ do
  e <- mkE
  testRunner $ TestList
    [ TestLabel "roundTrip JSON" $ roundTrip jsonEnc e kitchenSink
    , TestLabel "roundTrip compact" $ roundTrip compactEnc e kitchenSink
    , TestLabel "roundTrip binary" $ binaryRoundTrip kitchenSink
    ]
