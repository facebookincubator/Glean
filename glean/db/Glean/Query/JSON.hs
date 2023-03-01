{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.JSON
  ( factToJSON
  , factToCompact

  -- * Legacy code
  , OrderedValue(..)
  , runFactsToJSON
  , queryToJSON
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.ST (ST, stToIO)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson
import Data.Aeson (toJSON, toEncoding)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.String
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word (Word8, Word64)
import Foreign.Marshal.Utils (copyBytes)
import Safe (atMay)

import Thrift.Protocol.JSON.Base64
import Util.Buffer (ascii, liftST)
import qualified Util.Buffer as Buffer

import Glean.Display
import Glean.Query.Nested.Types
import Glean.RTS as RTS
import qualified Glean.RTS.Foreign.JSON as RTS hiding (String, Array)
import qualified Glean.RTS.Foreign.Thrift as Thrift
import Glean.RTS.Types (Type, ExpandedType(..))
import qualified Glean.RTS.Term as RTS
import Glean.Database.Schema
import qualified Glean.Types as Thrift
import Glean.Angle.Types as Schema hiding (Type)
import Glean.Schema.Util

newtype EncodingError = EncodingError String
  deriving(Show)

instance Exception EncodingError

encodingError :: String -> ST s a
encodingError = unsafeIOToST . throwIO . EncodingError

-- | Convert a fact to JSON
factToJSON
  :: Bool
  -> IntMap ByteString
  -> PredicateDetails
  -> Fid
  -> ByteString
  -> ByteString
  -> IO ByteString
factToJSON no_base64 expanded pred fid key value =
  encodeFact expanded pred fid key value (jsonEncoder no_base64)

-- | Convert a fact to Thrift Compact
factToCompact
  :: IntMap ByteString
  -> PredicateDetails
  -> Fid
  -> ByteString
  -> ByteString
  -> IO ByteString
factToCompact expanded pred fid key value =
  encodeFact expanded pred fid key value compactEncoder

type Enc = forall s . Buffer.Fill s ()

-- | Specifies how facts should be encoded
data Encoder = Encoder
  { -- | Bytes
    encByte :: Word8 -> Enc

    -- | Numbers
  , encNat :: Word64 -> Enc

    -- | Booleans
  , encBool :: Bool -> Enc

    -- | Mangled RTS strings
  , encMangledString :: RTS.MangledStringRef -> Int -> Enc

    -- | Binary
  , encBinary :: RTS.ByteStringRef -> Enc

    -- | Start an array
  , encArrayBegin :: Type -> Int -> Enc

    -- | Separate two array elements
  , encArraySep :: Enc

    -- | Finish an array
  , encArrayEnd :: Enc

    -- | Start an object
  , encObjectBegin :: Enc

    -- | Separate two object fields
  , encObjectSep :: Enc

    -- | Object field name/number - Thrift folds boolean values into the field
    -- header so pass in the 'Decoder' and an action to decode and write the
    -- value into the stream and let the encoder decide what to do with it.
  , encObjectField
      :: forall s.
      Maybe Int  -- previous field id (if any)
      -> Int  -- current field id
      -> Text  -- field name
      -> Type  -- field type
      -> Decoder s  -- Thrift folds bool fields into the header so pass in
      -> Buffer.Fill s ()  -- a Decoder and an action to write the field value

      -> Buffer.Fill s ()

    -- | Finish an object
  , encObjectEnd :: Enc
  }

-- | Encode a fact with the given 'Encoder'
encodeFact
  :: IntMap ByteString
  -> PredicateDetails
  -> Fid
  -> ByteString
  -> ByteString
  -> Encoder
  -> IO ByteString
{-# INLINE encodeFact #-}
encodeFact
  expanded
  PredicateDetails{..}
  fid
  key
  value
  encoder@Encoder{..} = stToIO $
    RTS.withDecoder key $ \dkey ->
    RTS.withDecoder value $ \dvalue ->
    Buffer.fillByteString 8 $ do
      -- NOTE: We hardcode the field numbers for id, key and value here.
      encObjectBegin
      encObjectField Nothing 1 "id" NatTy dkey
        $ encNat $ fromIntegral $ fromFid fid
      encObjectSep
      encObjectField (Just 1) 2 "key" predicateKeyType dkey
        $ encode expanded encoder dkey predicateKeyType
      case predicateValueType of
        Schema.RecordTy [] -> return () -- omit "value" field if value is unit
        _other -> do
          encObjectSep
          encObjectField (Just 2) 3 "value"
              predicateValueType dvalue
            $ encode expanded encoder dvalue predicateValueType
      encObjectEnd

-- | Takes a Glean schema type and a value of that type, and fills a 'Buffer'
-- with a blob that can be decoded into the appropriate native Client Type
-- corresponding to the generated Thrift schema. The encoding (currently only
-- JSON) is specified by the 'Encoder'.
--
-- NOTE: This is INLINE because we want a specialised version for each encoding
-- that we support.
encode
  :: IntMap ByteString
  -> Encoder
  -> RTS.Decoder s
  -> Type
  -> Buffer.Fill s ()
{-# INLINE encode #-}
encode expanded Encoder{..} !d = enc
  where
    enc typ = case typ of
      ByteTy -> do
        x <- liftST $ RTS.dByte d
        encByte x
      NatTy{} -> do
        x <- liftST $ RTS.dNat d
        encNat x
      StringTy{} -> do
        (x,n) <- liftST $ RTS.dTrustedStringRef d
        encMangledString x $ fromIntegral n
      ArrayTy elty -> do
        size <- liftST $ RTS.dArray d
        case elty of
          ByteTy{} -> do
            ref <- liftST $ RTS.dByteStringRef d size
            encBinary ref
          _ -> do
            encArrayBegin elty $ fromIntegral size
            case size of
              0 -> return ()
              1 -> enc elty
              _ -> do
                enc elty
                let go 0 = return ()
                    go n = do
                      encArraySep
                      enc elty
                      go (n-1)
                go (size-1)
            encArrayEnd
      RecordTy fieldTys -> do
        encObjectBegin
        -- We need to carry both the current and previous field numbers as
        -- Thrift encodes deltas between those.
        let field prev (i, FieldDef name ety) = case ety of
              MaybeTy ety -> do
                sel <- liftST $ RTS.dSelector d
                if sel == 0
                  then return prev
                  else enc_field prev i name ety d
              ty -> enc_field prev i name ty d
        foldM_ field Nothing $ zip [1..] fieldTys
        encObjectEnd
      SumTy fieldTys -> do
        sel <- liftST $ RTS.dSelector d
        encObjectBegin
        void $ case atMay fieldTys (fromIntegral sel) of
          Just (FieldDef name ty) -> do
            enc_field Nothing (fromIntegral sel + 1) name ty d
          Nothing ->
            enc_field Nothing (length fieldTys + 1) "UNKNOWN" (RecordTy []) d
        encObjectEnd
      NamedTy (ExpandedType _ ty) -> enc ty
      EnumeratedTy _ -> do
        x <- liftST $ RTS.dSelector d
        encNat $ fromIntegral x
      MaybeTy{} -> liftST $ encodingError "unsupported Maybe"
      BooleanTy{} -> do
        x <- liftST $ RTS.dSelector d
        encBool (x /= 0)
      PredicateTy{} -> do
        fid <- liftST $ RTS.dFact d
        let id = fromIntegral (fromFid fid)
        case IntMap.lookup id expanded of
          Just bs ->  Buffer.byteString bs
          Nothing -> do
            encObjectBegin
            encObjectField Nothing 1 "id" NatTy d
              $ encNat $ fromIntegral id
            encObjectEnd

    {-# INLINE enc_field #-}
    enc_field prev i name ty d = do
      when (isJust prev) encObjectSep
      encObjectField prev i name ty d $ enc ty
      return $ Just i

jsonEncoder :: Bool -> Encoder
jsonEncoder no_base64 = Encoder
  { encByte = number . fromIntegral
  , encNat = number
  , encBool = \b -> if b
      then do
        ascii 't'
        ascii 'r'
        ascii 'u'
        ascii 'e'
      else do
        ascii 'f'
        ascii 'a'
        ascii 'l'
        ascii 's'
        ascii 'e'
  , encMangledString = \ref _ -> do
      n <- liftST $ RTS.getMangledStringEscapedSize ref
      do
        ascii '\"'
        Buffer.alloc n $ \p -> do
          RTS.escapeMangledString ref p n
          return n
        ascii '\"'
  , encBinary = \ !ref ->
      if no_base64
        then do
          n <- liftST $ RTS.getStringEscapedSize ref
          do
            ascii '\"'
            Buffer.alloc n $ \p -> do
              RTS.escapeString ref p n
              return n
            ascii '\"'
        else do
          -- TODO: can be optimised if necessary
          bytes <- liftST $ unsafeIOToST $ RTS.derefByteString ref
          ascii '\"'
          Buffer.byteString
            $ LBS.toStrict $ Builder.toLazyByteString $ encodeBase64 bytes
          ascii '\"'
  , encArrayBegin = \_ _ -> ascii '['
  , encArraySep = ascii ','
  , encArrayEnd = ascii ']'
  , encObjectBegin = ascii '{'
  , encObjectSep = ascii ','
  , encObjectField = \_ _ !name _ _ enc -> do
      ascii '\"'
      Buffer.byteString (Text.encodeUtf8 name)
      ascii '\"'
      ascii ':'
      enc
  , encObjectEnd = ascii '}'
  }
  where
    number :: Word64 -> Enc
    number !n = Buffer.alloc 21 $ RTS.encodeNumber $ fromIntegral n
      -- max length of 64 bit number is sign + 20 digits

type ThriftType = Word8

-- | Compute the Thrift type for a 'Type'
thriftType :: Type -> ThriftType
thriftType ByteTy{} = 3
thriftType NatTy{} = 6
thriftType StringTy{} = 8
thriftType (ArrayTy ByteTy{}) = 8
thriftType ArrayTy{} = 9
thriftType RecordTy{} = 12
thriftType SumTy{} = 12
thriftType (NamedTy (ExpandedType _ ty)) = thriftType ty
thriftType EnumeratedTy{} = 5 -- spec says enums are i32
thriftType BooleanTy{} = 1
thriftType PredicateTy{} = 12
thriftType typ =
  throw $ EncodingError $ "thrifTType: invalid type " ++ show typ


compactEncoder :: Encoder
compactEncoder = Encoder
  { encByte = Buffer.byte
  , encNat = Thrift.encodeZigZag . fromIntegral
  , encBool = Buffer.byte . bool
  , encMangledString = \ref n -> do
      -- length
      Thrift.encodeVarint (fromIntegral n)
      -- bytes
      Buffer.alloc (fromIntegral n) $ \p -> do
        m <- unsafeIOToST (RTS.demangle ref p)
        assert (m == fromIntegral n) $ return (fromIntegral n)
  , encBinary = \(RTS.ByteStringRef s n) -> do
      -- length
      Thrift.encodeVarint (fromIntegral n)
      -- bytes
      Buffer.alloc (fromIntegral n) $ \p -> do
        -- s might be NULL if the range is empty so check for that
        when (n /= 0) $ unsafeIOToST $ copyBytes p s $ fromIntegral n
        return (fromIntegral n)
  , encArrayBegin = \typ n -> do
      -- array types
      let !ety = thriftType typ
      if n < 15
        -- small lengths are folded into the header byte
        then Buffer.byte $ ((fromIntegral n .&. 0x0F) `shiftL` 4) .|. ety
        else do
          Buffer.byte $ 0xF0 .|. ety
          Thrift.encodeVarint $ fromIntegral n
  , encArraySep = return ()
  , encArrayEnd = return ()
  , encObjectBegin = return ()
  , encObjectSep = return ()
  , encObjectField = \prev i _ typ d enc -> do
      -- field types - different from array types
      !ety <- case thriftType typ of
        1 -> do
          -- bool values folded into the header
          sel <- liftST $ dSelector d
          return $ bool $ sel /= 0
        ety -> return ety
      let delta = i - fromMaybe 0 prev
      if delta /= 0 && delta <= 15
        -- small field number deltas folded into the field header
        then Buffer.byte $ ((fromIntegral delta .&. 0x0F) `shiftL` 4) .|. ety
        else do
          -- field number delta 0 indicates that the real field number follows
          Buffer.byte ety
          Thrift.encodeZigZag $ fromIntegral i
      when (ety > 2) enc
    -- object terminator
  , encObjectEnd = Buffer.byte 0
  }
  where
    bool True = 1
    bool False = 2

-----------------
-- Legacy code --
-----------------

-- -----------------------------------------------------------------------------
-- Converting Glean Terms to JSON-encoded Thrift

-- The order of fields is important in the JSON we return, since we
-- want the JSON to match the schema. Aeson doesn't preserve field
-- ordering because it uses HashMap to represent objects, so we need
-- to encode into our own JSON type and define custom encoding for it.
data OrderedValue
  = OrderedObject [(Text,OrderedValue)]
  | OrderedArray (Vector OrderedValue)
  | OrderedValue Aeson.Value

instance Show OrderedValue where
  show = LBC.unpack . Aeson.encode

instance Aeson.ToJSON OrderedValue where
  toJSON (OrderedObject fields) =
    Aeson.object [ (f, toJSON v) | (f,v) <- fields ]
  toJSON (OrderedArray arr) = Aeson.Array (Vector.map toJSON arr)
  toJSON (OrderedValue val) = toJSON val

  -- toEncoding preserves field order, but toJSON can't.
  toEncoding (OrderedObject fields) =
    Aeson.dict Aeson.text toEncoding (foldr . uncurry) fields
  toEncoding (OrderedArray arr) = Aeson.list toEncoding (Vector.toList arr)
  toEncoding (OrderedValue val) = toEncoding val

type FactsToJSON = Except String

runFactsToJSON :: FactsToJSON a -> Either String a
runFactsToJSON = runExcept

matchToJSON
  :: DbSchema
  -> Thrift.UserQueryOptions
  -> Type
  -> RTS.Match (Nested Fid)
  -> FactsToJSON OrderedValue
matchToJSON env opts typ term = case (typ,term) of
  (PredicateTy{}, RTS.Wildcard) -> return (OrderedObject [])
  (_, RTS.Wildcard) -> return $ OrderedValue Aeson.Null
  (SumTy fieldTys, RTS.MatchTerm (NestedSum mode fields)) -> do
    flds <- forM (zip fieldTys fields) $ \(FieldDef name ty, term) -> do
      jsonField <- mapM (queryToJSON env opts ty) term
      return $ (name,) <$> jsonField
    let addAny | SumMatchAny <- mode =
                  (("any", OrderedValue $ Aeson.Bool True) :)
               | otherwise = id
    return $ OrderedObject $ addAny $ catMaybes flds
  (ArrayTy ty, RTS.MatchTerm (NestedArray term)) -> do
    term' <- queryToJSON env opts ty term
    return $ OrderedObject [ ("every", term') ]
  (PredicateTy{}, RTS.MatchTerm (NestedRef (Fid id))) ->
    return $ OrderedObject
      [ ("id", OrderedValue $ Aeson.Number (fromIntegral id)) ]
  (PredicateTy{}, RTS.MatchTerm (NestedPred details mids mterm)) -> do
    let
      idsField = case mids of
        Nothing -> []
        Just ids -> [ ("ids", OrderedArray (Vector.fromList (map doId ids))) ]
          where doId (Fid fid) =
                  OrderedValue $ Aeson.Number (fromIntegral fid)
    case mterm of
      Nothing -> return $ OrderedObject idsField
      Just term -> do
        let !PredicateDetails{..} = details
        json <- queryToJSON env opts predicateKeyType term
        return $ OrderedObject $ ("key", json) : idsField
  (StringTy{}, RTS.PrefixWildcard s) -> return $
    OrderedObject [("prefix", OrderedValue $ Aeson.String (Text.decodeUtf8 s))]
  (_,_) ->
   throwError $ "queryToJSON: "
     ++ renderString (layoutCompact (displayVerbose term))
     ++ " :: " ++ show typ

-- | Takes a Glean schema type and a pattern of that type, and returns a
-- JSON blob that can be decoded into the appropriate native Client Type
-- corresponding to the generated Thrift schema.
queryToJSON
  :: DbSchema
  -> Thrift.UserQueryOptions
  -> Type
  -> RTS.Term (RTS.Match (Nested Fid))
  -> FactsToJSON OrderedValue
queryToJSON env opts@Thrift.UserQueryOptions{..} typ t = case (typ,t) of
  (_, RTS.Byte w) -> return $ OrderedValue $ Aeson.Number (fromIntegral w)
  (_, RTS.Nat w) -> return $ OrderedValue $ Aeson.Number (fromIntegral w)
  (_, RTS.String s) -> return $ OrderedValue $ Aeson.String $ Text.decodeUtf8 s
  (ArrayTy ByteTy, RTS.ByteArray bs)
    | userQueryOptions_no_base64_binary ->
      return $ OrderedValue $ Aeson.String (Text.decodeUtf8 bs)
    | otherwise ->
      return $ OrderedValue $ Aeson.String (encodeBase64Text bs)
  (ArrayTy ty, RTS.Array vals) ->
     OrderedArray . Vector.fromList <$> mapM (queryToJSON env opts ty) vals
  (RecordTy fieldTys, RTS.Tuple terms) -> fmap OrderedObject
    $ forM (zip fieldTys terms) $ \(FieldDef name ty, term) -> do
      json <- queryToJSON env opts ty term
      return (name, json)
  (SumTy fieldTys, RTS.Alt n term)
    | Just (FieldDef name ty) <- atMay fieldTys (fromIntegral n) -> do
    json <- queryToJSON env opts ty term
    return $ OrderedObject [(name,json)]
  (NamedTy (ExpandedType _ ty), term) ->
    queryToJSON env opts ty term
  (EnumeratedTy _, RTS.Alt n _) ->
    return $ OrderedValue $ Aeson.Number (fromIntegral n)
  (MaybeTy ty, term) ->
    queryToJSON env opts (lowerMaybe ty) term
  (BooleanTy{}, RTS.Alt 0 _) -> return $ OrderedValue $ Aeson.Bool False
  (BooleanTy{}, RTS.Alt 1 _) -> return $ OrderedValue $ Aeson.Bool True
  (_, RTS.Ref ref) -> matchToJSON env opts typ ref
  (typ, term) ->
     throwError $ "queryToJSON: "
       ++ renderString (layoutCompact (displayVerbose term))
       ++ " :: " ++ show typ
