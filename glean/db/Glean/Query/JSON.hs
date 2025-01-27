{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.JSON
  ( factToJSON
  , factToCompact
  ) where

import Control.Exception
import Control.Monad
import Control.Monad.ST (ST, stToIO)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LBS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word8, Word64)
import Foreign.Marshal.Utils (copyBytes)
import Safe (atMay)

import Thrift.Protocol.JSON.Base64
import Util.Buffer (ascii, liftST)
import qualified Util.Buffer as Buffer

import Glean.RTS as RTS
import qualified Glean.RTS.Foreign.JSON as RTS hiding (String, Array)
import qualified Glean.RTS.Foreign.Thrift as Thrift
import Glean.RTS.Types (Type, ExpandedType(..))
import Glean.Database.Schema
import Glean.Angle.Types as Schema hiding (Type)

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

    -- | Start a set
  , encSetBegin :: Type -> Int -> Enc

    -- | Separate two set elements
  , encSetSep :: Enc

    -- | Finish a set
  , encSetEnd :: Enc

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
      SetTy elty -> do
        size <- liftST $ RTS.dSet d
        case elty of
          ByteTy{} -> do
            ref <- liftST $ RTS.dByteStringRef d size
            encBinary ref
          _ -> do
            encSetBegin elty $ fromIntegral size
            case size of
              0 -> return ()
              1 -> enc elty
              _ -> do
                enc elty
                let go 0 = return ()
                    go n = do
                      encSetSep
                      enc elty
                      go (n-1)
                go (size-1)
            encSetEnd
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
      TyVar{} -> error "JSON.encode: TyVar"
      HasTy{} -> error "JSON.encode: HasTy"
      HasKey{} -> error "JSON.encode: HasKey"
      ElementsOf{} -> error "JSON.encode: ElementsOf"

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
  -- Sets are encoded as ordered arrays.
  , encSetBegin = \_ _ -> ascii '['
  , encSetSep = ascii ','
  , encSetEnd = ascii ']'
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
-- See https://github.com/apache/thrift/blob/master/doc/specs/thrift-compact-protocol.md#struct-encoding
thriftType :: Type -> ThriftType
thriftType ByteTy{} = 3
thriftType NatTy{} = 6
thriftType StringTy{} = 8
thriftType (ArrayTy ByteTy{}) = 8
thriftType ArrayTy{} = 9
thriftType (SetTy ByteTy{}) = 8
thriftType SetTy{} = 9
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
  , encArrayBegin = encArray
  , encArraySep = return ()
  , encArrayEnd = return ()
  , encSetBegin = encArray
  , encSetSep = return ()
  , encSetEnd = return ()
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
    encArray typ n = do
      -- array types
      let !ety = thriftType typ
      if n < 15
        -- small lengths are folded into the header byte
        then Buffer.byte $ ((fromIntegral n .&. 0x0F) `shiftL` 4) .|. ety
        else do
          Buffer.byte $ 0xF0 .|. ety
          Thrift.encodeVarint $ fromIntegral n
