-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE NamedFieldPuns #-}
module Glean.RTS (
  Fid(..), lowestFid,
  Pid(..), lowestPid,

  withValue,
  toValue, fromValue,
  encodeByteArray,
  mangleString,

  ByteStringRef(..), unsafeWithByteStringRef, derefByteString,
  MangledStringRef(..), unsafeWithMangledStringRef, demangle,
  Decoder,  withDecoder,
  dByte, dNat,
  dTrustedStringRef, dString,
  dArray, dByteStringRef, dBytes,
  dSelector, dFact,

  glean_push_value_byte,
  glean_push_value_nat,
  glean_push_value_string,
  glean_push_value_array,
  glean_push_value_bytes,
  glean_push_value_selector,
  glean_push_value_fact,

  glean_push_fact,

  glean_pop_value_byte,
  glean_pop_value_nat,
  glean_pop_value_string,
  glean_pop_value_array,
  glean_pop_value_bytes,
  glean_pop_value_selector,
  glean_pop_value_fact,
) where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable

import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import System.IO.Unsafe (unsafePerformIO)

import qualified Glean.FFI as FFI
import Glean.RTS.Builder
import Glean.RTS.Types
import Glean.RTS.Term

withValue :: Value -> (Builder -> IO a) -> IO a
withValue v f = withBuilder $ \b -> do
  encodeValue b v
  f b

encodeValue :: Builder -> Value -> IO ()
encodeValue b (Byte n) = FFI.call $ glean_push_value_byte b n
encodeValue b (Nat n) = FFI.call $ glean_push_value_nat b n
encodeValue b (Array xs) = do
  FFI.call $ glean_push_value_array b $ fromIntegral $ length xs
  mapM_ (encodeValue b) xs
encodeValue b (ByteArray xs) = encodeByteArray b xs
encodeValue b (Tuple xs) = mapM_ (encodeValue b) xs
encodeValue b (Alt n x) = do
  FFI.call $ glean_push_value_selector b $ fromIntegral n
  encodeValue b x
encodeValue b (String s) = FFI.unsafeWithBytes s $ \p n ->
  FFI.call $ glean_push_value_string b (castPtr p) n
encodeValue b (Ref id) = FFI.call $ glean_push_value_fact b id

encodeByteArray :: Builder -> ByteString -> IO ()
encodeByteArray b xs =
  BS.unsafeUseAsCStringLen xs $ \(p,n) -> do
    FFI.call $ glean_push_value_array b (fromIntegral n)
    FFI.call $ glean_push_value_bytes b (castPtr p) (fromIntegral n)

toValue :: Rep Pid -> ByteString -> Value
toValue ty bs = runST $ withDecoder bs $ \d -> decodeValue d ty

fromValue :: Value -> ByteString
fromValue val = unsafePerformIO $ withValue val finishBuilder

-- | A reference to a range of bytes with a context-dependent lifetime.
data ByteStringRef = ByteStringRef
  { byteStringRefPtr :: {-# UNPACK #-} !(Ptr Word8)
  , byteStringRefSize :: {-# UNPACK #-} !CSize
  }

-- | Execute the action with a reference to the contents of the 'ByteString'.
unsafeWithByteStringRef :: ByteString -> (ByteStringRef -> IO a) -> IO a
unsafeWithByteStringRef s f =
  FFI.unsafeWithBytes s $ \p n -> f $ ByteStringRef (castPtr p) n

-- | Construct a 'ByteString' by copying a range of bytes.
derefByteString :: ByteStringRef -> IO ByteString
derefByteString (ByteStringRef p n) = FFI.copyByteString (castPtr p) n

-- | A reference to a range of bytes containing an RTS-mangled string.
newtype MangledStringRef = MangledStringRef
  { mangledBytes  :: ByteStringRef
      -- ^ Byte range including terminator.
  }

-- | Construct a 'MangledStringRef' from a 'ByteString' containing the mangled
-- bytes. The references can't be used outside the function.
unsafeWithMangledStringRef :: ByteString -> (MangledStringRef -> IO a) -> IO a
unsafeWithMangledStringRef s f =
  unsafeWithByteStringRef s $ f . MangledStringRef

-- | Yield the internal representation of a UTF-8 string.
mangleString :: ByteString -> ByteString
mangleString s = BS.intercalate "\0\1" (BS.split 0 s) <> "\0\0"

-- | A stateful 'Decoder' for binary values.
data Decoder s = Decoder
  { decoderPtr :: {-# UNPACK #-} !(Ptr (Ptr ()))
  , decoderEnd :: {-# UNPACK #-} !(Ptr ())
  , decoderBuf :: {-# UNPACK #-} !FFI.FFIResultBuf
  }

-- | Create a temporary 'Decoder' for a 'ByteString'.
withDecoder :: ByteString -> (Decoder s -> ST s a) -> ST s a
withDecoder bs f = unsafeIOToST $
  FFI.withFFIResultBuf $ \ !buf ->
  FFI.unsafeWithBytes bs $ \start len ->
  alloca $ \pstart -> do
    poke pstart start
    unsafeSTToIO $ f Decoder
      { decoderPtr = pstart
      , decoderEnd = start `plusPtr` fromIntegral len
      , decoderBuf = buf
      }

dffi
  :: Storable a
  => (Ptr (Ptr ()) -> Ptr () -> Ptr a -> IO CString) -> Decoder s -> ST s a
{-# INLINE dffi #-}
dffi f d =
  unsafeIOToST $ FFI.ffiBuf (decoderBuf d) $ f (decoderPtr d) (decoderEnd d)

dffi2
  :: (Storable a, Storable b)
  => (Ptr (Ptr ()) -> Ptr () -> Ptr a -> Ptr b -> IO CString)
  -> Decoder s
  -> ST s (a,b)
{-# INLINE dffi2 #-}
dffi2 f d =
  unsafeIOToST $ FFI.ffiBuf2 (decoderBuf d) $ f (decoderPtr d) (decoderEnd d)

-- | Obtain the next byte from the 'Decoder'.
dByte :: Decoder s -> ST s Word8
dByte = dffi glean_pop_value_byte

-- | Obtain a packed natural number from the 'Decoder'.
dNat :: Decoder s -> ST s Word64
dNat = dffi glean_pop_value_nat

-- | Obtain the length of an array from the 'Decoder'.
dArray :: Decoder s -> ST s CSize
dArray = dffi glean_pop_value_array

-- | Obtain a fixed number of bytes from the 'Decoder'. The 'ByteStringRef'
-- has the same lifetime as the 'Decoder'.
dByteStringRef :: Decoder s -> CSize -> ST s ByteStringRef
dByteStringRef d n = do
  p <- dffi (\s e -> glean_pop_value_bytes_ref s e n) d
  return $ ByteStringRef p n

-- | Obtain the given number of bytes from the 'Decoder'.
dBytes :: Decoder s -> CSize -> ST s ByteString
dBytes d n = do
  -- TODO: reimplement based on dByteStringRef
  p <- dffi (\s e -> glean_pop_value_bytes s e n) d
  unsafeIOToST $ FFI.unsafeMallocedByteString (castPtr p) n

-- | Obtain a selector from the 'Decoder'.
dSelector :: Decoder s -> ST s CSize
dSelector = dffi glean_pop_value_selector

-- | Obtain a reference to an RTS-mangled string from the 'Decoder'. The
-- reference has the same lifetime as the 'Decoder'.
dTrustedStringRef :: Decoder s -> ST s (MangledStringRef, CSize)
dTrustedStringRef Decoder{..} = unsafeIOToST $ do
  start <- peek decoderPtr
  n <- glean_pop_value_trusted_string_ref decoderPtr decoderEnd
  end <- peek decoderPtr
  return
    ( MangledStringRef
        { mangledBytes = ByteStringRef
          { byteStringRefPtr = castPtr start
          , byteStringRefSize = fromIntegral $ end `minusPtr` start
          }
        },
      n )


-- | Obtain a UTF8-encoded string from the 'Decoder'.
dString :: Decoder s -> ST s ByteString
dString d = do
  -- TODO: reimplement based on dStringRef
  (p,n) <- dffi2 glean_pop_value_string d
  unsafeIOToST $ FFI.unsafeMallocedByteString (castPtr p) n

-- | Obtain a fact id from the 'Decoder'.
dFact :: Decoder s -> ST s Fid
dFact = dffi glean_pop_value_fact

demangle :: MangledStringRef -> Ptr Word8 -> IO CSize
demangle (MangledStringRef (ByteStringRef p n)) q =
  glean_string_demangle_trusted p n q

-- | Decode a binary value. We allocate a single slot to hold results and reuse
-- it for every decoding step, which is a lot more efficient.
decodeValue :: Decoder s -> Rep Pid -> ST s (Term Fid)
decodeValue d ty = case ty of
  ByteRep -> Byte <$> dByte d
  NatRep -> Nat <$> dNat d
  ArrayRep elty -> do
    size <- dArray d
    case elty of
      ByteRep -> ByteArray <$> dBytes d size
      _ -> Array <$> replicateM (fromIntegral size) (decodeValue d elty)
  TupleRep tys -> Tuple <$> mapM (decodeValue d) tys
  SumRep tys -> do
    sel <- dSelector d
    Alt (fromIntegral sel) <$> decodeValue d (tys !! fromIntegral sel)
  StringRep -> String <$> dString d
  PredicateRep _ -> Ref <$> dFact d

foreign import ccall unsafe glean_push_value_byte
  :: Builder -> Word8 -> IO CString
foreign import ccall unsafe glean_push_value_nat
  :: Builder -> Word64 -> IO CString
foreign import ccall unsafe glean_push_value_array
  :: Builder -> CSize -> IO CString
foreign import ccall unsafe glean_push_value_bytes
  :: Builder -> Ptr Word8 -> CSize -> IO CString
foreign import ccall unsafe glean_push_value_selector
  :: Builder -> CSize -> IO CString
foreign import ccall unsafe glean_push_value_string
  :: Builder -> Ptr Word8 -> CSize -> IO CString
foreign import ccall unsafe glean_push_value_fact
  :: Builder -> Fid -> IO CString

foreign import ccall unsafe glean_pop_value_byte
  :: Ptr (Ptr ()) -> Ptr () -> Ptr Word8 -> IO CString
foreign import ccall unsafe glean_pop_value_nat
  :: Ptr (Ptr ()) -> Ptr () -> Ptr Word64 -> IO CString
foreign import ccall unsafe glean_pop_value_array
  :: Ptr (Ptr ()) -> Ptr () -> Ptr CSize -> IO CString
foreign import ccall unsafe glean_pop_value_bytes_ref
  :: Ptr (Ptr ()) -> Ptr () -> CSize -> Ptr (Ptr Word8) -> IO CString
foreign import ccall unsafe glean_pop_value_bytes
  :: Ptr (Ptr ()) -> Ptr () -> CSize -> Ptr (Ptr Word8) -> IO CString
foreign import ccall unsafe glean_pop_value_selector
  :: Ptr (Ptr ()) -> Ptr () -> Ptr CSize -> IO CString
foreign import ccall unsafe glean_pop_value_trusted_string_ref
  :: Ptr (Ptr ()) -> Ptr () -> IO CSize
foreign import ccall unsafe glean_pop_value_string
  :: Ptr (Ptr ()) -> Ptr () -> Ptr (Ptr Word8) -> Ptr CSize -> IO CString
foreign import ccall unsafe glean_pop_value_fact
  :: Ptr (Ptr ()) -> Ptr () -> Ptr Fid -> IO CString

foreign import ccall unsafe glean_push_fact
  :: Builder -> Pid -> Builder -> CSize -> IO CString

foreign import ccall unsafe glean_string_demangle_trusted
  :: Ptr Word8 -> CSize -> Ptr Word8 -> IO CSize
