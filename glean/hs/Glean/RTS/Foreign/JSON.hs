module Glean.RTS.Foreign.JSON
  ( Dyn, Value(..), withParsed, unfold
  , ByteStringRef(..), getUTF8
  , ArrayRef, size, index, toList
  , ObjectRef, arity, field
  , JSONError(..)
  , encode
  , encodeNumber
  , getStringEscapedSize, escapeString
  , getMangledStringEscapedSize, escapeMangledString
  )
where

import Control.Exception
import Control.Monad
import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import qualified Util.Buffer as Buffer

import Glean.FFI
import Glean.RTS (ByteStringRef(..), MangledStringRef(..))

-- | Type of JSON exceptions
newtype JSONError = JSONError String
  deriving(Show)

instance Exception JSONError

-- | A dynamic JSON value which can be inspected via 'unfold'.
newtype Dyn = Dyn (Ptr Dyn)

newtype Document = Document (Ptr Document)
  deriving(Storable)

instance Static Document where
  destroyStatic = glean_json_document_free

-- | Parse a 'ByteString' containing JSON and invoke the function with the
-- resulting 'Value'.
--
-- WARNING: The 'Value' and any parts of it obtained via 'unfold' are only
-- value within the function and will be deallocated at the end.
withParsed :: ByteString -> (Value -> IO a) -> IO a
withParsed s f =
  handle (throwIO . JSONError . ffiErrorMessage) $
  unsafeWithBytes s $ \bytes size ->
  using (invoke $ glean_json_parse (castPtr bytes) size) $
  f <=< unfold <=< glean_json_document_root

-- | Reference to a JSON array.
data ArrayRef =
  ArrayRef
    {-# UNPACK #-} !Dyn
    {-# UNPACK #-} !CSize

-- | Reference to a JSON object.
data ObjectRef =
  ObjectRef
    {-# UNPACK #-} !Dyn
    {-# UNPACK #-} !CSize

-- | Decoded JSON value.
data Value
  = Null
  | Bool !Bool
  | Int {-# UNPACK #-} !Int64
  | Double {-# UNPACK #-} !Double
  | String {-# UNPACK #-} !ByteStringRef
  | Array {-# UNPACK #-} !ArrayRef
  | Object {-# UNPACK #-} !ObjectRef

data Type
  = NULL
  | ARRAY
  | BOOL
  | DOUBLE
  | INT64
  | OBJECT
  | STRING
  deriving(Eq,Ord,Enum,Bounded,Show)

-- | Unfold one level of the JSON value.
unfold :: Dyn -> IO Value
unfold dyn = do
  ty <- glean_json_value_type dyn
  case toEnum ty of
    NULL -> return Null
    BOOL -> do
      b <- glean_json_value_get_bool dyn
      return $ Bool $ b /= 0
    INT64 -> Int <$> glean_json_value_get_int dyn
    DOUBLE -> throwIO $
      JSONError "JSON floating point numbers aren't supported by Glean"
    STRING -> do
      (chars,size) <- invoke $ glean_json_value_get_string dyn
      return $ String $ ByteStringRef chars size
    ARRAY -> Array . ArrayRef dyn <$> glean_json_value_get_size dyn
    OBJECT -> Object . ObjectRef dyn <$> glean_json_value_get_size dyn

getUTF8 :: ByteStringRef -> IO Text
getUTF8 (ByteStringRef p n) = fromUTF8 (castPtr p) n

-- | Array length
size :: ArrayRef -> Int
size (ArrayRef _ n) = fromIntegral n

-- | Array indexing
index :: ArrayRef -> Int -> IO Value
index (ArrayRef dyn n) i
  | j >= n = throwIO $ JSONError "JSON.index out of bounds"
  | otherwise =
      unfold =<< glean_json_value_get_array_element dyn j
  where
    j = fromIntegral i

-- | Obtain all elements of the array
toList :: ArrayRef -> IO [Value]
toList (ArrayRef dyn n)
  | n == 0 = return []
  | otherwise =
      mapM (unfold <=< glean_json_value_get_array_element dyn) [0 .. n-1]

-- | Number of fiels in an object
arity :: ObjectRef -> Int
arity (ObjectRef _ n) = fromIntegral n

-- | Field lookup by name
field :: ObjectRef -> ByteString -> IO (Maybe Value)
field (ObjectRef dyn _) key =
  unsafeWithBytes key $ \bytes size -> do
    p <- glean_json_value_get_object_field dyn (castPtr bytes) size
    if p == nullPtr
      then return Nothing
      else Just <$> unfold (Dyn p)

-- | Encode a 'Value' to JSON
--
-- FIXME: This is a truly terrible hack.
encode :: Value -> IO ByteString
encode Null = return "null"
encode (Bool False) = return "false"
encode (Bool True) = return "true"
encode (Int n) = return $ Char8.pack $ show n
encode (Double x) = return $ Char8.pack $ show x
encode (String ref) = stToIO $ Buffer.fillByteString 1 $ do
  n <- Buffer.liftST $ getStringEscapedSize ref
  Buffer.ascii '\"'
  Buffer.alloc n $ \p -> do
    escapeString ref p n
    return n
  Buffer.ascii '\"'
encode (Array (ArrayRef dyn _)) = encodeDyn dyn
encode (Object (ObjectRef dyn _)) = encodeDyn dyn

encodeDyn :: Dyn -> IO ByteString
encodeDyn dyn = do
  (s,n) <- invoke $ glean_json_encode dyn
  unsafeMallocedByteString (castPtr s) n

-- | Convert a number to its JSON representation. The output buffer must have
-- enough space (at most 21 bytes for a signed 64 bit number).
encodeNumber :: Int64 -> Ptr Word8 -> ST s Int
encodeNumber w p = fromIntegral <$> unsafeIOToST (glean_json_encode_number w p)

-- | Given a reference to a UTF8 string, compute the size of its JSON-escaped
-- representation.
getStringEscapedSize :: ByteStringRef -> ST s Int
getStringEscapedSize (ByteStringRef p n) =
  fromIntegral
  <$> unsafeIOToST (glean_json_string_escaped_size p n)

-- | Escape a UTF8 string for JSON.
escapeString
  :: ByteStringRef -- ^ string
  -> Ptr Word8 -- ^ output buffer which must have enough space
  -> Int -- ^ size of the escaped representation computed by
         -- 'getStringEscapedSize'
  -> ST s ()
escapeString (ByteStringRef p n) o k =
  unsafeIOToST (glean_json_string_escape p n o (fromIntegral k))

-- | Given a reference to an RTS-mangled string, compute the size of its
-- JSON-escaped representation.
getMangledStringEscapedSize :: MangledStringRef -> ST s Int
getMangledStringEscapedSize (MangledStringRef (ByteStringRef p n)) =
  fromIntegral
  <$> unsafeIOToST (glean_json_mangled_string_escaped_size p n)

-- | Escape an RTS-mangled string for JSON.
escapeMangledString
  :: MangledStringRef -- ^ string
  -> Ptr Word8 -- ^ output buffer which must have enough space
  -> Int -- ^ size of the escaped representation computed by
         -- 'getStringEscapedSize'
  -> ST s ()
escapeMangledString (MangledStringRef (ByteStringRef p n)) o k =
  unsafeIOToST (glean_json_mangled_string_escape p n o (fromIntegral k))

foreign import ccall safe glean_json_parse
  :: Ptr CChar -> CSize -> Ptr Document -> IO CString
foreign import ccall unsafe glean_json_document_free
  :: Document -> IO ()
foreign import ccall unsafe glean_json_document_root
  :: Document -> IO Dyn

foreign import ccall unsafe glean_json_value_type
  :: Dyn -> IO Int

foreign import ccall unsafe glean_json_value_get_int
  :: Dyn -> IO Int64
foreign import ccall unsafe glean_json_value_get_bool
  :: Dyn -> IO CInt
foreign import ccall unsafe glean_json_value_get_string
  :: Dyn -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()
foreign import ccall unsafe glean_json_value_get_size
  :: Dyn -> IO CSize
foreign import ccall unsafe glean_json_value_get_array_element
  :: Dyn -> CSize -> IO Dyn
foreign import ccall unsafe glean_json_value_get_object_field
  :: Dyn
  -> Ptr CChar
  -> CSize
  -> IO (Ptr Dyn)

foreign import ccall unsafe glean_json_encode
  :: Dyn -> Ptr (Ptr Word8) -> Ptr CSize -> IO CString

foreign import ccall unsafe glean_json_encode_number
  :: Int64
  -> Ptr Word8
  -> IO CSize

foreign import ccall unsafe glean_json_string_escaped_size
  :: Ptr Word8 -> CSize -> IO CSize

foreign import ccall unsafe glean_json_string_escape
  :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe glean_json_mangled_string_escaped_size
  :: Ptr Word8 -> CSize -> IO CSize

foreign import ccall unsafe glean_json_mangled_string_escape
  :: Ptr Word8 -> CSize -> Ptr Word8 -> CSize -> IO ()
