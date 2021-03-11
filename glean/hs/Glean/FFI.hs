{-# LANGUAGE TypeOperators #-}
module Glean.FFI (
  GleanFFIError, ffiErrorMessage, call,
  List(..), FFIFun(..), Tuple(..), invoke,

  unsafeWithBytes, copyByteString, unsafeMallocedByteString,
  unsafeMallocedVector,
  withUTF8Text, fromUTF8, unsafeMallocedUTF8,

  usingMalloced, usingManyMalloced, withMany,

  Destroy, Object(..), create, release, construct, with,
  Constructor, constructor, Static(..), withStatic, using,

  FFIResultBuf, withFFIResultBuf, ffiBuf, ffiBuf2,
) where

import Foreign hiding (with, withMany)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import qualified Data.Vector.Storable

foreign import ccall unsafe "&glean_ffi_free_error" glean_ffi_free_error
  :: FunPtr (CString -> IO ())

newtype GleanFFIError = GleanFFIError (ForeignPtr CChar)

ffiErrorMessage :: GleanFFIError -> String
ffiErrorMessage (GleanFFIError fp) =
  unsafePerformIO $ withForeignPtr fp peekCString

instance Show GleanFFIError where
  show = ffiErrorMessage

instance Exception GleanFFIError

call :: IO CString -> IO ()
call f = do
  p <- f
  when (p /= nullPtr) $ do
    fp <- newForeignPtr glean_ffi_free_error p
    throwIO $ GleanFFIError fp

infixr 5 :>

data List xs where
  Nil :: List '[]
  (:>) :: x -> List xs -> List (x ': xs)

class Tuple xs where
  type Tup xs
  tuple :: List xs -> Tup xs

instance Tuple '[] where
  type Tup '[] = ()
  tuple Nil = ()

instance Tuple '[a] where
  type Tup '[a] = a
  tuple (a :> _) = a

instance Tuple [a,b] where
  type Tup [a,b] = (a,b)
  tuple (a :> b :> _) = (a,b)

instance Tuple [a,b,c] where
  type Tup [a,b,c] = (a,b,c)
  tuple (a :> b :> c :> _) = (a,b,c)

instance Tuple [a,b,c,d] where
  type Tup [a,b,c,d] = (a,b,c,d)
  tuple (a :> b :> c :> d :> _) = (a,b,c,d)

instance Tuple [a,b,c,d,e] where
  type Tup [a,b,c,d,e] = (a,b,c,d,e)
  tuple (a :> b :> c :> d :> e :> _) = (a,b,c,d,e)

instance Tuple [a,b,c,d,e,f] where
  type Tup [a,b,c,d,e,f] = (a,b,c,d,e,f)
  tuple (a :> b :> c :> d :> e :> f :> _) = (a,b,c,d,e,f)

instance Tuple [a,b,c,d,e,f,g] where
  type Tup [a,b,c,d,e,f,g] = (a,b,c,d,e,f,g)
  tuple (a :> b :> c :> d :> e :> f :> g :> _) = (a,b,c,d,e,f,g)

instance Tuple [a,b,c,d,e,f,g,h] where
  type Tup [a,b,c,d,e,f,g,h] = (a,b,c,d,e,f,g,h)
  tuple (a :> b :> c :> d :> e :> f :> g :> h :> _) = (a,b,c,d,e,f,g,h)

instance Tuple [a,b,c,d,e,f,g,h,i] where
  type Tup [a,b,c,d,e,f,g,h,i] = (a,b,c,d,e,f,g,h,i)
  tuple (a :> b :> c :> d :> e :> f :> g :> h :> i :> _) = (a,b,c,d,e,f,g,h,i)

class FFIFun f where
  type Res f :: [*]
  invoke' :: f -> IO (List (Res f))

instance FFIFun (IO CString) where
  type Res (IO CString) = '[]
  invoke' f = do
    call f
    return Nil

instance FFIFun (IO ()) where
  type Res (IO ()) = '[]
  invoke' f = do
    f
    return Nil

-- | For output parameter 'Ptr a' this will 'alloca' memory (unitialized)
-- and then proceed to the next parameter.  On returning this prepends
-- the result into x.  This is only safe if the invoked function
-- always fills in this output parameter pointer before returning.
instance (Storable a, FFIFun f) => FFIFun (Ptr a -> f) where
  type Res (Ptr a -> f) = a ': Res f
  invoke' f = alloca $ \p -> do
    xs <- invoke' (f p)
    x <- peek p
    return $ x :> xs

-- | This is only safe if the invoked function
-- always fills in output parameter pointers before returning.
invoke :: (FFIFun f, Tuple (Res f)) => f -> IO (Tup (Res f))
invoke = fmap tuple . invoke'

unsafeWithBytes :: ByteString -> (Ptr () -> CSize -> IO a) -> IO a
unsafeWithBytes s f = BS.unsafeUseAsCStringLen s $ \(p,n) ->
  f (castPtr p) (fromIntegral n)

copyByteString :: Ptr () -> CSize -> IO ByteString
copyByteString p n = BS.packCStringLen (castPtr p, fromIntegral n)

unsafeMallocedByteString :: Ptr () -> CSize -> IO ByteString
unsafeMallocedByteString p n =
  BS.unsafePackMallocCStringLen (castPtr p, fromIntegral n)

unsafeMallocedVector
  :: Storable a => Ptr a -> CSize -> IO (Data.Vector.Storable.Vector a)
unsafeMallocedVector p n = do
  fp <- newForeignPtr finalizerFree p
  return $ Data.Vector.Storable.unsafeFromForeignPtr0 fp $ fromIntegral n

withUTF8Text :: Text -> (Ptr () -> CSize -> IO a) -> IO a
withUTF8Text = unsafeWithBytes . Text.encodeUtf8

fromUTF8 :: Ptr () -> CSize -> IO Text
fromUTF8 p n = Text.decodeUtf8With Text.lenientDecode <$> copyByteString p n

unsafeMallocedUTF8 :: Ptr () -> CSize -> IO Text
unsafeMallocedUTF8 p n =
  Text.decodeUtf8With Text.lenientDecode <$> unsafeMallocedByteString p n

usingMalloced :: Ptr a -> IO b -> IO b
usingMalloced p action = action `finally` free p

usingManyMalloced :: [Ptr a] -> IO b -> IO b
usingManyMalloced ps action = foldr usingMalloced action ps

withMany :: (a -> (b -> IO r) -> IO r) -> [a] -> ([b] -> IO r) -> IO r
withMany with as f = go [] as
  where
    go bs [] = f $ reverse bs
    go bs (a:as) = with a $ \b -> go (b:bs) as

type Destroy a = FunPtr (Ptr a -> IO ())

class Object a where
  wrap :: ForeignPtr a -> a
  unwrap :: a -> ForeignPtr a
  destroy :: Destroy a

create :: Object a => Ptr a -> IO a
create = fmap wrap . newForeignPtr destroy

release :: Object a => a -> IO ()
release = finalizeForeignPtr . unwrap

construct :: Object a => IO (Ptr a) -> IO a
construct mk = mask_ $ mk >>= create

with :: Object a => a -> (Ptr a -> IO b) -> IO b
with x f = withForeignPtr (unwrap x) f

class Static a where
  destroyStatic :: a -> IO ()

newtype Constructor a = Constructor (IO a)

constructor :: IO a -> Constructor a
constructor = Constructor

withStatic :: Static a => Constructor a -> (a -> IO b) -> IO b
withStatic (Constructor mk) = using mk

using :: Static a => IO a -> (a -> IO b) -> IO b
using mk = bracket mk destroyStatic


-- -----------------------------------------------------------------------------
-- Sharing buffers to hold the result of FFI calls

newtype FFIResultBuf = FFIResultBuf (Ptr Word64)

wORD64_SIZE :: Int
wORD64_SIZE = sizeOf (undefined :: Word64)

-- | Create a temporary buffer which has space for 2 'Word64'. This can be used
-- via 'ffiBuf' and 'ffiBuf2' to avoid allocating a new buffer for every call
-- (as 'invoke' does) which is inefficient for high-volume/low-overhead calls.
-- 2 'Word64' are enough for everything we want to do currently.
withFFIResultBuf :: (FFIResultBuf -> IO a) -> IO a
withFFIResultBuf f = allocaBytes (wORD64_SIZE * 2) $ f . FFIResultBuf

-- | Execute an 'invoke'-like call, using the shared buffer to store the result.
--
-- NOTE: the value must be no bigger than `Word64`. This is checked
-- with an assertion, but will silently segfault when optimisation is
-- on.
ffiBuf
  :: forall a. (Storable a)
  => FFIResultBuf
  -> (Ptr a -> IO CString)
  -> IO a
{-# INLINE ffiBuf #-}
ffiBuf (FFIResultBuf buf) fun = do
  assert (sizeOf (undefined :: a) <= wORD64_SIZE) $ return ()
  invoke $ fun (castPtr buf)
  peek (castPtr buf)

-- | Execute an 'invoke'-like call, using the shared buffer to store the two
-- results.
--
-- NOTE: the values must be no bigger than `Word64` each. This is checked
-- with an assertion, but will silently segfault when optimisation is
-- on.
ffiBuf2
  :: forall a b. (Storable a, Storable b)
  => FFIResultBuf
  -> (Ptr a -> Ptr b -> IO CString)
  -> IO (a,b)
{-# INLINE ffiBuf2 #-}
ffiBuf2 (FFIResultBuf buf) fun = do
  assert (sizeOf (undefined :: a) <= wORD64_SIZE) $ return ()
  assert (sizeOf (undefined :: b) <= wORD64_SIZE) $ return ()
  let p = castPtr buf
      q = castPtr $ buf `plusPtr` wORD64_SIZE
  invoke $ fun p q
  (,) <$> peek p <*> peek q
