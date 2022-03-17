{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}
-- | This splits out the RTSType encoding and decoding from Typed.hs
module Glean.Typed.Build
  ( Decoder(..)
  , DecoderEnv(..)
  , DecodingException(..), decodeFail
  , decode
  , decodeWithCache
  , sumD, enumD, thriftEnumD
  ) where

import Control.Exception ( Exception, throwIO )
import Control.Monad
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.ByteString (ByteString)
import qualified Data.ByteString.Unsafe as BS
import Data.Dynamic
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Foreign.Marshal.Utils as Marshal ( with )
import Foreign
import Thrift.Protocol (ThriftEnum, toThriftEnum)

import qualified Glean.FFI as FFI
import qualified Glean.RTS as RTS
import qualified Glean.Types as Thrift

-- -----------------------------------------------------------------------------

data DecoderEnv = DecoderEnv
  { begin :: {-# UNPACK #-} !(Ptr (Ptr ()))
  , end :: {-# UNPACK #-} !(Ptr ())
  , buf :: {-# UNPACK #-} !FFI.FFIResultBuf
  , serialized :: IntMap Thrift.Fact
  , cacheRef :: {-# UNPACK #-}!(IORef (IntMap Dynamic))
  }

-- | runDecoder takes 'begin' and 'end' and advances the 'begin' position.
-- Can usually throw 'GleanFFIError' or 'DecodingException'
newtype Decoder a = Decoder
 { runDecoder :: DecoderEnv -> IO a
 }

instance Functor Decoder where
  fmap f (Decoder g) = Decoder $ \env -> fmap f (g env)

instance Applicative Decoder where
  pure x = Decoder $ \_ -> pure x
  (<*>) = ap

instance Monad Decoder where
  return = pure
  Decoder m >>= k = Decoder $ \env -> do
    a <- m env
    runDecoder (k a) env

-- -----------------------------------------------------------------------------

newtype DecodingException = DecodingException { decodingException :: Text }

instance Show DecodingException where show = Text.unpack . decodingException

instance Exception DecodingException

-- | Throw 'DecodingException'
decodeFail :: Text -> IO a
decodeFail = throwIO . DecodingException

-- -----------------------------------------------------------------------------

-- | Can throw 'DecodingException'-. The 'Decoder' must consume the entire
-- 'ByteString' leaving no extra bytes.
decode :: forall m a. (MonadIO m) => Decoder a -> ByteString -> m a
decode decoder v = do
  cache <- liftIO $ newIORef IntMap.empty
  decodeWithCache IntMap.empty cache decoder v

decodeWithCache
  :: forall m a. (MonadIO m)
  => IntMap Thrift.Fact
  -> IORef (IntMap Dynamic)
  -> Decoder a
  -> ByteString
  -> m a
decodeWithCache serialized cache decoder v =
  liftIO $ BS.unsafeUseAsCStringLen v $ \(p,n) -> do
    let begin = castPtr p :: Ptr ()
        end   = castPtr (p `plusPtr` n) :: Ptr ()
    Marshal.with begin $ \ (pbegin :: Ptr (Ptr ())) -> do
      FFI.withFFIResultBuf $ \buf -> do
        x <- runDecoder decoder (DecoderEnv pbegin end buf serialized cache)
        pos <- peek pbegin
        when (pos /= end) $ do
          let extra = Text.pack (show (end `minusPtr` pos))
          decodeFail ("extra " <> extra <> " bytes at end of value")
        return x


-- | A generic decoder for sum types. Can throw 'GleanFFIError'
sumD :: Decoder b -> [Decoder b] -> Decoder b
sumD empty alts = Decoder $ \env@DecoderEnv{..} -> do
  sel <- FFI.ffiBuf buf $ RTS.glean_pop_value_selector begin end
  let Decoder f = index sel alts
  f env
  where
    index 0 (x:_) = x
    index i (_:xs) = index (i-1) xs
    index _ [] = empty

-- | A generic decoder, used for Bool. Can throw 'GleanFFIError'
enumD :: forall a. (Enum a, Bounded a) => Decoder a -> Decoder a
enumD unknown = Decoder $ \env@DecoderEnv{..} -> do
  sel <- FFI.ffiBuf buf $ RTS.glean_pop_value_selector begin end
  let Decoder f = if sel <= ord maxBound - ord minBound
        then return $ toEnum $ fromIntegral $ sel + ord minBound
        else unknown
  f env
  where
    ord x = fromIntegral $ fromEnum (x :: a)

-- | A generic decoder for thrift enum types.
thriftEnumD :: ThriftEnum a => Decoder a
thriftEnumD = Decoder $ \DecoderEnv{..} ->
  fmap (toThriftEnum . fromIntegral)
    $ FFI.ffiBuf buf
    $ RTS.glean_pop_value_selector begin end
