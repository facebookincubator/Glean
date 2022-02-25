{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE AllowAmbiguousTypes, TypeOperators #-}

-- |
-- Support for converting between Haskell types and Glean's binary
-- format.
--
module Glean.Typed.Binary
  ( -- * 'Type' encoder / decoder
    Type(..)
    -- ** helper
  , decodeRts
    -- * Used in generated Haskell code
  , mapD, buildRtsSelector, thriftEnum_buildRtsValue
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Unsafe as BS
import Data.Dynamic
import Data.IORef
import Data.IntMap (IntMap)
import Data.Text ( Text )
import Data.Word ( Word64 )
import Foreign.Ptr

import qualified Glean.FFI as FFI
import qualified Glean.RTS as RTS
import qualified Glean.RTS.Builder as RTS
import Glean.Typed.Build
import Glean.Typed.Id
import Glean.Types as Thrift

import Thrift.Protocol (ThriftEnum, fromThriftEnum)

-- -----------------------------------------------------------------------------

-- | Types that can be converted to/from Glean's binary representation
class Type a where
  -- | Convert to a Glean value
  buildRtsValue :: RTS.Builder -> a -> IO ()

  -- | Convert from a Glean value
  decodeRtsValue :: Decoder a

  -- | If this type only has one possible value and hence doesn't have a
  -- run-time representation (i.e., is effectively ()), yield that value
  constantRtsValue :: Maybe a

  constantRtsValue = Nothing

  -- | Decode this type when it is the result of a query. Queries for
  -- a predicate type return the facts of the predicate directly,
  -- whereas queries for non-predicate types return facts of a
  -- pseudo-predicate whose keys are the query results.  The
  -- 'decodeAsFact' method abstracts the difference in representation
  -- between predicate and non-predicate query results, so we can just
  -- use 'decodeAsFact' to decode query results.
  --
  -- The default implementation works for all non-predicate types;
  -- predicate types override it with 'decodeFact'.
  decodeAsFact
    :: (MonadIO m)
    => IntMap Thrift.Fact               -- ^ serialized nested facts
    -> IORef (IntMap Dynamic)           -- ^ cached deserialized facts
    -> IdOf a                           -- ^ Id of fact to decode
    -> Thrift.Fact                      -- ^ fact to decode
    -> m a

  decodeAsFact nested cache _fid (Thrift.Fact _ k _) =
    decodeWithCache nested cache decodeRtsValue k

-- -----------------------------------------------------------------------------

-- | Convenient combination, buiding on the 'decode' from
-- "Glean.RTS.Types.Build"
decodeRts :: forall m a. (MonadIO m, Type a) => ByteString -> m a
decodeRts = decode decodeRtsValue

-- | Nicer and shorter name, very handy to implement custom types by
-- using the tuple instances defined in this module.
mapD :: Type a => (a -> b) -> Decoder b
mapD f = f <$> decodeRtsValue

-- | Helper for SUM types
buildRtsSelector :: RTS.Builder -> Int -> IO ()
buildRtsSelector b = FFI.call . RTS.glean_push_value_selector b . fromIntegral

thriftEnum_buildRtsValue :: forall a. ThriftEnum a => RTS.Builder -> a -> IO ()
thriftEnum_buildRtsValue b x = buildRtsSelector b $ fromThriftEnum x

-- -----------------------------------------------------------------------------
-- Basic Type instances

instance Type Word64 where
  buildRtsValue b x = FFI.call $ RTS.glean_push_value_nat b x
  decodeRtsValue = Decoder $ \DecoderEnv{..} ->
    FFI.ffiBuf buf $ RTS.glean_pop_value_nat begin end

instance Type ByteString where
  buildRtsValue b xs = BS.unsafeUseAsCStringLen xs $ \(p,n) -> do
    FFI.call $ RTS.glean_push_value_array b (fromIntegral n)
    FFI.call $ RTS.glean_push_value_bytes b (castPtr p) (fromIntegral n)
  decodeRtsValue = Decoder $ \DecoderEnv{..} -> do
    size <- FFI.ffiBuf buf $ RTS.glean_pop_value_array begin end
    ptr <- FFI.ffiBuf buf $ RTS.glean_pop_value_bytes begin end size
    BS.unsafePackMallocCStringLen (castPtr ptr, fromIntegral size)

instance Type Text where
  buildRtsValue b s = FFI.withUTF8Text s $ \p n ->
    FFI.call $ RTS.glean_push_value_string b (castPtr p) n
  decodeRtsValue = Decoder $ \DecoderEnv{..} -> do
    (p,n) <- FFI.invoke $ RTS.glean_pop_value_string begin end
    FFI.unsafeMallocedUTF8 (castPtr p) n

-- | The instance for () does not encode or decode bytes, it is vacuous
instance Type () where
  buildRtsValue _ () = return ()
  decodeRtsValue = pure ()
  constantRtsValue = Just ()

-- | Bool is alias for sum of two ()
instance Type Bool where
  buildRtsValue b False = buildRtsSelector b 0
  buildRtsValue b True = buildRtsSelector b 1
  decodeRtsValue = enumD

instance Type Nat where
  buildRtsValue b nat = FFI.call $
    RTS.glean_push_value_nat b ((fromIntegral . unNat) nat)
  decodeRtsValue = Decoder $ \DecoderEnv{..} ->
    fmap (Nat . fromIntegral)
         (FFI.ffiBuf buf (RTS.glean_pop_value_nat begin end))

instance Type Byte where
  buildRtsValue b byt = FFI.call $
    RTS.glean_push_value_byte b ((fromIntegral . unByte) byt)
  decodeRtsValue = Decoder $ \DecoderEnv{..} ->
    fmap (Byte . fromIntegral)
         (FFI.ffiBuf buf (RTS.glean_pop_value_byte begin end))

-- -----------------------------------------------------------------------------
-- Containers

-- | Lists are preceded with their length
instance Type a => Type [a] where
  buildRtsValue b xs = liftIO $ do
    FFI.call $ RTS.glean_push_value_array b $ fromIntegral $ length xs
    mapM_ (buildRtsValue b) xs
  decodeRtsValue = Decoder $ \env@DecoderEnv{..} -> do
    size <- FFI.ffiBuf buf $ RTS.glean_pop_value_array begin end
    replicateM (fromIntegral size) (runDecoder decodeRtsValue env)

-- | 'Maybe' is alias pattern for sum of () | 'a' (in this order)
instance Type a => Type (Maybe a) where
  buildRtsValue b Nothing = buildRtsSelector b 0
  buildRtsValue b (Just x) = do
    buildRtsSelector b 1
    buildRtsValue b x
  decodeRtsValue = sumD [pure Nothing, Just <$> decodeRtsValue]

-- -----------------------------------------------------------------------------

-- | 'IdOf' as a Glean primitive
instance Type (IdOf p) where
  buildRtsValue b (IdOf fid) = FFI.call $ RTS.glean_push_value_fact b fid
  decodeRtsValue = Decoder $ \DecoderEnv{..} ->
    IdOf <$> FFI.ffiBuf buf (RTS.glean_pop_value_fact begin end)

-- -----------------------------------------------------------------------------
-- All the tuples, using the wonderful Applicative instance of Decoder

instance (Type a, Type b) => Type (a,b) where
  buildRtsValue builder (a,b) = do
    buildRtsValue builder a
    buildRtsValue builder b
  decodeRtsValue = (,)
    <$> decodeRtsValue
    <*> decodeRtsValue

instance (Type a, Type b, Type c) => Type (a,b,c) where
  buildRtsValue builder (a,b,c) = do
    buildRtsValue builder a
    buildRtsValue builder b
    buildRtsValue builder c
  decodeRtsValue = (,,)
    <$> decodeRtsValue
    <*> decodeRtsValue
    <*> decodeRtsValue

instance (Type a, Type b, Type c, Type d) => Type (a,b,c,d) where
  buildRtsValue builder (a,b,c,d) = do
    buildRtsValue builder a
    buildRtsValue builder b
    buildRtsValue builder c
    buildRtsValue builder d
  decodeRtsValue = (,,,)
    <$> decodeRtsValue
    <*> decodeRtsValue
    <*> decodeRtsValue
    <*> decodeRtsValue

instance (Type a, Type b, Type c, Type d, Type e) => Type (a,b,c,d,e) where
  buildRtsValue builder (a,b,c,d,e) = do
    buildRtsValue builder a
    buildRtsValue builder b
    buildRtsValue builder c
    buildRtsValue builder d
    buildRtsValue builder e
  decodeRtsValue = (,,,,)
    <$> decodeRtsValue
    <*> decodeRtsValue
    <*> decodeRtsValue
    <*> decodeRtsValue
    <*> decodeRtsValue
