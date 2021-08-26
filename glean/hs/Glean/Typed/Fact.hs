-- Copyright (c) Facebook, Inc. and its affiliates.

  {-# LANGUAGE ConstraintKinds #-}


module Glean.Typed.Fact
  ( decodeFact
  , decodeRef
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Dynamic
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.IntMap (IntMap)

import Glean.Typed.Build
import Glean.Typed.Binary
import Glean.Typed.Id
import Glean.Typed.Predicate
import qualified Glean.Types as Thrift -- generated

-- | Decode a serialized fact, possibly with nested facts
decodeFact
  :: (Predicate p, MonadIO m)
  => IntMap Thrift.Fact               -- ^ serialized nested facts
  -> IORef (IntMap Dynamic)           -- ^ cached deserialized facts
  -> IdOf p                           -- ^ Id of fact to decode
  -> Thrift.Fact                      -- ^ fact to decode
  -> m p
{-# INLINE decodeFact #-}
decodeFact x y i (Thrift.Fact _pid k v) = mkFact i
  <$> (Just <$> decodeWithCache x y decodeRtsValue k)
  <*> (Just <$> case constantRtsValue of
    Just val -> return val
    Nothing -> decodeWithCache x y decodeRtsValue v)

-- | Decode a fact reference inside a fact.
--
-- * If the nested fact is in the cache, then return it
-- * If the serialized fact is available, we decode it and add it to the cache
-- * Otherwise, we simply construct the empty fact with 'justId'.
--
decodeRef :: forall p. (Predicate p, Typeable p) => Decoder p
decodeRef = Decoder $ \env@DecoderEnv{..} -> do
  (fid :: IdOf p) <- runDecoder decodeRtsValue env
  cache <- liftIO $ readIORef cacheRef
  let id = fromIntegral (fromFid (idOf fid))
  case IntMap.lookup id cache of
    Just dyn
      | Just p <- fromDynamic dyn -> return p
      | otherwise -> liftIO $ throwIO $ ErrorCall "decodeRef: wrong type"
    Nothing -> do
      case IntMap.lookup id serialized of
        Nothing -> return (justId fid)
        Just fact -> do
          f <- decodeFact serialized cacheRef fid fact
          liftIO $ modifyIORef' cacheRef $ \cache ->
            IntMap.insert id (toDyn f) cache
          return f
