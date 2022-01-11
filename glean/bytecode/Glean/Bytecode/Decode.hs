{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Bytecode.Decode
  ( Offset(..)
  , Reg(..)
  , Decode
  , Decodable(..)
  , runDecode
  , decodeAll
  )
where

import Control.Monad
import Control.Monad.State.Strict (State, runState, state)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Word (Word64)

import Glean.Bytecode.Types (Ty)

newtype Offset = Offset { fromOffset :: Word64 }
newtype Reg (t :: Ty) = Reg { fromReg :: Word64 }

-- | A decoder which can be applied to a stream of words via 'runDecode'.
type Decode = MaybeT (State [Word64])

-- | Execute a 'Decode' and return the result and the remaining words.
runDecode :: Decode a -> [Word64] -> Maybe (a, [Word64])
runDecode (MaybeT f) xs = case runState f xs of
  (Just x, ys) -> Just (x, ys)
  _ -> Nothing

-- | Things that can be decoded.
class Decodable arg where
  decode :: Decode arg

instance Decodable Word64 where
  decode = MaybeT $ state $ \xs -> case xs of
    y : ys -> (Just y, ys)
    _ -> (Nothing, xs)

instance Decodable Offset where
  decode = Offset <$> decode

instance Decodable [Offset] where
  decode = do
    n <- decode
    replicateM (fromIntegral (n :: Word64)) decode

instance Decodable (Reg t) where
  decode = Reg <$> decode

-- | Decode as many elements as possible and return the decoded elements and
-- the rest of the words, starting with the first that can't be decoded.
decodeAll :: Decodable a => [Word64] -> ([a], [Word64])
decodeAll = go []
  where
    go xs [] = (reverse xs, [])
    go xs vs = case runDecode decode vs of
      Just (x, ws) -> go (x:xs) ws
      Nothing -> (reverse xs, vs)
