-- | Simple 'Storable'-based append buffers in 'IO' and 'ST'.
--
-- The main properties are amortised constant 'push' and constant GC cost.
module Glean.Util.ValueBuffer
  ( Buffer, IOBuffer
  , new
  , push
  , appends
  , get
  , unsafeFreeze
  )
where

import Control.Monad
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Vector.Generic (Vector, Mutable)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV

data Impl v s a = Impl
  { _implData :: !(Mutable v s a)
  , _implLen :: {-# UNPACK #-} !Int
  }

-- | Append buffer type.
newtype Buffer v s a = Buffer { _fromBuffer :: MutVar s (Impl v s a) }

-- | 'Buffer' specialised to 'IO'.
type IOBuffer v = Buffer v RealWorld

-- | Create a new buffer with the given initial capacity.
new :: (PrimMonad m, Vector v a) => Int -> m (Buffer v (PrimState m) a)
{-# INLINABLE new #-}
new cap = do
  v <- MV.new $ max cap 1
  Buffer <$> newMutVar (Impl v 0)

-- | Push an element to the buffer, growing it if necessary.
push :: (PrimMonad m, Vector v a) => Buffer v (PrimState m) a -> a -> m ()
{-# INLINABLE push #-}
push (Buffer ref) x = do
  Impl v i <- readMutVar ref
  w <- if i < MV.length v
    then return v
    else MV.unsafeGrow v (MV.length v)
  MV.unsafeWrite w i x
  writeMutVar ref $! Impl w (i+1)

-- | Append multiple batches to the buffer.
--
-- NOTE: The list will be evaluated strictly.
appends
  :: (PrimMonad m, Vector v a) => Buffer v (PrimState m) a -> [v a] -> m ()
{-# INLINABLE appends #-}
appends _ [] = return ()
appends (Buffer ref) xs = do
  Impl v i <- readMutVar ref
  let !n = sum $ map V.length xs
  !w <- if i+n <= MV.length v
    then return v
    else MV.grow v $ max (i + n - MV.length v) (MV.length v)
  let app !i x = do
        let k = V.length x
        V.copy (MV.slice i k w) x
        return (i+k)
  i' <- foldM app i xs
  writeMutVar ref $ Impl w i'

-- | Return the underlying vector.
--
-- NOTE: Subsequent changes to the buffer might mutate the vector and vice
-- versa.
get
  :: (PrimMonad m, Vector v a)
  => Buffer v (PrimState m) a
  -> m (Mutable v (PrimState m) a)
{-# INLINABLE get #-}
get (Buffer ref) = do
  Impl v i <- readMutVar ref
  return $ MV.unsafeSlice 0 i v

-- | Freeze the buffer and return all writen elements as a vector.
unsafeFreeze
  :: (PrimMonad m, Vector v a) => Buffer v (PrimState m) a -> m (v a)
{-# INLINABLE unsafeFreeze #-}
unsafeFreeze = V.unsafeFreeze <=< get
