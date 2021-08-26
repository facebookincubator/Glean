-- Copyright (c) Facebook, Inc. and its affiliates.

-- | A value that may change over time, and that we can observe efficiently.
module Glean.Util.Observed
  ( ObserveMonad(..)
  , Observed
  , changingValue
  , fixedValue
  , doOnUpdate
  ) where

import Control.Concurrent.STM
import Control.Monad
import Data.IORef

-- | A value that may change over time, and that we can observe efficiently.
data Observed a = Observed
  { observedGet :: STM a
  , observedUpdated :: Maybe (IORef (IO ()))
    -- invoked when the value is updated
  }

-- | Create an 'Observed' that tracks an underlying value.
--
-- The caller should invoke the returned 'IO' action when the
-- underlying value changes.
changingValue
  :: a
  -> IO (Observed a, (a -> a) -> IO ())
changingValue initial = do
  tvar <- newTVarIO initial
  updateActionRef <- newIORef (return ())
  let
    onUpdate fn = do
      atomically (modifyTVar tvar fn)
      join $ readIORef updateActionRef
  return (Observed (readTVar tvar) (Just updateActionRef), onUpdate)

fixedValue :: a -> Observed a
fixedValue a = Observed (return a) Nothing

instance Functor Observed where
  fmap f (Observed stm upd) = Observed (fmap f stm) upd


-- | Run the given IO action whenever the value in the Observed is
-- updated. Guarantees (or lack thereof):
--
-- * When the action runs, the Observed has already been updated and
--   'get' will return the new value.
-- * Multiple instances of the action may run concurrently
--
doOnUpdate :: Observed a -> IO () -> IO ()
doOnUpdate Observed{..} action =
  forM_ observedUpdated $ \ref ->
    atomicModifyIORef' ref $ \io -> (io >> action, ())

-- | Monads which can 'get' the current value of an 'Observed'
class ObserveMonad m where
  -- | Obtain the current value of an 'Observed'. This is efficient
  -- enough to be called every time you need the value.
  get :: Observed a -> m a

instance ObserveMonad STM where
  get (Observed f _) = f

instance ObserveMonad IO where
  get = atomically . get
