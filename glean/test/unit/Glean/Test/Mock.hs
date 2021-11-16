{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Test.Mock
  ( IOBased(..)
  , Mock
  , mock
  , implement
  , call
  , augment
  , reimplement
  , prepare
  , doBefore
  , doFinally
  , unexpectedCall
  ) where

import Control.Exception (finally)
import Control.Monad (join)
import Data.IORef

-- | Things of type a1 -> ... -> an -> IO b
class IOBased a where
  -- | Constant action
  constIO :: (forall b. IO b) -> a

  -- | Transform the `IO` action
  mapIO :: (forall b. IO b -> IO b) -> a -> a

  -- | Push an outer `IO` into the `IO` action
  joinIO :: IO a -> a

instance IOBased (IO a) where
  constIO io = io
  mapIO f = f
  joinIO = join

instance IOBased b => IOBased (a -> b) where
  constIO io _ = constIO io
  mapIO f g x = mapIO f (g x)
  joinIO io x = joinIO $ ($x) <$> io

-- | Execute an `IO` before the action
doBefore :: IOBased a => IO () -> a -> a
doBefore p = mapIO (p >>)

-- | Execute an `IO` after the action
doFinally :: IOBased a => IO () -> a -> a
doFinally p = mapIO (`finally` p)

infixr 5 :>
data Stream a = a :> Stream a

instance Functor Stream where
  fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
  pure x = let xs = x :> xs in xs
  (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

prepend :: [a] -> Stream a -> Stream a
prepend xs s = foldr (:>) s xs

-- | A mocked action.
newtype Mock a = Mock (IORef (Stream a))

-- | Produce an "unexpected call" error
unexpectedCall :: IOBased a => String -> a
unexpectedCall name = constIO $ fail $ "unexpected call to '" ++ name ++ "'"

-- | Mock an `IOBased` action. All calls will produce "unexpected call" errors.
mock :: IOBased a => String -> IO (Mock a)
mock name = implement name $ unexpectedCall name

-- | Produce a 'Mock' based on an implementation of the operation. The name is
-- currently ignored but will be useful later.
implement :: String -> a -> IO (Mock a)
implement _name x = Mock <$> newIORef (pure x)

-- | Call a mocked 'IOBased' action.
call :: IOBased a => Mock a -> a
call (Mock ref) = joinIO $ atomicModifyIORef ref $ \(x :> xs) -> (xs, x)

augment :: Mock a -> (a -> a) -> IO ()
augment (Mock ref) f = atomicModifyIORef ref $ \xs -> (f <$> xs, ())

reimplement :: Mock a -> a -> IO ()
reimplement m = augment m . const

-- | Transform the next calls to the 'Mock'.
prepare :: Mock a -> [a -> a] -> IO ()
prepare (Mock ref) fs = atomicModifyIORef ref $ \xs ->
  (prepend fs (pure id) <*> xs, ())
