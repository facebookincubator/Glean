{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Trace
  ( Listener
  , listener
  , recorder
  , notify

  , Expect
  , expect
  , reject
  , accept
  , want
  , wants
  , (>|<)
  , opt
  , alt
  , (|||)
  , parallel
  )
where

import Control.Monad
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Hashable (Hashable)
import Data.Typeable

import Util.STM

-- | A 'Listener' can be notified of 'Typeable' events. Listeners can be
-- combined via the 'Semigroup'/'Monoid' interface.
newtype Listener = Listener (forall a. Typeable a => a -> STM ())

instance Semigroup Listener where
  Listener f <> Listener g = Listener $ \x -> f x >> g x

instance Monoid Listener where
  mempty = Listener $ const $ return ()

-- | Create a 'Listener' for a particular type.
listener :: Typeable a => (a -> STM ()) -> Listener
listener f = Listener $ mapM_ f . cast

-- | Create a 'Listener' which writes all events to a queue.
recorder :: Typeable a => IO (Listener, TQueue a)
recorder = do
  events <- newTQueueIO
  return (listener $ writeTQueue events, events)

-- | Notify a listener about an event, ignoring all exceptions.
notify :: Typeable a => Listener -> a -> STM ()
notify (Listener f) = f


-- | 'Expect' encapsulates expectations about which events should happen in
-- what order. We can expect a particular event via 'want', sequentially
-- compose expectations via 'Semigroup'/'Monoid', encode alternatives via
-- '>|<' and interleave expectations via '|||'. Once an 'Expect' is constructed,
-- it can be applied to the contents of a 'TQueue' via 'expect'.
data Expect a = Expect
  (HashMap a (Expect a))  -- what to do with each event
  Bool                    -- is this 'Expect' optional
  deriving(Show)

instance (Eq a, Hashable a) => Semigroup (Expect a) where
  Expect xs a <> q@(Expect _ b) =
    alt $ Expect (fmap (<> q) xs) (a && b) : [q | a]

instance (Eq a, Hashable a) => Monoid (Expect a) where
  mempty = Expect mempty True

-- | An 'Expect' that always fails.
reject :: (Eq a, Hashable a) => Expect a
reject = Expect mempty False

-- | An 'Expect' that always succeeds.
accept :: (Eq a, Hashable a) => Expect a
accept = mempty

-- | Expect a specific event.
want :: (Eq a, Hashable a) => a -> Expect a
want x = Expect (HashMap.singleton x accept) False

-- | Expect a sequence of events.
wants :: (Eq a, Hashable a) => [a] -> Expect a
wants = foldr (\x p -> Expect (HashMap.singleton x p) False) accept

infixl 3 >|<

-- | Alternative
(>|<) :: (Eq a, Hashable a) => Expect a -> Expect a -> Expect a
Expect xs a >|< Expect ys b = Expect (HashMap.unionWith (>|<) xs ys) (a || b)

-- | Optional
opt :: (Eq a, Hashable a) => Expect a -> Expect a
opt p = p >|< accept

-- | Multiple alternatives
alt :: (Eq a, Hashable a) => [Expect a] -> Expect a
alt = foldr (>|<) reject

infixl 2 |||

-- | Parallel composition - the two expectations can be arbitrarily interleaved
-- (both ordering within each 'Expect' is preserved).
(|||) :: (Eq a, Hashable a) => Expect a -> Expect a -> Expect a
p@(Expect xs a) ||| q@(Expect ys b) = alt $
  [ Expect
      (HashMap.unionWith
        (>|<)
        (fmap (||| Expect ys False) xs)
        (fmap (Expect xs False |||) ys))
      (a && b) ]
  ++ [p | b]
  ++ [q | a]

-- | Interleave multiple 'Expect's.
parallel :: (Eq a, Hashable a) => [Expect a] -> Expect a
parallel = foldr (|||) accept

-- | Consume elements of the 'TQueue' until we hit an 'accept', failing on
-- unexpected elements. This will always consume the smallest possible number
-- of events.
expect :: (Eq a, Hashable a, Show a) => TQueue a -> Expect a -> IO ()
expect queue (Expect xs b)
  | HashMap.null xs = if b then return () else fail "reject"
  | otherwise = join $ atomically $ do
      x <- readTQueue queue
      case HashMap.lookup x xs of
        Just p -> return $ expect queue p
        Nothing
          | b -> do
              unGetTQueue queue x
              return $ return ()
          | otherwise -> return $ fail $ concat
              [ "unexpected ", show x
              , "; expected one of ", show (HashMap.keys xs) ]
