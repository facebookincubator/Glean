-- Copyright (c) Facebook, Inc. and its affiliates.

{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Test.HUnit
  ( assertThrows
  , assertThrowsType
  ) where

import Control.Concurrent.STM
import Control.Exception hiding(assert)
import Data.Proxy
import GHC.Stack (HasCallStack)
import Test.HUnit

instance Assertable t => Assertable (STM t) where
  assert = assert . atomically

assertThrows
  :: (HasCallStack, Eq e, Exception e) => String -> e -> IO a -> Assertion
assertThrows s e action = do
  r <- try action
  case r of
    Left exc -> assertEqual s exc e
    Right _ -> assertFailure s

assertThrowsType
  :: forall e a. (HasCallStack, Exception e)
  => String
  -> Proxy e
  -> IO a
  -> Assertion
assertThrowsType s _ action = do
  r <- try action
  case r of
    Left (_ :: e) -> return ()
    Right _ -> assertFailure s
