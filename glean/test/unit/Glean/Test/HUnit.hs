{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Glean.Test.HUnit
  ( assertThrows
  , assertThrowsType
  ) where

import Util.STM
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
