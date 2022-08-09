{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Bytecode.Supply
  ( Supply(..)
  , peekSupply
  , RegSupply
  , regSupply
  ) where

import Glean.Bytecode.Types

-- | Class of things of type `a` which can be supplied from `s`
class Supply a s where
  -- | Produce the next `a` and and updated `s`
  supply :: s -> (a,s)

-- | Get the next `a` in the supply
peekSupply :: Supply a s => s -> a
peekSupply = fst . supply

-- | A supply of registers
newtype RegSupply = RegSupply (Register 'Word)

-- | Create a new register supply which allocates consecutive registers starting
-- with the given one.
regSupply :: Register a -> RegSupply
regSupply = RegSupply . castRegister

instance Supply (Register a) RegSupply where
  supply (RegSupply r) = (castRegister r, RegSupply $ succ r)
