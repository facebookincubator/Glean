-- Copyright 2004-present Facebook. All Rights Reserved.
-- Note: UnboxedTuples needed for deriving Prim
{-# LANGUAGE GeneralizedNewtypeDeriving, TypeInType, UnboxedTuples #-}
  -- | This does not import Glean modules at all
module Glean.Typed.Id
  ( -- * Id-like
    Pid(..), Fid(..)
  , PidOf(..), IdOf(..)
) where

import Control.DeepSeq
import Data.Hashable (Hashable)
import Data.Text.Prettyprint.Doc (Pretty(..))
import Data.Vector.Primitive (Prim)
import Foreign.Storable (Storable)

import Glean.RTS.Types

-- -----------------------------------------------------------------------------

-- | Tie the Pid to a predicate phantom type 'p'.
newtype PidOf p = PidOf { pidOf :: Pid }
  deriving(Eq, Ord, Show)

instance Pretty (PidOf p) where
  pretty (PidOf p) = pretty p

-- | Id of a fact of a given predicate type 'p' (phantom type parameter).
-- Note: This is not the Id/Fid/Pid of the definition of predicate 'p'.
newtype IdOf p = IdOf { idOf :: Fid }
  deriving(Eq, Ord, Show, Hashable, NFData, Storable, Prim)

instance Pretty (IdOf p) where
  pretty (IdOf f) = pretty f
