{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications, ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}

-- | The 'Cxx.Declaration' type has so many cases.  Try to capture some
-- using 'IsDeclaration' so we can reuse them.
module Glean.Util.Declarations
  ( -- * Visiting branches
    DeclBranch, applyDeclaration
    -- * Ranges
  , declarationSrcRange
    -- * Fact Id
  , getDeclId
  ) where

import Data.Int (Int64)
import Data.Typeable

import Glean.Schema.Cxx1.Types as Cxx -- gen
import Glean.Schema.Src.Types as Src -- gen
import Glean.Util.Range (HasSrcRange(..))
import Glean

-- -----------------------------------------------------------------------------

-- | 'DeclBranch' is the class constraint for writing the case handler
-- of 'applyDeclaration'
type DeclBranch p =
  ( Predicate p
  , Typeable p
  , Show p
  , Typeable (KeyType p), Show (KeyType p)
  , HasSrcRange (KeyType p)
  )

-- -----------------------------------------------------------------------------

{-# INLINE applyDeclaration #-}
-- | Provide a case handler.  If the 'DeclBranch' does not provide enough
-- constraints then please extend it, or use 'applyConstainedDeclaration'
applyDeclaration
  :: (forall a . DeclBranch a => a -> b)
  -> Cxx.Declaration
  -> b
applyDeclaration f = \case
  Cxx.Declaration_namespace_ d -> f d
  Cxx.Declaration_usingDeclaration d -> f d
  Cxx.Declaration_usingDirective d -> f d
  Cxx.Declaration_record_ d -> f d
  Cxx.Declaration_enum_ d -> f d
  Cxx.Declaration_function_ d -> f d
  Cxx.Declaration_variable d -> f d
  Cxx.Declaration_objcContainer d -> f d
  Cxx.Declaration_objcMethod d -> f d
  Cxx.Declaration_objcProperty d -> f d
  Cxx.Declaration_typeAlias d -> f d
  Cxx.Declaration_namespaceAlias d -> f d
  Cxx.Declaration_EMPTY -> error "unknown declaration branch"


-- -----------------------------------------------------------------------------

declarationSrcRange :: Cxx.Declaration -> Maybe Src.Range
declarationSrcRange = applyDeclaration (fmap srcRange . getFactKey)
-- -----------------------------------------------------------------------------

{-# INLINE getDeclId #-}
-- | The fact id from 'getDeclId' can be from the type of any branch, so
-- it cannot be 'IdOf'
getDeclId :: Cxx.Declaration -> Int64
getDeclId = applyDeclaration (fromFid . idOf . getId)
