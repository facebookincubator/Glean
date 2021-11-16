{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE NamedFieldPuns #-}
-- | 'AnnMaker' hides creation of references and annotations (useful for
-- pretty printing).
module Glean.Util.AnnMaker
  ( -- * AnnMaker Annotations
    AnnMaker(..)
  , voidAnnMaker
    -- * Pretty-printer Annotations
  , Ann(..)
  , anStyle
  , mayAct
    -- * Cxx support
    -- * Cxx Code Entity
  , entityFromCxxDeclaration
  , entityFromCxxDecl
  , entityFromCxxDef
  , entityFromCxxEnumerator
    -- * Cxx AnnMaker RefTo
  , declRefTos
  , defDeclRefTos
  , enumeratorRefTos
    -- * Hack Code Entity
  , entityFromHackDeclaration
  , entityFromHackDecl
  ) where

import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeCxx.Types as CodeCxx
import qualified Glean.Schema.CodeHack.Types as CodeHack
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Hack.Types as Hack
import qualified Glean.Schema.Src.Types as Src

import Glean.Pretty.Style (Style)

import Glean.Typed.Predicate (SumBranches(..), Predicate(..))
import Glean.Util.Declarations
import Glean.Util.Range
import Glean.Util.Same

import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Word (Word64)

-- -----------------------------------------------------------------------------

-- | Interface to allow making full annotations, such as in
-- @pretty*@ functions
data AnnMaker r = AnnMaker
  { annMakerEntity :: Code.Entity -> [r]
  -- ^ Make link to an entity
  , annMakerRange :: Src.Range -> [r]
  -- ^ Make link to precise location
  , annMakerFileLines :: Text -> [Word64] -> [r]
  -- ^ Make link to file and line numbers
  }

-- | The 'voidAnnMaker' is useful when pretty-printing without annotations
voidAnnMaker :: AnnMaker r
voidAnnMaker = AnnMaker
  { annMakerEntity = const []
  , annMakerRange = const []
  , annMakerFileLines = \ _ _ -> [] }

-- -----------------------------------------------------------------------------
-- C++ Entity support

entityFromCxxDeclaration :: Cxx.Declaration -> Code.Entity
entityFromCxxDeclaration x = injectBranch (injectBranch x :: CodeCxx.Entity)

entityFromCxxDecl :: SumBranches p Cxx.Declaration => p -> Code.Entity
entityFromCxxDecl = entityFromCxxDeclaration . injectBranch

entityFromCxxDef :: SumBranches p CodeCxx.Definition => p -> Code.Entity
entityFromCxxDef = injectBranch
  . (injectBranch :: CodeCxx.Definition -> CodeCxx.Entity)
  . injectBranch

entityFromCxxEnumerator :: Cxx.Enumerator -> Code.Entity
entityFromCxxEnumerator x = injectBranch (injectBranch x :: CodeCxx.Entity)

-- -----------------------------------------------------------------------------
-- C++ RefTo support

declRefTos :: (DeclBranch p) => AnnMaker r -> p -> [r]
declRefTos am decl = annMakerEntity am (entityFromCxxDecl decl)
  ++ maybe [] (annMakerRange am . srcRange) (getFactKey decl)

defDeclRefTos
  :: (d ~ DeclToDef p, SumBranches d CodeCxx.Definition, DeclBranch p)
  => AnnMaker r -> d -> p -> [r]
defDeclRefTos am cdef decl = annMakerEntity am (entityFromCxxDef cdef)
  ++ maybe [] (annMakerRange am . srcRange) (getFactKey decl)

enumeratorRefTos :: AnnMaker r -> Cxx.Enumerator -> [r]
enumeratorRefTos am enumerator =
  annMakerEntity am (entityFromCxxEnumerator enumerator)
  ++ maybe [] (annMakerRange am . srcRange) (getFactKey enumerator)

-- -----------------------------------------------------------------------------
-- Hack Entity support

entityFromHackDeclaration :: Hack.Declaration -> Code.Entity
entityFromHackDeclaration x = injectBranch (injectBranch x :: CodeHack.Entity)

entityFromHackDecl :: SumBranches p Hack.Declaration => p -> Code.Entity
entityFromHackDecl = entityFromHackDeclaration . injectBranch

-- -----------------------------------------------------------------------------
-- Pretty-printing annotations

-- | Annotation type for pretty-printing 'Doc'
--
-- The @r@ is the reference data type, for links to symbols/code/docs.
data Ann r
  = AnnStyles [Style]
    --- ^ Annotate 'Doc' with list of semantic 'Style' names.
  | AnnAct Text [r]
    -- ^ Annotate 'Doc' as Action with intent Text and list of reference links.

-- | Simply smart combinator for annotating 'Doc' with a 'Style'.
--
-- Creates an 'AnnStyles' with a singleton list of this 'Style'.
anStyle :: Style -> Doc (Ann r) -> Doc (Ann r)
anStyle = annotate . AnnStyles . (:[])

-- | Simple smart combinator for annotating 'Doc' with link actions.
--
-- When the list is @[]@ this is identity.
mayAct :: [r] -> Doc (Ann r) -> Doc (Ann r)
mayAct [] doc = doc
mayAct refTos doc = annotate (AnnAct "" refTos) doc
