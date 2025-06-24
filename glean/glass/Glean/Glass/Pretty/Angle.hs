{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Glean.Glass.Pretty.Angle
  (
    prettyAngleSignature
  ) where

import Data.Text ( pack )
import Compat.Prettyprinter
import qualified Glean.Haxl.Repos as Glean
import Glean.Schema.CodeAnglelang.Types as A ( Entity(..) )
import qualified Glean.Schema.Anglelang.Types as A
import Glean.Glass.Types
import Glean.Glass.SymbolId.Class (toQName, ToQName)
import Glean.Glass.SymbolId.Angle ({- instances -})
import Glean.Haxl.Repos (RepoHaxl)


prettyAngleSignature
  :: LayoutOptions
  -> A.Entity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyAngleSignature opts entity = do
  let decl = A.entity_decl entity
      kind = prettyDeclKind decl
  name <- prettyName decl
  let doc = layoutSmart opts (kind <+> name)
      annDoc = reAnnotateS (\() -> Nothing) doc
  return $ Just annDoc

prettyDeclKind :: A.Declaration -> Doc ()
prettyDeclKind decl = case decl of
  A.Declaration_pred _ -> pretty $ pack "predicate"
  A.Declaration_ty _ -> pretty $ pack "type"
  A.Declaration_schema _ -> pretty $ pack "schema"
  _ -> emptyDoc


prettyName
  :: ToQName A.Declaration => A.Declaration
  -> RepoHaxl u w (Doc ())
prettyName decl = do
  qname <- toQName decl
  case qname of
    Left _ -> return emptyDoc
    Right (Name name,_ns) -> return (pretty name)
