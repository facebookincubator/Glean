{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Pretty.SCIP ( prettyScipSignature ) where

import Data.Text ( Text )

import qualified Glean
import Glean.Angle as Angle
import Glean.Haxl.Repos as Glean ( RepoHaxl )
import Glean.Glass.Utils ( fetchData )
import Compat.Prettyprinter
  (pretty,
   layoutSmart,
   LayoutOptions,
   SimpleDocStream,
   reAnnotateS)

import qualified Glean.Schema.Scip.Types as Scip
import Glean.Glass.Types ( SymbolId(..) )

prettyScipSignature
  :: LayoutOptions
  -> Scip.SomeEntity
  -> Glean.RepoHaxl u w (Maybe (SimpleDocStream (Maybe SymbolId)))
prettyScipSignature opts (Scip.SomeEntity defn) = do
  text <- fetchData (definitionHover (Glean.getId defn)) -- just first result
  let docStream = layoutSmart opts . pretty <$> text
  return $ reAnnotateS (const Nothing) <$> docStream

definitionHover :: Glean.IdOf Scip.Definition -> Angle Text
definitionHover defnId = vars $ \text docText ->
  text `where_` [
    wild .= predicate @Scip.DefinitionDocumentation (
      rec $
        field @"defn" (asPredicate (factId defnId)) $
        field @"docs" (asPredicate docText)
      end),
    docText .= predicate @Scip.Documentation text

  ]
