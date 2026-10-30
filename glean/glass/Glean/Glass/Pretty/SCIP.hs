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
  signature <- fetchData (definitionSignature defnId)
  -- Older indexers put the signature in a code block in the documentation
  text <- case signature of
    Just _ -> return signature
    Nothing -> fetchData (definitionHover defnId) -- just first result
  let docStream = layoutSmart opts . pretty <$> text
  return $ reAnnotateS (const Nothing) <$> docStream
  where
    defnId = Glean.getId defn

definitionSignature :: Glean.IdOf Scip.Definition -> Angle Text
definitionSignature defnId = vars $ \text signature ->
  text `where_` [
    wild .= predicate @Scip.DefinitionSignature (
      rec $
        field @"defn" (asPredicate (factId defnId)) $
        field @"signature" (asPredicate signature)
      end),
    signature .= predicate @Scip.Documentation text
  ]

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
