{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.LSIF ({- instances -}) where

import Data.Text ( Text )
import qualified Data.Text as Text
import Control.Monad.Catch ( throwM )

import qualified Glean
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Lsif.Types as Lsif

import Glean.Glass.SymbolId.Class
import Glean.Glass.Utils ( pathFragments )
-- import qualified Glean.Schema.Src.Types as Src

instance Symbol Lsif.SomeEntity where
  toSymbol e = case e of
    Lsif.SomeEntity_defn defn -> toSymbolPredicate defn
    Lsif.SomeEntity_decl decl -> toSymbolPredicate decl
    _ -> throwM $ SymbolError "Unknown LSIF case"

instance Symbol Lsif.Definition_key where
  toSymbol (Lsif.Definition_key doc range) = pathAndName doc range

instance Symbol Lsif.Declaration_key where
  toSymbol (Lsif.Declaration_key doc range) = pathAndName doc range

pathAndName :: Lsif.Document -> Lsif.Range -> Glean.RepoHaxl u m [Text]
pathAndName doc range = do
    path <- filePathOfDocument doc
    ident <- identOfRange range
    return (path ++ [ident])

filePathOfDocument :: Lsif.Document -> Glean.RepoHaxl u m [Text]
filePathOfDocument doc = do
  Lsif.Document_key{..} <- Glean.keyOf doc
  path <- Glean.keyOf document_key_file
  return (pathFragments path)

identOfRange :: Lsif.Range -> Glean.RepoHaxl u m Text
identOfRange range = do
  Lsif.Range_key{..} <- Glean.keyOf range
  ident <- Glean.keyOf range_key_text
  return $ if Text.null ident then "unknown" else ident
