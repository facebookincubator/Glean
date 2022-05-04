{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.LSIF ({- instances -}) where

import qualified Network.URI.Encode as URI
import Data.Text ( Text )
import qualified Data.Text as Text
import Control.Monad.Catch ( throwM )

import qualified Glean
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Lsif.Types as Lsif
import Glean.Glass.Utils ( pathFragments )
import Glean.Glass.Types ( Name(..) )

import Glean.Glass.SymbolId.Class

instance Symbol Lsif.SomeEntity where
  toSymbol e = case e of
    Lsif.SomeEntity_defn defn -> toSymbolPredicate defn
    Lsif.SomeEntity_decl decl -> toSymbolPredicate decl
    _ -> throwM $ SymbolError "Unknown LSIF case"

instance Symbol Lsif.DefinitionMoniker_key where
  toSymbol (Lsif.DefinitionMoniker_key defn (Just moniker)) = do
    Lsif.Moniker_key kind _scheme ident <- Glean.keyOf moniker
    case kind of
      -- case 1: locals w/ monikers
      Lsif.MonikerKind_Local -> do
        prefix <- toSymbolPredicate defn -- generate our own
        i <- Glean.keyOf ident
        return (prefix ++ ["<local>", i])

      -- case 2: non-locals w/ monikers
      _ -> do -- use the "lsif" tag to indicate symbol is a moniker only
        i <- Glean.keyOf ident
        return ("lsif" : pathFragments (URI.decodeText i))

      -- case 3: no monikers at all, use path + ident and hope for best
  toSymbol (Lsif.DefinitionMoniker_key defn Nothing) = toSymbolPredicate defn

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
  return $ if Text.null ident then "<unknown>" else ident

instance ToQName Lsif.SomeEntity where
  toQName e = case e of
    Lsif.SomeEntity_defn defn -> Glean.keyOf defn >>= toQName
    Lsif.SomeEntity_decl decl -> Glean.keyOf decl >>= toQName
    _ -> throwM $ SymbolError "Unknown LSIF case"

instance ToQName Lsif.DefinitionMoniker_key where
  toQName (Lsif.DefinitionMoniker_key defn Nothing) =
    Glean.keyOf defn >>= toQName
  toQName (Lsif.DefinitionMoniker_key defn (Just moniker)) = do
    Lsif.Moniker_key _kind _scheme ident <- Glean.keyOf moniker
    container <- Glean.keyOf ident
    base <- symbolName defn
    return $ Right (Name container, Name base)

instance ToQName Lsif.Definition_key where
  toQName (Lsif.Definition_key _ range) = do
    id <- identOfRange range
    return $ Right (Name "", Name id)

instance ToQName Lsif.Declaration_key where
  toQName (Lsif.Declaration_key _ range) = do
    id <- identOfRange range
    return $ Right (Name "", Name id)

symbolName :: Lsif.Definition -> Glean.RepoHaxl u m Text
symbolName defn = do
  Lsif.Definition_key _ range <- Glean.keyOf defn
  identOfRange range
