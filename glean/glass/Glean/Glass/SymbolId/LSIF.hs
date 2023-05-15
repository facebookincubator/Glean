{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.LSIF ({- instances -}) where

import Control.Monad.Catch ( throwM )
import Data.Maybe
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Network.URI.Encode as URI
import Util.Text ( textShow )

import qualified Glean
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Lsif.Types as Lsif
import qualified Glean.Schema.LsifTypes.Types as Lsif
import Glean.Glass.Utils ( pathFragments )
import Glean.Glass.Types

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
      -- case 1: locals. case a) no id, just position or b) id + pos
      Lsif.MonikerKind_Local -> Glean.keyOf defn >>= localsIdent

      -- case 2: non-locals w/ monikers
      _ -> do -- use the "lsif" tag to indicate symbol is a moniker only
        i <- Glean.keyOf ident
        return ("lsif" : pathFragments (URI.decodeText i))

      -- case 3: no monikers at all, use path + global ident and hope for best
  toSymbol (Lsif.DefinitionMoniker_key defn Nothing) = toSymbolPredicate defn

instance Symbol Lsif.Definition_key where
  toSymbol (Lsif.Definition_key doc range) = pathAndMaybeName doc range

instance Symbol Lsif.Declaration_key where
  toSymbol (Lsif.Declaration_key doc range) = pathAndMaybeName doc range

localsIdent :: Lsif.Definition_key -> Glean.RepoHaxl u m [Text]
localsIdent (Lsif.Definition_key doc range) = pathAndLocalPosition doc range

pathAndLocalPosition :: Lsif.Document -> Lsif.Range -> Glean.RepoHaxl u m [Text]
pathAndLocalPosition doc range = do
  path <- filePathOfDocument doc
  mident <- identOfRange range
  pos <- positionOfRange range
  return (path ++ ["<local>"] ++ maybeToList mident ++ pos)

pathAndMaybeName :: Lsif.Document -> Lsif.Range -> Glean.RepoHaxl u m [Text]
pathAndMaybeName doc range = do
  path <- filePathOfDocument doc
  mident <- identOfRange range
  case mident of
    Just ident -> return (path ++ [ident])
    Nothing -> do
      pos <- positionOfRange range
      return (path ++ ["<global>"] ++ pos)

filePathOfDocument :: Lsif.Document -> Glean.RepoHaxl u m [Text]
filePathOfDocument doc = do
  Lsif.Document_key{..} <- Glean.keyOf doc
  path <- Glean.keyOf document_key_file
  return (pathFragments path)

identOfRange :: Lsif.Range -> Glean.RepoHaxl u m (Maybe Text)
identOfRange range = do
  Lsif.Range_key{..} <- Glean.keyOf range
  ident <- Glean.keyOf range_key_text
  return $ if Text.null ident then Nothing else Just ident

positionOfRange :: Lsif.Range -> Glean.RepoHaxl u m [Text]
positionOfRange range = do
  Lsif.Range_key{..} <- Glean.keyOf range
  pure $ showSpan range_key_range

showSpan :: Lsif.RangeSpan -> [Text]
showSpan Lsif.RangeSpan{..} = map textShow
  [Glean.unNat rangeSpan_lineBegin, Glean.unNat rangeSpan_columnBegin
  ,Glean.unNat rangeSpan_lineEnd, Glean.unNat rangeSpan_columnEnd + 1]

instance ToQName Lsif.SomeEntity where
  toQName e = case e of
    Lsif.SomeEntity_defn defn -> Glean.keyOf defn >>= toQName
    Lsif.SomeEntity_decl decl -> Glean.keyOf decl >>= toQName
    _ -> throwM $ SymbolError "Unknown LSIF case"

instance ToQName Lsif.DefinitionMoniker_key where
  toQName (Lsif.DefinitionMoniker_key defn Nothing) =
    Glean.keyOf defn >>= toQName
  toQName (Lsif.DefinitionMoniker_key defn (Just moniker)) = do
    Lsif.Moniker_key kind _scheme ident <- Glean.keyOf moniker
    base <- symbolName defn
    fullContainer <- case kind of
      Lsif.MonikerKind_Local -> "" -- can't use moniker for locals namespace
      _ -> Glean.keyOf ident
    let container = fromMaybe fullContainer
          (Text.stripSuffix (":" <> base) fullContainer)
    return $ Right (Name base, Name container)

instance ToQName Lsif.Definition_key where
  toQName (Lsif.Definition_key _ range) = do
    mid <- identOfRange range
    return $ Right (Name "", Name (fromMaybe "<unknown>" mid))

instance ToQName Lsif.Declaration_key where
  toQName (Lsif.Declaration_key _ range) = do
    mid <- identOfRange range
    return $ Right (Name "", Name (fromMaybe "<unknown>" mid))

symbolName :: Lsif.Definition -> Glean.RepoHaxl u m Text
symbolName defn = do
  Lsif.Definition_key _ range <- Glean.keyOf defn
  mid <- identOfRange range
  pure (fromMaybe "<unknown>" mid)
