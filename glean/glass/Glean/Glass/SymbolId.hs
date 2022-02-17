{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId
  (
    -- * introduction
    toSymbolId

    -- * elimination
  , symbolTokens

  -- * the language an entity is contained within
  , entityLanguage
  , entityDefinitionType
  , entityKind

  -- * searching for entities
  , ToAngle(..)
  , entityToAngle

  -- * Qualified names
  , toQualifiedName

  -- * searching by prefix
  , findSymbolKind

   ) where

import Data.Text ( Text )
import Control.Monad.Catch ( throwM, try )
import qualified Data.Text as Text
import qualified Network.URI.Encode as URI

import Glean.Glass.Types as Glass
    ( DefinitionKind(DefinitionKind_Definition),
      SymbolKind,
      QualifiedName(..),
      Language(Language_Thrift, Language_Cpp, Language_JavaScript,
               Language_Hack, Language_Haskell, Language_Java,
               Language_PreProcessor, Language_Python, Language_Rust,
               Language_Buck, Language_Erlang),
      SymbolId(SymbolId),
      RepoName(..) )
import Glean.Glass.Query as Query ( symbolKind )
import Glean.Glass.Repos
    ( fromShortCode, toShortCode )
import Glean.Glass.Utils as Utils ( searchWithLimit )
import Glean.Glass.Attributes.SymbolKind as Glass
    ( symbolKindToSymbolKind )

import Glean.Angle ( alt, Angle )
import qualified Glean.Haxl.Repos as Glean

import Glean.Glass.SymbolId.Class
    ( Symbol(..),
      SymbolError(SymbolError),
      ToAngle(..),
      ToQName(..),
      ToSymbolParent(..),
      ToSymbolSignature(..) )

import Glean.Glass.SymbolId.Cxx ({- instances -})
import Glean.Glass.SymbolId.Flow ({- instances -})
import Glean.Glass.SymbolId.Hack ({- instances -})
import Glean.Glass.SymbolId.Hs ({- instances -})
import Glean.Glass.SymbolId.Pp ({- instances -})
import Glean.Glass.SymbolId.Python ({- instances -})
import Glean.Glass.SymbolId.Rust ({- instances -})
import Glean.Glass.SymbolId.Buck ({- instances -})
import Glean.Glass.SymbolId.Thrift ({- instances -})
import Glean.Glass.SymbolId.Erlang ({- instances -})

import Glean.Glass.Pretty.Cxx as Cxx ( prettyCxxSignature )
import Glean.Glass.Pretty.Hack as Hack ( prettyHackSignature )
import qualified Glean.Glass.SymbolId.Cxx as Cxx
import qualified Glean.Glass.SymbolId.Pp as Pp

import qualified Glean.Schema.Code.Types as Code

import Glean.Schema.CodeHack.Types as Hack ( Entity(Entity_decl) )
import Glean.Schema.CodePython.Types as Python ( Entity(Entity_decl) )
import Glean.Schema.CodeErlang.Types as Erlang ( Entity(Entity_decl) )

-- Introduce a SymbolId. This is essentially the semantic "path to this symbol
-- in the Codex style. www/php/Glean/getLatestRepo
--
-- If we can't encode the entity, it is still useful to return the path and
-- file, as this is enough to navigate with.
--
toSymbolId :: RepoName -> Code.Entity -> Glean.RepoHaxl u w SymbolId
toSymbolId (RepoName repo) entity = do
  let fileType = toShortCode (entityLanguage entity)
  eqname <- try $ toSymbol entity
  return $ case eqname of
    Left (SymbolError _e) -> symbol [repo, fileType, "SYMBOL_ID_MISSING"]
    Right spec -> symbol $ repo : fileType : map URI.encodeText spec
  where
    symbol = SymbolId . Text.intercalate "/"

--
-- For entity descriptions, we use toQualifiedName to extract some info
-- about the namespace or scope of the entity
--
toQualifiedName :: Code.Entity -> Glean.RepoHaxl u w (Either Text QualifiedName)
toQualifiedName entity = do
  qname <- toQName entity
  return $ case qname of
    Right (qualifiedName_localName, qualifiedName_container) ->
      Right QualifiedName {..}
    Left e -> Left $ "QualifiedName: " <> e

-- | Tokenize a symbol (inverse of the intercalate "/")
-- Leaves the path/qname/syms for further search.
symbolTokens :: SymbolId -> Either Text (RepoName, Language, [Text])
symbolTokens (SymbolId symid)
  | (repo: code: pieces) <- tokens
  , Just lang <- fromShortCode code
  = Right (RepoName repo, lang, map URI.decodeText pieces)

  | otherwise = Left $ "Invalid symbol: " <> symid
  where
    tokens = Text.split (=='/') symid

-- | The language is the outermost tag of the code.Entity constructor
entityLanguage :: Code.Entity -> Language
entityLanguage e = case e of
  Code.Entity_cxx{} -> Language_Cpp
  Code.Entity_flow{} -> Language_JavaScript
  Code.Entity_hack{} -> Language_Hack
  Code.Entity_hs{} -> Language_Haskell
  Code.Entity_java{} -> Language_Java
  Code.Entity_pp{} -> Language_PreProcessor
  Code.Entity_python{} -> Language_Python
  Code.Entity_rust{} -> Language_Rust
  Code.Entity_thrift{} -> Language_Thrift
  Code.Entity_buck{} -> Language_Buck
  Code.Entity_erlang{} -> Language_Erlang

-- | An encoded Entity.
--
-- e.g. Glean/getRepoName -- method
--      GleanRecursive   -- enum
--
instance Symbol Code.Entity where
  toSymbol e = case e of
    Code.Entity_hack (Hack.Entity_decl x) -> toSymbol x
    Code.Entity_python (Python.Entity_decl x) -> toSymbol x
    Code.Entity_flow x -> toSymbol x
    Code.Entity_cxx x -> toSymbol x
    Code.Entity_pp x -> toSymbol x
    Code.Entity_hs x -> toSymbol x
    Code.Entity_rust x -> toSymbol x
    Code.Entity_buck x -> toSymbol x
    Code.Entity_thrift x -> toSymbol x
    Code.Entity_erlang x -> toSymbol x
    _ -> throwM $ SymbolError "Language not supported"

-- | Top level with error handler, to catch attempts to query
-- languages we don't support
entityToAngle :: Code.Entity -> Either Text (Angle Code.Entity)
entityToAngle e = case e of
  Code.Entity_hack (Hack.Entity_decl x) -> Right $
    alt @"hack" (alt @"decl" (toAngle x))
  Code.Entity_python (Python.Entity_decl x) -> Right $
    alt @"python" (alt @"decl" (toAngle x))
  Code.Entity_flow x -> Right $
    alt @"flow" (toAngle x)
  Code.Entity_cxx x -> Right $
    alt @"cxx" (toAngle x)
  Code.Entity_hs x -> Right $
    alt @"hs" (toAngle x)
  Code.Entity_erlang (Erlang.Entity_decl x) -> Right $
    alt @"erlang" (alt @"decl" (toAngle x))
  _ -> Left $
    "Unsupported language: " <> toShortCode (entityLanguage e)

instance ToQName Code.Entity where
  toQName e = case e of
    Code.Entity_python (Python.Entity_decl x) -> toQName x
    Code.Entity_hack (Hack.Entity_decl x) -> toQName x
    Code.Entity_flow x -> toQName x
    Code.Entity_cxx x -> toQName x
    Code.Entity_erlang x -> toQName x
    _ -> return $ Left "Language unsupported"

instance ToSymbolParent Code.Entity where
  toSymbolParent e = case e of
    Code.Entity_cxx x -> toSymbolParent x
    Code.Entity_pp{} -> return Nothing
    _ -> return Nothing

instance ToSymbolSignature Code.Entity where
  toSymbolSignature e = case e of
    Code.Entity_cxx x -> return $ case Cxx.prettyCxxSignature x of
      "" -> Nothing
      s -> Just s
    Code.Entity_hack x -> Hack.prettyHackSignature x
    Code.Entity_pp{} -> return Nothing
    _ -> return Nothing

-- | Pointwise lookup of a symbol kind by entity
findSymbolKind
  :: Code.Entity
  -> Glean.RepoHaxl u w (Either Text Glass.SymbolKind)
findSymbolKind e = case entityToAngle e of
  Left err -> return $ Left $ "ToSymbolKind: " <> err
  Right ent -> do
    r <- Utils.searchWithLimit (Just 1) $ Query.symbolKind ent
    return $ case r of
      [] -> Left "No kind found"
      (kind:_) -> Right $ Glass.symbolKindToSymbolKind kind

-- | Attribute for definition/declaration distinction
entityDefinitionType :: Code.Entity -> Maybe Glass.DefinitionKind
entityDefinitionType (Code.Entity_cxx e) = Just (Cxx.cxxEntityDefinitionType e)
entityDefinitionType Code.Entity_pp{} = Just Glass.DefinitionKind_Definition
entityDefinitionType _ = Nothing

-- | Glass-side implementation of entity kinds, for C++ only
entityKind :: Code.Entity -> Glean.RepoHaxl u w (Maybe Glass.SymbolKind)
entityKind (Code.Entity_cxx e) = Cxx.cxxEntityKind e
entityKind (Code.Entity_pp e) = Pp.ppEntityKind e
entityKind _ = return Nothing
