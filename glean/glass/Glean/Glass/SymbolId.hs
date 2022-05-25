{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
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

  -- * Lookups and language names
  , toShortCode
  , fromShortCode

  -- * searching for entities
  , entityToAngle

  -- * Qualified names
  , toQualifiedName

  -- reexports
  , SymbolRepoPath(..)

  ) where

import Data.Text ( Text )
import Control.Monad.Catch ( throwM, try )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import qualified Data.Text as Text
import Data.Tuple ( swap )
import qualified Network.URI.Encode as URI

import Glean.Glass.Base (SymbolRepoPath(..))
import Glean.Glass.Types as Glass
    ( DefinitionKind(DefinitionKind_Definition),
      SymbolKind,
      QualifiedName(..),
      Language(..),
      SymbolId(SymbolId),
      RepoName(..) )

import Glean.Angle ( alt, Angle )
import qualified Glean.Haxl.Repos as Glean
import Glean.Util.ToAngle

import Glean.Glass.SymbolId.Class
    ( Symbol(..),
      SymbolError(SymbolError),
      ToQName(..),
      ToSymbolParent(..) )

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
import Glean.Glass.SymbolId.LSIF ({- instances -})

import qualified Glean.Glass.SymbolId.Cxx as Cxx
import qualified Glean.Glass.SymbolId.Pp as Pp

import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeLsif.Types as Lsif

import Glean.Schema.CodeHack.Types as Hack ( Entity(Entity_decl) )
import Glean.Schema.CodePython.Types as Python ( Entity(Entity_decl) )
import Glean.Schema.CodeErlang.Types as Erlang ( Entity(Entity_decl) )

-- Introduce a SymbolId. This is essentially the semantic "path to this symbol
-- in the Codex style. www/php/Glean/getLatestRepo
--
-- If we can't encode the entity, it is still useful to return the path and
-- file, as this is enough to navigate with.
toSymbolId
  :: SymbolRepoPath -> Code.Entity -> Glean.RepoHaxl u w SymbolId
toSymbolId SymbolRepoPath{symbolRepo=Glass.RepoName repo} entity = do
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

-- | SymbolID-encoded language, used for db name lookups
shortCodeTable :: [(Language,Text)]
shortCodeTable =
  [ (Language_Haskell, "hs")
  , (Language_JavaScript, "js")
  , (Language_Hack, "php")
  , (Language_Python, "py")
  , (Language_Cpp, "cpp")
  , (Language_PreProcessor , "pp")
  , (Language_Thrift , "thrift")
  , (Language_Rust , "rs")
  , (Language_Buck , "buck")
  , (Language_Erlang , "erl")
  , (Language_Go , "go")
  , (Language_TypeScript , "ts")
  ]

languageToCode :: Map.Map Language Text
languageToCode = Map.fromList shortCodeTable

codeToLanguage :: Map.Map Text Language
codeToLanguage = Map.fromList (map swap shortCodeTable)

-- | Symbol identifier to use when we don't support symbol identifiers
unsupportedSymbol :: Text
unsupportedSymbol = "UNSUPPORTED_LANGUAGE"

-- | Language to canonical shortcode in symbol
toShortCode :: Language -> Text
toShortCode lang = fromMaybe unsupportedSymbol
  (Map.lookup lang languageToCode)

fromShortCode :: Text -> Maybe Language
fromShortCode code = Map.lookup code codeToLanguage

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
  Code.Entity_lsif Lsif.Entity_go{} -> Language_Go
  Code.Entity_lsif Lsif.Entity_typescript{} -> Language_TypeScript
  Code.Entity_lsif Lsif.Entity_rust{} -> Language_Rust
  Code.Entity_lsif Lsif.Entity_EMPTY -> Language__UNKNOWN 0
  Code.Entity_EMPTY -> Language__UNKNOWN 0

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
    Code.Entity_lsif (Lsif.Entity_typescript x) -> toSymbol x
    Code.Entity_lsif (Lsif.Entity_go x) -> toSymbol x
    Code.Entity_lsif (Lsif.Entity_rust x) -> toSymbol x
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
  Code.Entity_lsif (Lsif.Entity_typescript x) -> Right $
    alt @"lsif" (alt @"typescript" (toAngle x))
  Code.Entity_lsif (Lsif.Entity_go x) -> Right $
    alt @"lsif" (alt @"go" (toAngle x))
  Code.Entity_lsif (Lsif.Entity_rust x) -> Right $
    alt @"lsif" (alt @"rust" (toAngle x))
  _ -> Left $
    "Unsupported language: " <> toShortCode (entityLanguage e)

instance ToQName Code.Entity where
  toQName e = case e of
    Code.Entity_python (Python.Entity_decl x) -> toQName x
    Code.Entity_hack (Hack.Entity_decl x) -> toQName x
    Code.Entity_flow x -> toQName x
    Code.Entity_cxx x -> toQName x
    Code.Entity_erlang x -> toQName x
    Code.Entity_lsif (Lsif.Entity_typescript x) -> toQName x
    Code.Entity_lsif (Lsif.Entity_go x) -> toQName x
    Code.Entity_lsif (Lsif.Entity_rust x) -> toQName x
    _ -> return $ Left "Language unsupported"

instance ToSymbolParent Code.Entity where
  toSymbolParent e = case e of
    Code.Entity_cxx x -> toSymbolParent x
    Code.Entity_pp{} -> return Nothing
    _ -> return Nothing

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
