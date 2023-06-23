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
  , languageToCodeLang
  , languageExpandCpp

  -- * Lookups and language names
  , toShortCode
  , fromShortCode

  -- * searching for entities
  , entityToAngle

  -- * Qualified names
  , toQualifiedName
  , toSymbolLocalName
  , toSymbolQualifiedContainer

  -- reexports
  , SymbolRepoPath(..)

  ,nativeSymbol) where

import Control.Monad.Catch ( throwM, try )
import Data.Maybe ( fromMaybe )
import Data.Tuple ( swap )
import Util.Text ( textShow )
import Data.Text ( Text )
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Network.URI.Encode as URI

import Glean.Glass.Base (SymbolRepoPath(..))
import Glean.Glass.Types as Glass
    ( DefinitionKind(DefinitionKind_Definition),
      SymbolKind,
      QualifiedName(..),
      Language(..),
      SymbolId(SymbolId),
      RepoName(..),
      Name, )

import Glean.Angle ( alt, Angle )
import qualified Glean.Haxl.Repos as Glean
import Glean.Util.ToAngle ( ToAngle(toAngle) )

import Glean.Glass.SymbolId.Class
    ( Symbol(..),
      SymbolError(SymbolError),
      ToQName(..),
      ToSymbolParent(..),
      ToNativeSymbol(..) )

import Glean.Glass.SymbolId.Buck ({- instances -})
import Glean.Glass.SymbolId.Cxx ({- instances -})
import Glean.Glass.SymbolId.Erlang ({- instances -})
import Glean.Glass.SymbolId.Flow ({- instances -})
import Glean.Glass.SymbolId.Hack ({- instances -})
import Glean.Glass.SymbolId.Hs ({- instances -})
import Glean.Glass.SymbolId.Java ({- instances -})
import Glean.Glass.SymbolId.LSIF ({- instances -})
import Glean.Glass.SymbolId.SCIP ({- instances -})
import Glean.Glass.SymbolId.Pp ({- instances -})
import Glean.Glass.SymbolId.Python ({- instances -})
import Glean.Glass.SymbolId.Thrift ({- instances -})

import qualified Glean.Glass.SymbolId.Cxx as Cxx
import qualified Glean.Glass.SymbolId.Pp as Pp

import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeLsif.Types as Lsif
import qualified Glean.Schema.CodeScip.Types as Scip

import Glean.Schema.CodeErlang.Types as Erlang ( Entity(Entity_decl) )
import Glean.Schema.CodeHack.Types as Hack ( Entity(Entity_decl) )
import Glean.Schema.CodePython.Types as Python ( Entity(Entity_decl) )
import Glean.Schema.CodeThrift.Types as Thrift ( Entity(Entity_decl) )

-- Introduce a SymbolId. This is essentially the semantic "path to this symbol
-- in the Codex style. www/php/Glean/getLatestRepo
--
-- If we can't encode the entity, it is still useful to return the path and
-- file, as this is enough to navigate with.
--
-- Symbol IDs have 3 parts:
--
-- - scm repo (corpus in biggrep term), e.g. "fbsource"
-- - language short code (e.g py or php or cpp)
-- - entity encoding
--
-- We uri encode the pieces and separate with / so they look like nice urls
--
toSymbolId :: SymbolRepoPath -> Code.Entity -> Glean.RepoHaxl u w SymbolId
toSymbolId path entity = do
  let langCode = toShortCode (entityLanguage entity)
  eqname <- try $ toSymbolWithPath entity (symbolPath path)
  return $ case eqname of
    Left (SymbolError _e) -> symbol [repo, langCode, "SYMBOL_ID_MISSING"]
    Right spec -> symbol $ repo : langCode :
      map (URI.encodeTextWith isAllowed) spec
  where
    symbol = SymbolId . Text.intercalate "/"
    Glass.RepoName repo = symbolRepo path

    -- for readability, we permit these control chars in the symbol id uri
    isAllowed ':' = True
    isAllowed '+' = True
    isAllowed ',' = True
    isAllowed '*' = True
    isAllowed c = URI.isAllowed c

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

toSymbolLocalName :: Code.Entity -> Glean.RepoHaxl u w (Maybe Name)
toSymbolLocalName entity = do
  qname <- toQName entity
  return $ case qname of
    Right (qualifiedName_localName, _) -> Just qualifiedName_localName
    Left _ -> Nothing

toSymbolQualifiedContainer :: Code.Entity -> Glean.RepoHaxl u w (Maybe Name)
toSymbolQualifiedContainer entity = do
  qname <- toQName entity
  return $ case qname of
    Right (_, qualifiedName_container) -> Just qualifiedName_container
    Left _ -> Nothing

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
  , (Language_Java , "java")
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
  Code.Entity_thrift{} -> Language_Thrift
  Code.Entity_buck{} -> Language_Buck
  Code.Entity_erlang{} -> Language_Erlang
  -- lsif languages
  Code.Entity_lsif Lsif.Entity_go{} -> Language_Go
  Code.Entity_lsif Lsif.Entity_typescript{} -> Language_TypeScript
  Code.Entity_lsif Lsif.Entity_rust{} -> Language_Rust
  Code.Entity_lsif _ -> Language__UNKNOWN 0
  -- scip languages
  Code.Entity_scip Scip.Entity_rust{} -> Language_Rust
  Code.Entity_scip Scip.Entity_go{} -> Language_Go
  Code.Entity_scip _ -> Language__UNKNOWN 0
  Code.Entity_EMPTY -> Language__UNKNOWN 0

-- | Map the user-visible glass.thrift Language enum to the internal Glean
-- language id. This can be used for optional filtering in search.
languageToCodeLang :: Language -> Maybe Code.Language
languageToCodeLang l = case l of
  Language_Cpp -> Just Code.Language_Cpp
  Language_JavaScript -> Just Code.Language_JavaScript
  Language_Hack -> Just Code.Language_Hack
  Language_Haskell -> Just Code.Language_Haskell
  Language_Java -> Just Code.Language_Java
  Language_ObjectiveC -> Just Code.Language_Cpp -- we don't distinguish these
  Language_Python -> Just Code.Language_Python
  Language_PreProcessor -> Just Code.Language_PreProcessor
  Language_Thrift -> Just Code.Language_Thrift
  Language_Rust -> Just Code.Language_Rust
  Language_Buck -> Just Code.Language_Buck
  Language_Erlang -> Just Code.Language_Erlang
  Language_TypeScript -> Just Code.Language_TypeScript
  Language_Go -> Just Code.Language_Go
  Language__UNKNOWN{} -> Nothing

-- | Search queries for C++ should always imply the PreProcessor too
languageExpandCpp :: [Code.Language] -> [Code.Language]
languageExpandCpp [] = []
languageExpandCpp (Code.Language_Cpp : rest) = Code.Language_Cpp :
  Code.Language_PreProcessor : rest
languageExpandCpp (lang : rest) = lang : languageExpandCpp rest

-- | An encoded Entity.
--
-- e.g. Glean/getRepoName -- method
--      GleanRecursive   -- enum
--
instance Symbol Code.Entity where
  toSymbol _ = throwM $ SymbolError "Code.Entity: use toSymbolWithPath"

  toSymbolWithPath e p = case e of
    Code.Entity_hack (Hack.Entity_decl x) -> toSymbolWithPath x p
    Code.Entity_python (Python.Entity_decl x) -> toSymbolWithPath x p
    Code.Entity_flow x -> toSymbolWithPath x p
    Code.Entity_cxx x -> toSymbolWithPath x p
    Code.Entity_buck x -> toSymbolWithPath x p
    Code.Entity_erlang x -> toSymbolWithPath x p
    Code.Entity_hs x -> toSymbolWithPath x p
    Code.Entity_java x -> toSymbolWithPath x p
    Code.Entity_pp x -> toSymbolWithPath x p
    Code.Entity_thrift (Thrift.Entity_decl x) -> toSymbolWithPath x p
    Code.Entity_lsif ent -> case ent of -- enumerate all variants for lsif
      Lsif.Entity_erlang se -> toSymbolWithPath se p
      Lsif.Entity_fsharp se -> toSymbolWithPath se p
      Lsif.Entity_go se -> toSymbolWithPath se p
      Lsif.Entity_haskell se -> toSymbolWithPath se p
      Lsif.Entity_java se -> toSymbolWithPath se p
      Lsif.Entity_kotlin se -> toSymbolWithPath se p
      Lsif.Entity_ocaml se -> toSymbolWithPath se p
      Lsif.Entity_python se -> toSymbolWithPath se p
      Lsif.Entity_rust se -> toSymbolWithPath se p
      Lsif.Entity_scala se -> toSymbolWithPath se p
      Lsif.Entity_swift se -> toSymbolWithPath se p
      Lsif.Entity_typescript se -> toSymbolWithPath se p
      Lsif.Entity_EMPTY -> throwM $ SymbolError "Unknown LSIF language"

    Code.Entity_scip ent -> case ent of
      Scip.Entity_rust se -> toSymbolWithPath se p
      Scip.Entity_go se -> toSymbolWithPath se p
      Scip.Entity_EMPTY -> throwM $ SymbolError "Unknown SCIP language"

    -- Code.Entity_lsif (Lsif.Entity_java x) -> toSymbol x
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
  Code.Entity_pp x -> Right $
    alt @"pp" (toAngle x)
  Code.Entity_hs x -> Right $
    alt @"hs" (toAngle x)
  Code.Entity_erlang (Erlang.Entity_decl x) -> Right $
    alt @"erlang" (alt @"decl" (toAngle x))
  Code.Entity_buck x -> Right $
    alt @"buck" (toAngle x)
  Code.Entity_thrift (Thrift.Entity_decl x) -> Right $
    alt @"thrift" (alt @"decl" (toAngle x))
  -- lsif languages, enumerate all lang constructors
  Code.Entity_lsif se -> alt @"lsif" <$> case se of
      Lsif.Entity_erlang x -> Right $ alt @"erlang" (toAngle x)
      Lsif.Entity_fsharp x -> Right $ alt @"fsharp" (toAngle x)
      Lsif.Entity_go x -> Right $ alt @"go" (toAngle x)
      Lsif.Entity_haskell x -> Right $ alt @"haskell" (toAngle x)
      Lsif.Entity_java x -> Right $ alt @"java" (toAngle x)
      Lsif.Entity_kotlin x -> Right $ alt @"kotlin" (toAngle x)
      Lsif.Entity_ocaml x -> Right $ alt @"ocaml" (toAngle x)
      Lsif.Entity_python x -> Right $ alt @"python" (toAngle x)
      Lsif.Entity_rust x -> Right $ alt @"rust" (toAngle x)
      Lsif.Entity_scala x -> Right $ alt @"scala" (toAngle x)
      Lsif.Entity_swift x -> Right $ alt @"swift" (toAngle x)
      Lsif.Entity_typescript x -> Right $ alt @"typescript" (toAngle x)
      Lsif.Entity_EMPTY -> Left "toAngle: Unknown LSIF language"
  -- scip anguages, enumerate all lang constructors
  Code.Entity_scip se -> alt @"scip" <$> case se of
      Scip.Entity_rust x -> Right $ alt @"rust" (toAngle x)
      Scip.Entity_go x -> Right $ alt @"go" (toAngle x)
      Scip.Entity_EMPTY -> Left "toAngle: Unknown SCIP language"

  _ -> Left $
    "Unsupported language: " <> toShortCode (entityLanguage e)

instance ToQName Code.Entity where
  toQName e = case e of
    Code.Entity_python (Python.Entity_decl x) -> toQName x
    Code.Entity_hack (Hack.Entity_decl x) -> toQName x
    Code.Entity_flow x -> toQName x
    Code.Entity_cxx x -> toQName x
    Code.Entity_pp x -> toQName x
    Code.Entity_erlang x -> toQName x
    Code.Entity_java x -> toQName x
    Code.Entity_thrift (Thrift.Entity_decl x) -> toQName x
    Code.Entity_lsif se -> case se of -- enumerate all cases for lsif
      Lsif.Entity_erlang x -> toQName x
      Lsif.Entity_fsharp x -> toQName x
      Lsif.Entity_go x -> toQName x
      Lsif.Entity_haskell x -> toQName x
      Lsif.Entity_java x -> toQName x
      Lsif.Entity_kotlin x -> toQName x
      Lsif.Entity_ocaml x -> toQName x
      Lsif.Entity_python x -> toQName x
      Lsif.Entity_rust x -> toQName x
      Lsif.Entity_scala x -> toQName x
      Lsif.Entity_swift x -> toQName x
      Lsif.Entity_typescript x -> toQName x
      Lsif.Entity_EMPTY -> pure $ Left "LSIF: language unsupported"
    Code.Entity_scip se -> case se of -- enumerate all cases for lsif
      Scip.Entity_rust x -> toQName x
      Scip.Entity_go x -> toQName x
      Scip.Entity_EMPTY -> pure $ Left "SCIP: language unsupported"
    _ -> pure $ Left ("Language unsupported: " <> textShow (entityLanguage e))

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


nativeSymbol :: Code.Entity -> Glean.RepoHaxl u w (Maybe Text)
nativeSymbol (Code.Entity_cxx e) = toNativeSymbol e
nativeSymbol _ = return Nothing
