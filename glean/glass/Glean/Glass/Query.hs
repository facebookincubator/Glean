{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Query
  (
  -- * Files
    srcFile

  -- * Working with XRefs
  , fileEntityLocations
  , fileEntityXRefLocations

  -- * Finding refernces to declarations
  , findReferenceRangeSpan

  -- * offsets and conversions to lines
  , fileLines

  -- * Search
  , searchByName
  , SearchCase(..)
  , SearchType(..)
  , SearchFn
  , SymbolSearchData(..)
  , ToSearchResult(..)
  , RepoSearchResult(..)

  -- ** scoped search
  , searchByScope
  , toScopeTokens
  , ScopeQuery(..)

  -- * Entity annotations
  , symbolKind

  -- * Query helpers
  , entityLocation

  ) where

import Data.Text (Text, toLower)
import qualified Data.Text as Text
import Data.Maybe
import Data.List.NonEmpty (NonEmpty(..), toList)

import qualified Glean
import Glean.Angle as Angle
import Glean.Haxl.Repos (RepoHaxl)

import Glean.Glass.Base (GleanPath(..))
import Glean.Glass.Types (SymbolResult(..), SymbolDescription(..))

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.CodemarkupSearch.Types as CodeSearch
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src

--
-- Find the id in this db of a source file. We mostly operate on these ids once
-- we have them, to avoid passing strings around.
--
-- > src.File "www/flib/intern/glean/Glean.php"
--
srcFile :: GleanPath -> Angle Src.File
srcFile (GleanPath path) =
  predicate @Src.File $
    string path

-- | Given a file id, look up the index of line endings
--
-- Line ending tables are needed to do line:col conversions, but we want to
-- minimize when we have to do this conversion. The Range vs Location types in
-- the .thrift API capture when this is needed.
--
fileLines :: Glean.IdOf Src.File -> Angle Src.FileLines
fileLines fileid =
  predicate @Src.FileLines (
    rec $ field @"file"
      (asPredicate (factId fileid))
    end)

-- | Get the definition entities defined in this file, and their declaration
-- sites.
--
-- > www> src.File "www/flib/intern/glean/Glean.php"
-- { "id": 123968960, "key": "www/flib/intern/glean/Glean.php" }
--
-- > {L , E} where
-- >  codemarkup.FileEntityLocations {$123968960, L, E}
--
fileEntityLocations :: Glean.IdOf Src.File -> Angle (Code.Location, Code.Entity)
fileEntityLocations fileid =
  vars $ \(location :: Angle Code.Location) (entity :: Angle Code.Entity) ->
    tuple (location,entity) `where_` [
      wild .= predicate @Code.FileEntityLocations (
        rec $
          field @"file" (asPredicate (factId fileid)) $
          field @"location" location $
          field @"entity" entity
        end)
      ]

-- | Find xrefs from this file, and their associated entities.
--
-- > www> src.File "www/flib/intern/glean/Glean.php"
-- > { "id": 885230, "key": "www/flib/intern/glean/Glean.php" }
--
-- > www> {X,E} where
-- >   codemarkup.FileEntityXRefLocations {file = $885230, xref = X, entity = E}
--
fileEntityXRefLocations
  :: Glean.IdOf Src.File -> Angle (Code.XRefLocation, Code.Entity)
fileEntityXRefLocations fileid =
  vars $ \(xref :: Angle Code.XRefLocation) (entity :: Angle Code.Entity) ->
    tuple (xref,entity) `where_` [
      wild .= predicate @Code.FileEntityXRefLocations (
        rec $
          field @"file" (asPredicate (factId fileid)) $
          field @"xref" xref $
          field @"entity" entity
        end)
      ]

-- | Entity-based find-references returning native range or bytespan
findReferenceRangeSpan
  :: Angle Code.Entity
  -> Angle (Src.File, Code.RangeSpan)
findReferenceRangeSpan ent =
  vars $ \(reffile :: Angle Src.File) (rangespan :: Angle Code.RangeSpan) ->
    Angle.tuple (reffile, rangespan) `where_` [
      wild .= predicate @Code.EntityReferences (
      rec $
          field @"target" ent $
          field @"file" (asPredicate reffile) $
          field @"range" rangespan
        end
      )
    ]

--
-- Finding entities by name search
--

-- | Which search
data SearchType = Exact | Prefix

-- | Whether we care about case
data SearchCase = Sensitive | Insensitive

--
-- | Finding entities by string search. Base layer in Glean
--
-- There are four main flavors
--
-- > "Glean" , exact and sensitive
-- > "glean" , exact and case insensitive
-- > "Gle".. , prefix and sensitive
-- > "GLE".. , prefix and insensitive (most generic)
--
-- Additionally, you can filter on the server side by kind. Empty list of
-- kinds will return all matching entities, with or without kinds.
--
-- You can also filter on the server by language id.
--
-- In all cases we return a basic triple of (entity, location, maybe kind)
--
searchByName
  :: SearchType
  -> SearchCase
  -> Text
  -> [Code.SymbolKind]
  -> [Code.Language]
  -> Angle CodeSearch.SearchByName

searchByName sType sCase name kinds langs =
  codeSearchByName $ toSearchQ sType sCase (Just name) Nothing kinds langs

--
-- | Search for symbols in namespaces
--
-- > folly::Optional
-- > ::Optional
-- > folly::
-- > HH\map
-- > ::folly::
-- > facebook::folly::Optional
--
searchByScope
  :: SearchType
  -> SearchCase
  -> ScopeQuery
  -> [Code.SymbolKind]
  -> [Code.Language]
  -> Either (Angle CodeSearch.SearchByScope) (Angle CodeSearch.SearchByName)

searchByScope sType sCase scopeQ kinds langs = case scopeQ of
  ScopeAndName scope name ->
    Left $ codeSearchByScope $ query (Just scope) (Just name)
  ScopeOnly scope -> -- scope-only wild cards are a bit experimental. disable
   --  Left $ codeSearchByScope $ query (Just scope) Nothing
    Right $ codeSearchByName $ query Nothing (Just (last $ toList scope))
  NameOnly name -> -- after tokenziation we can do a name-only search
    Right $ codeSearchByName $ query Nothing (Just name)
  where
    query scopes names = toSearchQ sType sCase names scopes kinds langs

data SearchQ = SearchQ {
    nameQ :: Angle Text,
    caseQ  :: Angle CodeSearch.SearchCase,
    scopeQ :: Maybe (Angle [Text]),
    mKindQ :: Maybe (Angle Code.SymbolKind),
    mLangQ :: Maybe (Angle Code.Language)
  }

toSearchQ
   :: SearchType
   -> SearchCase
   -> Maybe Text
   -> Maybe (NonEmpty Text)
   -> [Code.SymbolKind]
   -> [Code.Language]
   -> SearchQ
toSearchQ sType sCase name scope kinds langs = SearchQ{..}
  where
    nameQ = toNameQuery sType sCase name
    scopeQ = array . map string . toList <$> scope
    caseQ = toCaseQuery sCase
    mKindQ = toEnumSet kinds
    mLangQ = toEnumSet langs

toNameQuery :: SearchType -> SearchCase -> Maybe Text -> Angle Text
toNameQuery _ _ Nothing = wild
toNameQuery sType sCase (Just name)= case sType of
    Exact -> string nameLit
    Prefix -> stringPrefix nameLit
  where
    nameLit = case sCase of
      Sensitive -> name
      Insensitive -> toLower name

toEnumSet :: AngleEnum a => [a] -> Maybe (Angle (AngleEnumTy a))
toEnumSet langs = case langs of
  [] -> Nothing -- n.b. unconstrained
  ls -> Just $ foldr1 (.|) (map enum ls) -- restrict to specific langs

toCaseQuery :: SearchCase -> Angle CodeSearch.SearchCase
toCaseQuery sCase = enum $ case sCase of
  Sensitive -> CodeSearch.SearchCase_Sensitive
  Insensitive -> CodeSearch.SearchCase_Insensitive

-- | Variants of the scope query syntax
data ScopeQuery
  = ScopeAndName (NonEmpty Text) !Text -- ::a::b::ident
  | ScopeOnly (NonEmpty Text) -- ::a::b:: or a::
  | NameOnly !Text -- ::a
  deriving (Eq, Show)

-- | If it looks like we can parse this as a qualified name, then do that
-- We want to avoid any "" or wild cards appearing until perf is ok.
toScopeTokens :: Text -> Maybe ScopeQuery
toScopeTokens name = go $ map (`Text.splitOn` name) delimiters
  where
    -- we probably should have a class of per-language parsers for qnames
    delimiters = ["::", "\\"]

    -- first match with 2 or more scope fragments
    go :: [[Text]] -> Maybe ScopeQuery
    go [] = Nothing
    go (toks@(_:_:_):_) = case (dropWhile Text.null (init toks), last toks) of
      ([],"") -> Nothing -- i.e. "::"
      ([],name) -> Just (NameOnly name) -- i.e. ::b
      (s:ss, "") -> Just (ScopeOnly (s :| ss)) -- i.e. a:: or ::a::
      (s:ss, name) -> Just (ScopeAndName (s :| ss) name) -- a::b::c or ::a::b
    go (_:rest) = go rest

--
-- Find entities by strings, with an optional kind expression filter
--
codeSearchByName :: SearchQ -> Angle CodeSearch.SearchByName
codeSearchByName SearchQ{..} =
  predicate @CodeSearch.SearchByName $
    rec $
      field @"name" nameQ $ -- may be prefix
      field @"searchcase" caseQ $
      field @"kind" kindPat $ -- specific kinds only
      field @"language" languagePat -- optional language filters
    end
  where
    kindPat = maybe wild just mKindQ
    languagePat = fromMaybe wild mLangQ

--
-- Find entities by name in tokenized namespace
--
codeSearchByScope :: SearchQ -> Angle CodeSearch.SearchByScope
codeSearchByScope SearchQ{..} =
  predicate @CodeSearch.SearchByScope $
    rec $
      field @"name" nameQ $
      field @"scope" scopePat $
      field @"searchcase" caseQ $
      field @"kind" kindPat $ -- specific kinds only
      field @"language" languagePat -- optional language filters
    end
  where
    scopePat = fromMaybe (array []) scopeQ
    kindPat = maybe wild just mKindQ
    languagePat = fromMaybe wild mLangQ

type SearchFn
  = SearchCase
  -> Text
  -> [Code.SymbolKind]
  -> [Code.Language]
  -> Angle CodeSearch.SearchByName

-- | Glean-specific types from symbol search
-- note could return the language id but its also cheap to recompute from entity
data SymbolSearchData = SymbolSearchData
  { srEntity :: !Code.Entity
  , srLocation :: !Code.Location
  , srKind :: !(Maybe Code.SymbolKind)
  }

-- | We need most of the result of the predicate, so rather than a
-- data query we just unmarshall here and call the predicate directly
class ToSearchResult a where
  toSearchResult :: a -> RepoHaxl u w SymbolSearchData

instance ToSearchResult CodeSearch.SearchByName where
  toSearchResult p = do
    CodeSearch.SearchByName_key{..} <- Glean.keyOf p
    pure $ SymbolSearchData
      searchByName_key_entity
      searchByName_key_location
      searchByName_key_kind

instance ToSearchResult CodeSearch.SearchByScope where
  toSearchResult p = do
    CodeSearch.SearchByScope_key{..} <- Glean.keyOf p
    pure $ SymbolSearchData
      searchByScope_key_entity
      searchByScope_key_location
      searchByScope_key_kind

-- | Type of processed search results from a single scm repo
newtype RepoSearchResult =
  RepoSearchResult {
    unRepoSearchResult :: [(SymbolResult,Maybe SymbolDescription)]
  }

-- | Given an entity find the kind associated with it
symbolKind :: Angle Code.Entity -> Angle Code.SymbolKind
symbolKind ent = var $ \kind -> kind `where_`
  [ wild .= predicate @Code.EntityKind (
      rec $
          field @"entity" ent $
          field @"kind" kind
      end)
  ]

-- | Helper to generate entity location footers on entity search queries
-- Takes free file and rangespan variables to bind
entityLocation
  :: Angle Code.Entity
  -> (Angle Src.File -> Angle Code.RangeSpan -> Angle Text -> AngleStatement)
entityLocation entity file rangespan name =
  wild .= predicate @Code.EntityLocation (
    rec $
      field @"entity" entity $
      field @"location" (
        rec $
          field @"name" name $
          field @"file" (asPredicate file) $
          field @"location" rangespan
        end)
    end)
