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
  , SearchFn
  , SymbolSearchData(..)
  , ToSearchResult(..)
  , RepoSearchResult
  , FeelingLuckyResult(..)
  , SingleSymbol

  -- * Search
  -- ** Search flags
  , SearchCase(..)
  , SearchType(..)
  , SearchQuery(..)
  , SearchScope(..)
  , SearchMode(..)
  , AngleSearch(..)
  , QueryExpr(..)

  -- ** Search query builder
  , buildSearchQuery
  , buildLuckyContainerQuery

  -- ** scoped search

  -- * Entity annotations
  , symbolKind

  -- * Query helpers
  , entityLocation

  -- * for tests
  , toScopeTokens
  , ScopeQuery(..)

  ) where

import Data.Text (Text, toLower)
import qualified Data.Text as Text
import Data.Maybe
import Data.List.NonEmpty as NonEmpty (NonEmpty(..), toList)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Glean
import Glean.Angle as Angle
import Glean.Haxl.Repos (RepoHaxl)

import Glean.Glass.Base (GleanPath(..))
import Glean.Glass.Types (SymbolResult(..), SymbolDescription(..))
import Glean.Glass.Utils (splitOnAny, QueryType )

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

-- | Whether to parse the string at all
data SearchScope = NoScope | Scope

-- | Search modes: unique/feeling lucky or full search
data SearchMode = Normal | FeelingLucky

-- | Abstract over search predicates
data AngleSearch = forall p . ( QueryType p, ToSearchResult p) =>
  Search (QueryExpr p)

-- | Either a fully evaluated angle query
data QueryExpr p
  = Complete (Angle p)
  | InheritedScope {
      _scopeCase :: SearchCase,
      _baseScopeTerm :: NonEmpty Text,
      _searchFun :: SearchCase -> [NonEmpty Text] -> Angle p
  }

-- | Named arguments to the search query builder. These come from the client
data SearchQuery = SearchQuery {
  sType :: SearchType,
  sCase :: SearchCase,
  sScope :: SearchScope,
  sString :: Text,
  sKinds :: [Code.SymbolKind],
  sLangs :: [Code.Language]
}

--
-- | Given user preferences for search type, kind and language, and the query
-- string, choose a list of queries to run in order. results will be combined in
-- the order of specified.
--
-- We combine a few search policies here.
--
-- - prefix searches, we prefer exact matches first. Guarantee that by running
-- an exact match query
-- - scope searches with prefix can be expensive, so mark as Slow, which will
-- put a time limit on the query
-- - searches with namespace tokens that parse as names (e.g. ::foo) get
-- re-routed to a name search
--
-- Finding entities by string search. Base layer in Glean
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
-- Search for symbols in namespaces
--
-- > folly::Optional
-- > ::Optional
-- > folly::
-- > HH\map
-- > ::folly::
-- > facebook::folly::Optional
--
-- We go to some effort to rank query results so that "C" or "Vec" apear
-- early (i.e. namesspaces), and in qnames, exact matches win.
--
buildSearchQuery :: SearchMode -> SearchQuery -> [AngleSearch]
buildSearchQuery Normal query =
  map (compileQuery (sKinds query) (sLangs query)) (toRankedSearchQuery query)
buildSearchQuery FeelingLucky query =
  map (compileQuery (sKinds query) (sLangs query)) (feelingLuckyQuery query)

-- | For nested inherited queries, we have to do an inner container search
buildLuckyContainerQuery :: SearchQuery -> NonEmpty Text -> [AngleSearch]
buildLuckyContainerQuery query scope =
  map (compileQuery (sKinds query) (sLangs query))
    (feelingLuckyContainer (sCase query) scope)

compileQuery
  :: [Code.SymbolKind] -> [Code.Language] -> SearchExpr -> AngleSearch
compileQuery sKinds sLangs e = case e of
  -- most precise
  -- in feeling lucky mode, free functions in the global scoped are
  -- considered as important as classes.
    GlobalScopedPriorityKinds sCase name -> scopeQ Exact sCase Nothing name
      (Code.SymbolKind_Function : containerishKinds)
    GlobalScoped sCase name ->
      scopeQ Exact sCase Nothing name sKinds
  -- becoming more imprecise
    ContainerLiteral sCase name ->
      nameQ Exact sCase name containerKinds
    GlobalScopedPrefixly sCase name ->
      scopeQ Prefix sCase Nothing name sKinds
    Literal sCase name ->
      nameQ Exact sCase name sKinds
    Prefixly sCase name ->
      nameQ Prefix sCase name sKinds
    -- quite precise but different
    Scoped sCase scope name ->
      scopeQ Exact sCase (Just scope) name sKinds
    ScopedPrefixly sCase scope name ->
      scopeQ Prefix sCase (Just scope) name sKinds
    -- meta-query
    InheritedScoped sCase scope name ->
      inheritedQ Exact sCase scope name sKinds
  where
    containerKinds = if null sKinds then containerishKinds else sKinds

    -- two main search predicates: by name or by name and scope
    nameQ sTy sCa name kinds = Search $ Complete $ codeSearchByName $
      compileSearchQ sTy sCa (Just name) Nothing kinds sLangs
    scopeQ sTy sCa scopes name kinds = Search $ Complete $ codeSearchByScope $
      compileSearchQ sTy sCa (Just name) scopes kinds sLangs
    inheritedQ sTy sCa scopes name kinds =
      let (Inherited term, searchFn) = codeSearchByInheritedScope $
            compileInheritedSearchQ sTy sCa name scopes kinds sLangs
      in Search $ InheritedScope sCa term searchFn

-- | Common "API"-level things, you could reasonably expect to be ranked highly
containerishKinds :: [Code.SymbolKind]
containerishKinds =
  [ Code.SymbolKind_Namespace
  , Code.SymbolKind_Class_
  , Code.SymbolKind_Trait
  , Code.SymbolKind_Interface
  , Code.SymbolKind_Module
  ]

--
-- The major search styles, in policy "accuracy" order
--
data SearchExpr
  -- literal name searches
  = ContainerLiteral SearchCase Text -- exactly "Vec" or "vec" of kind:namespace
  | GlobalScopedPriorityKinds SearchCase Text -- exactly "vec" the class in HH
  | GlobalScoped SearchCase Text -- exactly "genmk" in the global scope
  | GlobalScopedPrefixly SearchCase Text -- prefix e.g. "genm".. in global scope
  | Literal SearchCase Text  -- exactly "Vec" or "C" (or "vec" or "VEC" or "Vec"
  | Prefixly SearchCase Text   -- "vec".. or "Vec".. etc any case
  -- scope searches (ignoring globals)
  | Scoped SearchCase (NonEmpty Text) Text -- "Vec\sort"
  | ScopedPrefixly SearchCase (NonEmpty Text) Text   -- "Vec\so"..
  -- More interesting: inherited scope terms, anchored to a base scope
  | InheritedScoped SearchCase (NonEmpty Text) Text -- "Vec\sort"

-- we do namespace (or class?) first if no kind filter, or kinds included
searchContainer :: Text -> [Code.SymbolKind] -> SearchCase -> [SearchExpr]
searchContainer sName sKinds sCase
  | null sKinds
  = [ContainerLiteral sCase sName]
  | otherwise
  = []

-- for global literal searches, case-sensitive matches beat insensitive matches
searchGlobal :: Text -> SearchCase -> [SearchExpr]
searchGlobal sName sCase = case sCase of
  Sensitive -> [GlobalScoped Sensitive sName]
  Insensitive -> [GlobalScoped Sensitive sName, GlobalScoped Insensitive sName]

-- Prefix search requested (default in codehub)
searchGlobalPrefix :: Text -> SearchType -> SearchCase -> [SearchExpr]
searchGlobalPrefix sName sType sCase = case sType of
  Prefix -> [GlobalScopedPrefixly sCase sName]
  Exact -> []

-- for literal searches, case-sensitive matches beat insensitive matches
searchLiteral :: Text -> SearchCase -> [SearchExpr]
searchLiteral sName sCase = case sCase of
  Sensitive -> [Literal Sensitive sName]
  Insensitive -> [Literal Sensitive sName, Literal Insensitive sName]

-- Prefix search requested (default in codehub)
searchPrefix :: Text -> SearchType -> SearchCase -> [SearchExpr]
searchPrefix sName sType sCase = case sType of
  Prefix -> [Prefixly sCase sName]
  Exact -> []

-- Scope search, exact or if prefix, then exact first
searchScope
  :: NonEmpty Text -> Text -> SearchType -> SearchCase -> [SearchExpr]
searchScope scope name sType sCase = case sType of
  Exact ->
      [ Scoped sCase scope name
      , InheritedScoped sCase scope name
      ]
  -- CodeHub default: lets always make sure to do an exact case search
  Prefix -> case sCase of
    Sensitive ->
      [ Scoped Sensitive scope name
      , InheritedScoped Sensitive scope name
      , ScopedPrefixly Sensitive scope name
      ]
    -- CodeHub default symbol search is prefixly, case insensitive
    Insensitive ->
      [ Scoped Sensitive scope name -- always aim for exact matches to appear
      , Scoped Insensitive scope name -- even if there's a lot of matches
      , InheritedScoped Sensitive scope name
      , InheritedScoped Insensitive scope name
      , ScopedPrefixly sCase scope name
      ]

-- | Ranked symbol search types.
-- Refine the query flags to ranking policy.
--
-- 0. exact namespace (e.g. "Vec" or "C" or "vec" or "c")
-- 1. maybe scope match (e.g. "Vec\sort" or "folly::optional")
-- 2. exact string match (e.g. "Optional")
-- 4. prefix match (e.g. "option..")
--
toRankedSearchQuery :: SearchQuery -> [SearchExpr]
toRankedSearchQuery query@SearchQuery{..} = case sScope of
  -- scope search. glass search -s  , and default on codehub
  Scope -> case toScopeTokens sString of
    Just scopeQ -> searchScopes scopeQ query
    Nothing -> toRankedSearchQuery (query { sScope = NoScope })

  -- not a scope search. i.e. glass cli default
  -- we will try to match strictly against the local name of the identifier
  NoScope -> searchGlobal sString sCase
     <> searchContainer sString sKinds sCase
     <> searchLiteral sString sCase
     <> searchGlobalPrefix sString sType sCase
     <> searchPrefix sString sType sCase

searchScopes :: ScopeQuery -> SearchQuery -> [SearchExpr]
searchScopes scopeQ SearchQuery{..} = case scopeQ of
  -- parses as valid qname. we could also try searching for the fragments as
  -- literals but unclear if that's useful. could search for scope symbols too
  ScopeAndName scope name ->
       searchScope scope name sType sCase
    <> searchLiteral sString sCase
  -- could be a container as well or a sub-namespace
  ScopeOnly scope@(_ :| (_ : _)) -> -- i.e length > 1
      searchScope (NonEmpty.fromList (NonEmpty.init scope))
         (NonEmpty.last scope) sType sCase
      <> searchLiteral sString sCase
    -- singleton scope term (like 'foo::')
  ScopeOnly (name :| []) ->
        searchContainer name sKinds sCase
    <> searchLiteral name sCase
    <> searchLiteral sString sCase
    <> searchPrefix sString sType sCase

  NameOnly name -> searchLiteral name sCase <> searchPrefix name sType sCase

-- | In feeling lucky mode, we do concurrent precise searches
-- then return the first of the exact matches we hit in priority order
-- We only need to search with limit=2 for these, so they can be fast and we
-- can do more of them
feelingLuckyQuery :: SearchQuery -> [SearchExpr]
feelingLuckyQuery query@SearchQuery{..} = case sScope of
  -- try to parse as a scope query first
  Scope ->
    case toScopeTokens sString of
      -- valid qname
      Just (ScopeAndName scope name) ->
        luckyScope scope name ++
        feelingLuckyQuery (query { sScope = NoScope }) -- plus try as literal

      -- qname prefix, non-singleton
      Just (ScopeOnly scopeOnly@(_ :| (_ : _))) ->
        let scope = NonEmpty.fromList (NonEmpty.init scopeOnly)
            name = NonEmpty.last scopeOnly
        in
          luckyScope scope name ++
          feelingLuckyQuery (query { sScope = NoScope }) -- plus try as literal

      -- prefix part (e.g. `Foo::`)
      Just (ScopeOnly (name :| [])) ->
        feelingLuckyQuery (query {
          sScope = NoScope,
          sString = name }
        )
      -- suffix part (e.g. `::bar`)
      Just (NameOnly name) ->
        feelingLuckyQuery (query { sScope = NoScope, sString = name })

      -- doesn't parse, just usual literal search
      Nothing -> feelingLuckyQuery (query { sScope = NoScope })

  -- not a scope search. i.e. glass cli default
  -- we will try to match strictly against the local name of the identifier
  -- I think we should be respecting the 'case sensitive flag here?
  NoScope ->
    [ GlobalScopedPriorityKinds Sensitive sString -- "vec" the class
    , GlobalScoped Sensitive sString -- "genmk", global "C"
    , ContainerLiteral Sensitive sString -- "C" or "vec"
    , GlobalScoped Insensitive sString -- "Vec" vs "vec"
    , ContainerLiteral Insensitive sString
    , Literal Sensitive sString
    , Literal Insensitive sString
    , GlobalScopedPrefixly Sensitive sString
    , GlobalScopedPrefixly Insensitive sString
    , Prefixly Sensitive sString  -- any sensitive prefix
    , Prefixly Insensitive sString  -- any insensitive prefix, broadest possible
    ]
  where
    luckyScope scope name =
      [ Scoped Sensitive scope name
      , Scoped Insensitive scope name
      , InheritedScoped Sensitive scope name
      , InheritedScoped Insensitive scope name
      , ScopedPrefixly Sensitive scope name
      , ScopedPrefixly Insensitive scope name
      ]

-- | For inherited scope terms we need to a (nested) guess as to the parent
-- scope to seed the search. It's like feelingLucky, but we know it has to be
-- a container-ish thing
feelingLuckyContainer :: SearchCase -> NonEmpty Text -> [SearchExpr]
feelingLuckyContainer sCase scopes = case scopes of
  container :| [] ->
    [ GlobalScopedPriorityKinds Sensitive container
    , ContainerLiteral Sensitive container
    , GlobalScoped Sensitive container
    , GlobalScopedPriorityKinds Insensitive container
    , ContainerLiteral Insensitive container
    , GlobalScoped Insensitive container -- if sCase == Sensitive we can skip
    , Literal Sensitive container
    , Literal Insensitive container
    ]
  -- "a::b::c" -> ([a,b], c)
  _ :| _ -> case (NonEmpty.init scopes, NonEmpty.last scopes) of
    (path, container) -> case path of
      [] -> feelingLuckyContainer sCase (container :| [])
      (p:ps) ->
        let nonEmptyPath = p :| ps
        in
          [ Scoped Sensitive nonEmptyPath container
          , Scoped Insensitive nonEmptyPath container
          ] -- or assume the path is wrong and try to find container?
          ++ feelingLuckyContainer sCase (container :| [])

-- | Search params compiled to Angle expressions
data SearchQ q = SearchQ {
    nameQ :: Angle Text,
    caseQ  :: Angle CodeSearch.SearchCase,
    scopeQ :: q,
    mKindQ :: Maybe (Angle Code.SymbolKind),
    mLangQ :: Maybe (Angle Code.Language)
  }

-- | Regular searches compile into search terms directly
newtype Direct = Direct { scopeTerm :: Maybe (Angle [Text]) }

-- | Inherited scope terms are suspended into a staged computation
-- that produces terms for a new direct query
--
-- We won't try to support prefix versions of this
--
newtype Inherited = Inherited {
    _scopeTerm :: NonEmpty Text -- base entity of the search
  }

--
-- Translate our structured search values into Angle expressions
--
compileSearchQ
  :: SearchType -> SearchCase -> Maybe Text
  -> Maybe (NonEmpty Text)
  -> [Code.SymbolKind] -> [Code.Language] -> SearchQ Direct
compileSearchQ = compileAnySearchQ id (\a b -> Direct (toScopeQuery a b))

--
-- Inherited searches: the scope term is non-empty and never Nothing/wild/global
--
compileInheritedSearchQ
  :: SearchType -> SearchCase -> Text -> NonEmpty Text
  -> [Code.SymbolKind] -> [Code.Language] -> SearchQ Inherited
compileInheritedSearchQ = compileAnySearchQ Just (const Inherited)

--
-- Or translate into a suspended computation
--
compileAnySearchQ
  :: (name -> Maybe Text)
  -> (SearchCase -> scope -> scopeTerm)
  -> SearchType -> SearchCase
  -> name
  -> scope
  -> [Code.SymbolKind] -> [Code.Language] -> SearchQ scopeTerm
compileAnySearchQ nameFn scopeFn sType sCase name scope kinds langs =
    SearchQ{..}
  where
    nameQ = toNameQuery sType sCase (nameFn name)
    scopeQ = scopeFn sCase scope
    caseQ = toCaseQuery sCase
    mKindQ = toEnumSet kinds
    mLangQ = toEnumSet langs

-- We support scope insensitive queries, so check that
toScopeQuery :: SearchCase -> Maybe (NonEmpty Text) -> Maybe (Angle [Text])
toScopeQuery _ Nothing = Nothing -- corresponds to a global scoped search
toScopeQuery sCase (Just scope) = Just (array (map string scopeLits))
  where
    scopeLits = case sCase of
      Sensitive -> toList scope
      Insensitive -> map toLower (toList scope)

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
toScopeTokens str = go (splitOnAny delimiters str)
  where
    -- these are applied in order
    delimiters = ["::", "\\", "."]

    -- first match with 2 or more scope fragments
    go :: [Text] -> Maybe ScopeQuery
    go [] = Nothing
    go [_] = Nothing -- i.e. this isn't a scope query
    go toks@(_:_:_) = case (dropWhile Text.null (init toks), last toks) of
      ([],"") -> Nothing -- i.e. "::"
      ([],name) -> Just (NameOnly name) -- i.e. ::b
      (s:ss, "") -> Just (ScopeOnly (s :| ss)) -- i.e. a:: or ::a::
      (s:ss, name) -> Just (ScopeAndName (s :| ss) name) -- a::b::c or ::a::b

--
-- Actual Glean queries for search
--

--
-- Find entities by strings, with an optional kind expression filter
--
codeSearchByName :: SearchQ Direct -> Angle CodeSearch.SearchByName
codeSearchByName SearchQ{..} =
  vars $ \p (k :: Angle (Maybe Code.SymbolKind)) ->
    p `where_` [
      k .= sig kindPat,
      p .= predicate @CodeSearch.SearchByName (
        rec $
          field @"name" nameQ $ -- may be prefix
          field @"searchcase" caseQ $
          field @"kind" k $ -- specific kinds only
          field @"language" languagePat -- optional language filters
        end)
    ]
  where
    kindPat = maybe wild just mKindQ
    languagePat = fromMaybe wild mLangQ

--
-- Find entities by name in tokenized namespace
--
codeSearchByScope :: SearchQ Direct -> Angle CodeSearch.SearchByScope
codeSearchByScope SearchQ{..} =
  vars $ \p (k :: Angle (Maybe Code.SymbolKind)) ->
    p `where_` [
      k .= sig kindPat,
      p .= predicate @CodeSearch.SearchByScope (
        rec $
          field @"name" nameQ $
          field @"scope" scopePat $
          field @"searchcase" caseQ $
          field @"kind" k $ -- specific kinds only
          field @"language" languagePat -- optional language filters
        end)
    ]
  where
    scopePat = fromMaybe (array []) (scopeTerm scopeQ) -- Nothing ==global scope
    kindPat = maybe wild just mKindQ
    languagePat = fromMaybe wild mLangQ

--
-- A suspended code search by scope with disjoint scope terms
-- It's like a regular codeSearchByScope, but we evaluate the scope term
-- to a set externally first, then subsitute into the angle query
--
codeSearchByInheritedScope
  :: SearchQ Inherited
  -> (Inherited,
      SearchCase -> [NonEmpty Text] -> Angle CodeSearch.SearchByScope)
codeSearchByInheritedScope SearchQ{..} = (scopeQ, queryFn)
  where
    kindPat = maybe wild just mKindQ
    languagePat = fromMaybe wild mLangQ

    queryFn = \caseTerm scopeTerm ->
      let
        scopePat = fromMaybe (array []) (toScopeSetQuery caseTerm scopeTerm)
      in
        vars $ \p (s :: Angle [Text]) ->
          p `where_` [
            s .= scopePat,
            p .= predicate @CodeSearch.SearchByScope (
              rec $
                field @"name" nameQ $
                field @"scope" s $
                field @"searchcase" caseQ $
                field @"kind" kindPat $ -- specific kinds only
                field @"language" languagePat -- optional language filters
              end
            )
          ]

-- | Scope terms expanded into set of names, then compiled to disjoint query
--
-- Compile set of terms to regular scope queries, then compose them with .|
--
toScopeSetQuery :: SearchCase -> [NonEmpty Text] -> Maybe (Angle [Text])
toScopeSetQuery _ [] = Nothing
toScopeSetQuery sCase ts =  Just $ foldr1 (.|) (catMaybes scopeQs)
  where
    scopeQs = map (toScopeQuery sCase . Just) ts

--
-- A class for converting the different search predicates to a standard format
--

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
  -- normalize predicate to search result type
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
type RepoSearchResult = [SingleSymbol]

-- An un-concatenated set of query results to search for unique hits in
-- within one scm repo, across dbs, across queries, a set of result symbols.
newtype FeelingLuckyResult = FeelingLuckyResult [[RepoSearchResult]]

type SingleSymbol = (SymbolResult,Maybe SymbolDescription)

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
