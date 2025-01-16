{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Handler.Symbols
  (
  -- ** find references
   findReferenceRanges

  -- * working with symbol ids
  , symbolLocation
  , describeSymbol

  -- * searching by string
  , searchSymbol

  -- ** by relationship
  , searchRelated

  -- ** a large report of all information we know about a symbol
  , searchRelatedNeighborhood

  ) where

import Control.Monad
import Control.Exception ( throwIO )
import Control.Monad.Catch ( throwM )
import Data.Bifunctor (second)
import Data.Either.Extra (eitherToMaybe)
import Data.Hashable
import Data.List as List ( sortOn )
import Data.List.Extra ( nubOrd, nubOrdOn, groupOn, groupSortOn )
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Ord ( comparing )
import Data.Text ( Text )
import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Control.Concurrent.Async as Async
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Util.List ( uniq, uniqBy )


import Glean.Angle as Angle ( Angle )
import qualified Glean
import Glean.Haxl as Glean ( keyOf, haxlRepo )
import Glean.Haxl.Repos as Glean

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code

import Glean.Glass.Base
import Glean.Glass.Describe ( mkSymbolDescription, describeEntity )
import Glean.Glass.Handler.Utils
import Glean.Glass.Logging
import Glean.Glass.Repos
import Glean.Glass.Path ( fromGleanPath )
import Glean.Glass.Range
import Glean.Glass.SymbolId
import Glean.Glass.Types
import Glean.Glass.RepoMapping (
   supportsCxxDeclarationSources
  )
import Glean.Glass.Search.Class ( ResultLocation )
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Query as Query
import Glean.Glass.NameSearch (
    SearchCase(..), SearchType(..),
    SearchScope(..), SearchMode(..),
    SearchQuery(..), SingleSymbol, FeelingLuckyResult(..),
    QueryExpr(..), RepoSearchResult, SymbolSearchData(..),
    toSearchResult, ToSearchResult(..), AngleSearch(..), srEntity,
    buildLuckyContainerQuery, buildSearchQuery, dedupSearchResult
  )
import Glean.Glass.XRefs ( GenXRef(..) )
import Glean.Glass.Search as Search
    ( CodeEntityLocation(..),
      SearchEntity(..),
      SearchResult(Many, None, One, rest, message, initial),
      searchEntity,
      searchEntityLocation )
import Glean.Glass.Utils
import Glean.Glass.Attributes.SymbolKind
    ( symbolKindToSymbolKind, symbolKindFromSymbolKind )
import qualified Glean.Glass.SearchRelated as Search
import Glean.Glass.SearchRelated
    ( RelatedLocatedEntities(childRL, parentRL),
      Recursive(NotRecursive) )
import Glean.Glass.Neighborhood ( searchNeighborhood )
import qualified Glean.Glass.Handler.Utils as Utils

import Glean.Glass.SymbolKind (findSymbolKind)
import Glean.Glass.Env (Env' (tracer, sourceControl, useSnapshotsForSymbolsList))

-- | Symbol-based find-references.
findReferenceRanges
  :: Glass.Env -> SymbolId -> RequestOptions -> IO [LocationRange]
findReferenceRanges env@Glass.Env{..} sym opts@RequestOptions{..} =
  withSymbol "findReferenceRanges" env opts sym
    $ \gleanDBs _dbInfo (repo, lang, toks) ->
      fetchSymbolReferenceRanges env repo lang toks limit
        GleanBackend{..}
  where
    limit = fmap fromIntegral requestOptions_limit

-- | Resolve a symbol identifier to its range-based location in the latest db
-- This is about 10x cheaper than a full describeSymbol() call
symbolLocation :: Glass.Env -> SymbolId -> RequestOptions -> IO SymbolLocation
symbolLocation env@Glass.Env{..} sym opts =
  withSymbol "symbolLocation" env opts sym $
    \gleanDBs dbInfo (scmRepo, lang, toks) ->
      backendRunHaxl GleanBackend{..} env $ do
        r <- Search.searchEntityLocation lang toks
        (entity, err) <- case r of
          None t -> throwM (ServerException t)
          One e -> return (e, Nothing)
          Many { initial = e, message = t } ->
            return (e, Just (GlassExceptionReason_entitySearchFail t))
        let SearchEntity { entityRepo, decl = (_,file,rangespan,_) } = entity
        let scmRevs = scmRevisions dbInfo
        (, fmap logError err) <$> withRepo entityRepo (do
          location <- rangeSpanToLocationRange scmRepo file rangespan
          repo_hash <- getRepoHashForLocation location scmRevs <$> Glean.haxlRepo
          return $ SymbolLocation {
            symbolLocation_location = location,
            symbolLocation_revision = repo_hash
          })

-- | Describe characteristics of a symbol
describeSymbol
  :: Glass.Env -> SymbolId -> RequestOptions
  -> IO SymbolDescription
describeSymbol env@Glass.Env{..} symId opts =
  withSymbol "describeSymbol" env opts symId $
    \gleanDBs dbInfo (scmRepo, lang, toks) ->
      backendRunHaxl GleanBackend{..} env $ do
        r <- Search.searchEntityLocation lang toks
        (first :| rest, err) <- case r of
          None t -> throwM (ServerException t)
          One e -> return (e :| [], Nothing)
          Many { initial = e, rest = es } -> return (e :| es, Nothing) -- log?
        let scmRevs = scmRevisions dbInfo
        desc0 <- withRepo (entityRepo first) $ mkSymbolDescription symId scmRevs
                  scmRepo (toCodeEntityLoc first) Nothing
        -- now combine with up to N additional results, possibly across repos
        -- we have e.g. Optional across fbsource.arvr and fbsource.*
        -- but typical case is Hack namespaces
        descN <- forM rest $ \this -> withRepo (entityRepo this) $
            mkSymbolDescription symId scmRevs
              scmRepo (toCodeEntityLoc this) Nothing
        let !desc = foldr combineDescriptions desc0 descN
        pure (desc, err)

toCodeEntityLoc ::
  SearchEntity (ResultLocation Code.Entity) -> CodeEntityLocation
toCodeEntityLoc SearchEntity{decl = (decl, file, rangespan, name)} =
  CodeEntityLocation decl file rangespan name

-- | Given a description for a symbol, fold in extra comments, annotations
-- and locations from zero or more additional occurences of the same symbol
-- Should only be necessary for rare non-unique symbols, like Hack namespaces
--
combineDescriptions
  :: SymbolDescription -> SymbolDescription -> SymbolDescription
combineDescriptions y x =
  x { symbolDescription_annotations = symbolDescription_annotations x <>
        symbolDescription_annotations y -- <> Maybe [a]
    , symbolDescription_pretty_comments = uniq $
        symbolDescription_pretty_comments x <>
        symbolDescription_pretty_comments y
    , symbolDescription_sym_other_locations =
        filter (/= symbolDescription_sym_location x) $
          uniq ( -- collapse duplicate syms
            symbolDescription_sym_location y : -- copy the single unique value
            symbolDescription_sym_other_locations x
          )
    }

-- | Search for entities by string name with kind and language filters
searchSymbol
  :: Glass.Env
  -> SymbolSearchRequest
  -> RequestOptions
  -> IO SymbolSearchResult
searchSymbol
    env@Glass.Env{..}
    req@SymbolSearchRequest{..}
    opts@RequestOptions{..} = do
  case selectGleanDBs repoMapping scmRepo languageSet of
    Left err -> throwIO $ ServerException err
    Right rs -> case sFeelingLucky of
      Normal -> joinSearchResults mlimit terse sorted <$> Async.mapConcurrently
        (continueOnErrors. uncurry searchSymbolsIn) (Map.toList rs)
      -- lucky mode is quite different, as it has to make priority choices
      FeelingLucky -> joinLuckyResults <$> Async.mapConcurrently
        (continueOnErrors . uncurry searchLuckySymbolsIn) (Map.toList rs)
  where
    continueOnErrors = fmap fst -- TODO support strict errors
    scmRepo = symbolSearchRequest_repo_name
    languageSet = symbolSearchRequest_language
    SymbolSearchOptions{..} = symbolSearchRequest_options
    terse = not symbolSearchOptions_detailedResults
    sorted = symbolSearchOptions_sortResults
    mlimit = fromIntegral <$> requestOptions_limit
    -- inner limit can be higher if we are sampling/sorting into kind/lang sets
    mlimitInner = if sorted then fmap (*5) mlimit else mlimit

    sCase = if symbolSearchOptions_ignoreCase then Insensitive else Sensitive
    sType = if symbolSearchOptions_exactMatch then Exact else Prefix
    sScope = if symbolSearchOptions_namespaceSearch then Scope else NoScope
    sString = Text.strip symbolSearchRequest_name -- drop leading whitespace
    sKinds = map symbolKindFromSymbolKind (Set.elems symbolSearchRequest_kinds)
    sLangs = languageExpandCpp $ mapMaybe languageToCodeLang
      (Set.elems languageSet)
    sFeelingLucky =
      if symbolSearchOptions_feelingLucky then FeelingLucky else Normal
    querySpec = SearchQuery{..}
    searchQs = buildSearchQuery sFeelingLucky querySpec

    -- run the same query across more than one glean db
    searchSymbolsIn
      :: RepoName
      -> Set GleanDBName
      -> IO (RepoSearchResult, Maybe ErrorLogger)
    searchSymbolsIn repo dbs = Glass.withAllocationLimit env $
      case nonEmpty (Set.toList dbs) of
        Nothing -> pure ([], Nothing)
        Just names ->
          withGleanDBs "searchSymbol" env opts req repo names $
            \gleanDBs dbInfo -> do
              res <- backendRunHaxl GleanBackend{..} env $
                Glean.queryAllRepos $ do
                  let scmRevs = scmRevisions dbInfo
                  res <- mapM (runSearch querySpec
                                repo scmRevs mlimitInner terse sString) searchQs
                  return (nubOrd (concat res))
                  -- remove latter duplicates in n*log n
              pure (res, Nothing)

    -- In lucky mode, we avoid flattening, instead selecting from the first
    -- unique result found in priority order. We don't de-dup as we go.
    searchLuckySymbolsIn
      :: RepoName -> Set GleanDBName -> IO (FeelingLuckyResult, Maybe ErrorLogger)
    searchLuckySymbolsIn repo dbs = case nonEmpty (Set.toList dbs) of
      Nothing -> pure (FeelingLuckyResult [], Nothing)
      Just names ->
        withGleanDBs "feelingLucky" env opts req repo names $
          \gleanDBs dbInfo -> do
            res <- backendRunHaxl GleanBackend{..} env $
              Glean.queryEachRepo $ do
                -- we can conveniently limit to 2 matches per inner search
                let scmRevs = scmRevisions dbInfo
                rs <- forM searchQs $
                  runSearch querySpec repo scmRevs (Just 2) terse sString
                return (map (nubOrdOn symbolIdOrder) rs)
            pure (FeelingLuckyResult res, Nothing)

-- cheap way to fix up entities with non-unique locations: uniq on symbol id
symbolIdOrder :: (SymbolResult, a) -> SymbolId
symbolIdOrder = symbolResult_symbol . fst

-- Run a specific Query.AngleSearch query, with optional time boxing
runSearch
  :: SearchQuery
  -> RepoName
  -> ScmRevisions
  -> Maybe Int
  -> Bool
  -> Text
  -> AngleSearch
  -> RepoHaxl u w [(SymbolResult, Maybe SymbolDescription)]
runSearch querySpec repo scmRevs mlimit terse sString (Search query) = do
  (ctx,names) <- case query of
    Complete q -> (Nothing,) <$> searchWithTimeLimit mlimit queryTimeLimit q
    InheritedScope sCase term searchFn -> do
      names <- luckyParentSearch queryTimeLimit querySpec term
      case names of
        (_,[]) -> pure (Nothing,[])
        (baseEntity,parentNames) -> do
          syms <- searchWithTimeLimit mlimit queryTimeLimit
                   (searchFn sCase parentNames)
          return (baseEntity, syms)
  mapM (processSymbolResult terse sString repo scmRevs ctx) names
  where
    queryTimeLimit = 5000 -- milliseconds, make this a flag?

-- | Do a lucky search for the parent entity and return it. It will anchor
-- our later search, and we can generate a link to it later.
luckyParentSearch
  :: Int
  -> SearchQuery
  -> NonEmpty Text
  -> RepoHaxl u w (Maybe SymbolSearchData, [NonEmpty Text])
luckyParentSearch queryTimeLimit querySpec term = do
    parentSet <- mapM (runLuckyParentSearch queryTimeLimit) parentQs
    case findUnique parentSet of
      Found baseEntity -> do -- this is the anchor container, now search parents
        parents <- uniq . map Search.parentEntity <$>
          Search.searchRecursiveEntities maxInheritedDepth
            RelationDirection_Parent RelationType_Extends
              (srEntity baseEntity)
        xs <- forM parents $ fmap flattenScope . eThrow <=< toQualifiedName
        return (Just baseEntity, xs)
      _ -> pure (Nothing, []) -- not sufficiently unique. can't proceed
  where
    parentQs = buildLuckyContainerQuery querySpec term
    maxInheritedDepth = 1000

    -- this isn't the full scope (i.e. nested container parents) , but 2 levels
    flattenScope qn = case unName (qualifiedName_container qn) of
      "" -> unName (qualifiedName_localName qn) :| []
      parent -> parent :| [unName (qualifiedName_localName qn)]

runLuckyParentSearch :: Int -> AngleSearch -> RepoHaxl u w [SymbolSearchData]
runLuckyParentSearch timeLimit (Search query) = do
  result <- case query of
    Complete q -> searchWithTimeLimit (Just 2) timeLimit q
    InheritedScope{} -> return [] -- no inner recursion please
  mapM toSearchResult result

-- n.b. we need the RepoName (i.e. "fbsource" to construct symbol ids)
processSymbolResult
  :: ToSearchResult a
  => Bool
  -> Text
  -> RepoName
  -> ScmRevisions
  -> Maybe SymbolSearchData
  -> a
  -> RepoHaxl u w (SymbolResult, Maybe SymbolDescription)
processSymbolResult terse sString repo scmRevs mContext result = do
  SymbolSearchData{..} <- toSearchResult result
  let Code.Location{..} = srLocation
  symbolResult_location <- rangeSpanToLocationRange
    repo location_file location_location
  path <- GleanPath <$> Glean.keyOf location_file
  symbolResult_symbol <- toSymbolId (fromGleanPath repo path) srEntity
  symbolResult_qname <- eThrow =<< toQualifiedName srEntity
  let symbolResult_kind = symbolKindToSymbolKind <$> srKind
  symbolResult_context <- case mContext of
    Just ctx -> Just <$> processContext repo ctx
    Nothing -> case symbolResult_kind of
      Just kind
        | kind `Set.member` atomicKinds -- use parent container
        -> parentContainer repo srEntity
      _ -> pure Nothing
  let symbolResult_language = entityLanguage srEntity
      symbolResult_name = location_name
      symbolResult_score = Map.singleton
        "exactness"
        (fromIntegral (fromEnum
            (scoreResult sString location_name)))
      basics = SymbolResult{..} -- just sym id, kind, lang, location
  (basics,) <$>
    if terse then pure Nothing else
      Just <$> describeEntity scmRevs srEntity basics

-- | Leaf kinds, those that don't contain other symbols. We will try to
-- put in parent context
atomicKinds :: Set SymbolKind
atomicKinds = Set.fromList
  [ SymbolKind_Method
  , SymbolKind_Field
  , SymbolKind_Constant
  , SymbolKind_Type
  , SymbolKind_Value
  , SymbolKind_Function
  , SymbolKind_Variable
  , SymbolKind_Enumerator
  , SymbolKind_Operator
  ]

-- | A little bit of processing for the related symbol context
processContext
  :: RepoName -> SymbolSearchData -> Glean.RepoHaxl u w SymbolContext
processContext repo SymbolSearchData{..} = do
  let Code.Location{..} = srLocation
  path <- GleanPath <$> Glean.keyOf location_file
  symbolContext_symbol <- toSymbolId (fromGleanPath repo path) srEntity
  symbolContext_qname <- eThrow =<< toQualifiedName srEntity
  let symbolContext_kind = symbolKindToSymbolKind <$> srKind
  return SymbolContext{..}

-- this takes first N results. We could try other strategies (such as
-- selecting evenly by language, first n in alpha order by sym or file, ..
--
-- This is also where we sort the groups (kinds) so that the top N
-- search results appear sampled and ranked.
--
joinSearchResults
  :: Maybe Int
  -> Bool
  -> Bool
  -> [RepoSearchResult]
  -> SymbolSearchResult
joinSearchResults mlimit terse sorted xs = SymbolSearchResult syms $
    if terse then [] else catMaybes descs
  where
    uniqXs = dedupSearchResult <$> xs
    (syms,descs) = unzip $ nubOrd $ case (mlimit, sorted) of
      (Nothing, _) -> flattened
      (Just n, False) -> take n flattened
      -- codehub/aka "sorted" mode grouping, ranking and sampling
      (Just n, True) -> takeFairN n (concatMap sortResults uniqXs)

    flattened = concat uniqXs

--
-- DFS to first singleton result.
--
-- Search matrix for feeling lucky
-- - scm repo
-- - dbs within repo
-- - queries in order within db
--
-- Second result will be the anchor container if it was an inherited search
--
joinLuckyResults :: [FeelingLuckyResult] -> SymbolSearchResult
joinLuckyResults allResults = case findLucky allResults of
    Nothing -> SymbolSearchResult [] []
    Just (result, desc) -> SymbolSearchResult [result] (catMaybes [desc])
  where
    -- little state machine across logical scm repos (e.g. "fbsource")
    findLucky :: [FeelingLuckyResult] -> Maybe SingleSymbol
    findLucky [] = Nothing
    findLucky (FeelingLuckyResult scmRepo : rest) =
      case findUniqueInner scmRepo of
        Continue -> findLucky rest
        Stop -> Nothing
        Found result -> Just result

    -- across a particular scm repo's glean dbs (e.g. arvr.cxx, fbcode.cxx)
    findUniqueInner :: [[[a]]] -> MaybeResult a
    findUniqueInner [] = Continue
    findUniqueInner (db : dbs) = case findUnique db of
      Continue -> findUniqueInner dbs
      Stop -> Stop -- could try to be smart and merge duplicates
      Found x -> Found x

-- results across each underlying glean query.
-- if we see a unique result that's our winner
findUnique :: [[a]] -> MaybeResult a
findUnique [] = Continue
findUnique (q : qs) = case hasUniqueResult q of
  Continue -> findUnique qs -- continue in priority order
  Stop -> Stop -- can't proceed
  Found x -> Found x -- got it!

hasUniqueResult :: [a] -> MaybeResult a
hasUniqueResult results = case results of
  [] -> Continue
  [unique] -> Found unique
  _ -> Stop -- more than 1 result, terminate

data MaybeResult a
  = Continue -- so far, no results, keep going
  | Found a -- one perfect result
  | Stop -- yikes, too many results. abandon ship

-- | Sort the results of one query set
sortResults
  :: RepoSearchResult
  -> [[(SymbolResult, Maybe SymbolDescription)]]
sortResults xs = map (List.sortOn relevance) (groupSortOn features xs)
  where
    -- we group on the language/kind sets to produce result sets
    -- then sort those results by score and alpha (note: not groupSortOn with
    -- relevance as that would create too many classes to select from)
    features (SymbolResult{..},_desc) = Feature
      symbolResult_language (scoreKind symbolResult_kind)

  -- we could improve this slightly with a qualified vs global qname check
    relevance (SymbolResult{..},_desc) =
      ( symbolResult_score
      , scoreKind symbolResult_kind
      , scoreScope symbolResult_qname
      , symbolResult_name
      , qualifiedName_container symbolResult_qname)

-- Match feelingLuck() server-side classes
containerishKinds :: Set SymbolKind
containerishKinds = Set.fromList
  [ SymbolKind_Namespace
  , SymbolKind_Class_
  , SymbolKind_Trait
  , SymbolKind_Interface
  , SymbolKind_Module
  ]

scoreScope :: QualifiedName -> Int
scoreScope qname = case qualifiedName_container qname of
  Name "" -> 0 -- free or global things have empty container names
  _ -> 1

scoreKind :: Maybe SymbolKind -> Int
scoreKind kind = case kind of
  Just k
    | k `Set.member` containerishKinds -> 1
    | k == SymbolKind_Function || k == SymbolKind_Method -> 2
    | otherwise -> 3
  _unknown -> 4

-- | We do some light ranking of the results
scoreResult :: Text -> Text -> MatchType
scoreResult query result
  | result == query = ExactMatch
  | Text.toLower result == queryLC = ExactInsensitiveMatch
  | otherwise = Otherwise
  where
    queryLC = Text.toLower query

-- A projection from the search result to features for fair sampling groups
--
-- todo: we might want to have fewer symbolkind groups (e.g. combine class-like
-- things)
--
data Feature = Feature !Language {-# UNPACK #-}!KindSort
  deriving (Eq, Ord)

-- Containerish, function/method, other
type KindSort = Int

-- | Glass needs to mark results by how good they are
-- For scope searches we don't care so much as the scope will filter
-- results. But for bare string searches we need to do a bit of work
data MatchType
  = ExactMatch             -- "FBID"
  | ExactInsensitiveMatch  -- "fbid" ~ "FBID"
  | Otherwise
  deriving (Eq, Ord, Bounded, Enum)

-- | Symbol search for references as ranges.
fetchSymbolReferenceRanges
  :: Glean.Backend b
  => Glass.Env
  -> RepoName
  -> Language
  -> [Text]
  -> Maybe Int
  -> GleanBackend b
  -> IO ([LocationRange], Maybe ErrorLogger)
fetchSymbolReferenceRanges env scsrepo lang toks limit b =
  backendRunHaxl b env $ do
    er <- symbolToAngleEntities lang toks
    let logDBs x = logError (gleanDBs b) <> x
    case er of
      Left err -> return ([], Just $ logDBs err)
      Right (entities, searchErr) -> do
        ranges <- forM entities $ \(entityRepo, query) ->
          withRepo entityRepo $ do
            let convert (targetFile, rspan) =
                  rangeSpanToLocationRange scsrepo targetFile rspan
            uses <- searchWithLimit limit $ Query.findReferenceRangeSpan query
            mapM convert uses
        return (nubOrd $ concat ranges, fmap (logDBs . logError) searchErr)

-- | Search for a symbol and return an Angle query that identifies the entities
-- Return the angle query and the Glean.repo to which it applies.
--
-- Todo: this entity query only works on the db it came from.
-- Should be in the type
--
symbolToAngleEntities
  :: Language
  -> [Text]
  -> ReposHaxl u w
      (Either ErrorLogger
        (NonEmpty (Glean.Repo, Angle Code.Entity), Maybe GlassExceptionReason))
symbolToAngleEntities lang toks = do
  r <- Search.searchEntity lang toks -- search everywhere for something a match
  (entities, searchErr) <- case r of
    None t -> throwM (ServerException t) -- return [] ?
    One e -> return (e:|[], Nothing)
      -- n.b. picks the first entity only
    Many { initial = e, rest = es, message = t } ->
      return (e:|es, Just (GlassExceptionReason_entitySearchFail t))
  let
    eithers =
      NonEmpty.map
        (\SearchEntity{entityRepo, decl} ->(entityRepo,) <$> entityToAngle decl)
        entities

  return $ case allOrError eithers of
    Left err -> Left (logError (GlassExceptionReason_entityNotSupported err))
    Right results -> Right (results, searchErr)

newtype GetStaticAttributes =
  GetStaticAttributes (Glean.Repo, Code.Entity, Maybe Language)
  deriving (Eq, Hashable)

parentContainer
  :: RepoName -> Code.Entity -> Glean.RepoHaxl u w (Maybe SymbolContext)
parentContainer repo ent = do
  parents <- Search.searchRelatedEntities 2 Search.ShowAll NotRecursive
    RelationDirection_Parent RelationType_Contains ent repo
  let mParent = Search.parentRL <$> listToMaybe parents
  case mParent of
    Nothing -> return Nothing
    Just (p, symbolContext_symbol) -> do
      mQName <- eitherToMaybe <$> toQualifiedName (fst4 p)
      return $ do -- :: Maybe
        symbolContext_qname <- mQName
        let symbolContext_kind = Nothing
        pure SymbolContext{..}

--
-- | Relational search. Given a symbol id find symbols related to it.
-- Relationships could be parent/child or oo inheritance, for example.
--
searchRelated
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> SearchRelatedRequest
  -> IO SearchRelatedResult
searchRelated env@Glass.Env{..} sym opts@RequestOptions{..}
    SearchRelatedRequest{..} =
  withSymbol "searchRelated" env opts sym $
    \gleanDBs_ dbInfo (repo, lang, toks) ->
      let gleanDBs = filterDBs lang gleanDBs_ in
      backendRunHaxl GleanBackend{..} env $ do
        entity <- searchFirstEntity lang toks
        withRepo (entityRepo entity) $ do
          (symPairs, desc, merr) <- case searchRelatedRequest_relatedBy of
            RelationType_Calls -> do
              (entityPairs, merr) <-
                searchRelatedCalls
                  repo searchRelatedRequest_relation entity lang
              desc <- descriptions repo entityPairs dbInfo
              let symPairs = symbolIdPairs entityPairs
              return (symPairs, desc, merr)
            RelationType_Generates -> do
              (symPairs, merr) <-
                searchRelatedGenerate
                  repo searchRelatedRequest_relation entity lang
              return (symPairs, Map.empty, merr)
            _ -> do
              (entityPairs, merr) <- do
                relatedLocatedEntities <-
                        Search.searchRelatedEntities limit
                          Search.ShowAll
                          searchRecursively
                          searchRelatedRequest_relation
                          searchRelatedRequest_relatedBy
                          (fst4 (decl entity))
                          repo
                return ((,Nothing) <$> relatedLocatedEntities, Nothing)
              desc <- descriptions repo entityPairs dbInfo
              let symPairs = symbolIdPairs entityPairs
              return (symPairs, desc, merr)
          let result = SearchRelatedResult
                { searchRelatedResult_edges = symPairs
                , searchRelatedResult_symbolDetails = desc
                }
          pure (result, merr)

  where
    symbolIdPairs
      :: [(RelatedLocatedEntities, Maybe [LocationRange])] -> [RelatedSymbols]
    symbolIdPairs entityPairs =
      map (\(Search.RelatedLocatedEntities{..}, ranges) ->
        RelatedSymbols (snd parentRL) (snd childRL) ranges
      ) entityPairs

    descriptions
      :: RepoName
      -> [(RelatedLocatedEntities, Maybe [LocationRange])]
      -> GleanDBInfo
      -> RepoHaxl u w (Map.Map Text SymbolDescription)
    descriptions repo entityPairs dbInfo =
     if searchRelatedRequest_detailedResults
        then do
          let uniqSymIds = uniqBy (comparing snd) $ concat
                [ [ e1, e2 ]
                | (Search.RelatedLocatedEntities e1 e2, _) <- entityPairs ]
          let scmRevs = scmRevisions dbInfo
          descs <- mapM (mkDescribe repo scmRevs) uniqSymIds
            -- carefully in parallel!
          pure $ Map.fromAscList descs
        else pure mempty

    -- building map of sym id -> descriptions, by first occurence
    mkDescribe repo scmRevs e@(_,SymbolId rawSymId) = (rawSymId,) <$>
      describe repo scmRevs e

    describe repo scmRevs ((entity, entityFile, entityRange, entityName), symId)
      = mkSymbolDescription symId scmRevs repo CodeEntityLocation{..} Nothing

    searchRecursively
      | searchRelatedRequest_recursive = Search.Recursive
      | otherwise = Search.NotRecursive

    limit = fromIntegral $ case requestOptions_limit of
      Just x | x < rELATED_SYMBOLS_MAX_LIMIT -> x
      _ -> rELATED_SYMBOLS_MAX_LIMIT

    -- Caller hieararchy for C++ requires cxx1.DeclarationSources,
    -- which may not be supported by all DBs.
    filterDBs :: Language -> NonEmpty (GleanDBName,a) -> NonEmpty (GleanDBName,a)
    filterDBs lang
      | RelationType_Calls <- searchRelatedRequest_relatedBy
      , RelationDirection_Parent <- searchRelatedRequest_relation
      , Language_Cpp <- lang
      = NonEmpty.fromList .
        NonEmpty.filter (supportsCxxDeclarationSources . fst)
      | otherwise
      = id

    searchRelatedGenerate
      :: RepoName
      -> RelationDirection
      -> SearchEntity (ResultLocation Code.Entity)
      -> Language
      -> RepoHaxl u w
          ([RelatedSymbols], Maybe ErrorLogger)
    searchRelatedGenerate repoName RelationDirection_Parent child _ = do
      let SearchEntity{decl = (decl, _file, _rangespan, _name)} = child
      case entityToAngle decl of
        Left err -> do
          repo <- Glean.haxlRepo
          return
            ([], Just (logError (GlassExceptionReason_entityNotSupported err)
                    <> logError repo)
            )
        Right query -> do
          results <- searchWithLimit Nothing $
            Query.generatedEntityToIdlEntity query
          idlDef <- forM results $ \(entity, file) -> do
            gleanPath <- GleanPath <$> Glean.keyOf file
            parentSymbol <- toSymbolId (fromGleanPath repoName gleanPath) entity
            return  $ RelatedSymbols parentSymbol sym Nothing
          return (idlDef, Nothing)

    -- TODO implement RelationDirection_Child
    searchRelatedGenerate _ _ _ _ = return ([], Nothing)

    -- Implements Call hierarchy (check LSP spec)
    searchRelatedCalls
      :: RepoName
      -> RelationDirection
      -> SearchEntity (ResultLocation Code.Entity)
      -> Language
      -> RepoHaxl u w
          ([(RelatedLocatedEntities, Maybe [LocationRange])], Maybe ErrorLogger)
    searchRelatedCalls _ RelationDirection__UNKNOWN{} _ _ = error "unreachable"
    -- Incoming calls, mapping a symbol to all its callers symbols.
    searchRelatedCalls repoName RelationDirection_Parent child Language_Cpp = do
      -- For C++ we use a more efficient predicate, albeit without call sites
      let SearchEntity{decl = (decl, file, rangespan, name)} = child
      case entityToAngle decl of
        Left err -> do
          repo <- Glean.haxlRepo
          return ([], Just (logError (GlassExceptionReason_entityNotSupported err)
                    <> logError repo)
            )
        Right query -> do
          let childRL = ((decl, file, rangespan, name), sym)
              limit = fromIntegral <$> requestOptions_limit
          results <- searchWithLimit limit $
            Query.findReferenceEntitiesFast query
          callers <- forM results $ \(entity, loc) -> do
            let path = Code.location_file loc
            gleanPath <- GleanPath <$> Glean.keyOf path
            symbol <- toSymbolId (fromGleanPath repoName gleanPath) entity
            let name = Code.location_name loc
                range = Code.location_location loc
                parentRL = ((entity, path, range, name), symbol)
            return Search.RelatedLocatedEntities{..}
          return ((,Nothing) <$> callers, Nothing)

    searchRelatedCalls repoName RelationDirection_Parent child _ = do
        let SearchEntity{decl = (decl, file, rangespan, name)} = child
        case entityToAngle decl of
          Left err -> do
            repo <- Glean.haxlRepo
            return
              ([], Just (logError (GlassExceptionReason_entityNotSupported err)
                      <> logError repo)
              )
          Right query -> do
            let childRL = ((decl, file, rangespan, name), sym)
                -- limit until we optimize the query to use DeclarationSource
                limit = maybe 200 fromIntegral requestOptions_limit
                timeLimit = 2000 -- ms
            results <- searchWithTimeLimit (Just limit) timeLimit $
              Query.findReferenceEntities query
            callers <- forM results $ \(path, entity, loc, callSite) -> do
              gleanPath <- GleanPath <$> Glean.keyOf path
              symbol <- toSymbolId (fromGleanPath repoName gleanPath) entity
              let name = Code.location_name loc
                  range = Code.location_location loc
                  parentRL = ((entity, path, range, name), symbol)
              location <- rangeSpanToLocationRange repoName path callSite
              return (Search.RelatedLocatedEntities{..}, location)
            let groups = groupLocatedEntitiesOn parentRL callers
            return (second Just <$> groups, Nothing)

    -- | Outgoing calls, maps a symbol to all the symbols it references.
    searchRelatedCalls repoName RelationDirection_Child parent lang = do
      let SearchEntity{decl = (decl, file, rangespan, name)} = parent
          parentRL = ((decl, file, rangespan, name), sym)
      parentRange <-
        locationRange_range <$> rangeSpanToLocationRange repoName file rangespan
      (xrefs_, _, _) <- Utils.documentSymbolsForLanguage Nothing (Just lang)
          (Utils.ExtraSymbolOpts True False Nothing)
          (Glean.getId file)
      -- only consider non-xlang xrefs
      let xrefs = [x | PlainXRef x <- xrefs_]
      xrefsRanges <- forM xrefs $ \(Code.XRefLocation{..}, entity) -> do
        gleanPath <- GleanPath <$>
          Glean.keyOf (Code.location_file xRefLocation_target)
        symbol <- toSymbolId (fromGleanPath repoName gleanPath) entity
        sourceRange <- rangeSpanToLocationRange repoName file xRefLocation_source
        kind <- findSymbolKind entity
        return (sourceRange, symbol, kind)
      let callees =
            [ (Search.RelatedLocatedEntities{..}, callRange)
            | ((Code.XRefLocation{..}, entity), (callRange, symbol, Right kind))
                <- zip xrefs xrefsRanges
            , kind `elem`
                [ SymbolKind_Constructor
                , SymbolKind_Function
                , SymbolKind_Method
                , SymbolKind_Macro
                ]
            , rangeContains parentRange (locationRange_range callRange)
            , let childRL =
                    (( entity
                    , Code.location_file xRefLocation_target
                    , Code.location_location xRefLocation_target
                    , Code.location_name xRefLocation_target
                    ), symbol)
            ]
      let groups = groupLocatedEntitiesOn childRL callees
      return (second Just <$> groups, Nothing)

    groupLocatedEntitiesOn f locatedEntities =
      [ (locatedEntity, locs)
      | group@((locatedEntity, _) : _) <-
          groupOn (snd . f . fst) locatedEntities
      , let locs = map snd group
      ]
--
-- Extract the "API" neighborhood of a symbol
--
searchRelatedNeighborhood
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> RelatedNeighborhoodRequest
  -> IO RelatedNeighborhoodResult
searchRelatedNeighborhood env@Glass.Env{..} sym opts@RequestOptions{..} req =
  withSymbol "searchRelatedNeighborhood" env opts sym $
    \gleanDBs dbInfo (repo, lang, toks) ->
      backendRunHaxl GleanBackend{..} env $ do
        baseEntity <- searchFirstEntity lang toks
        let lang = entityLanguage (fst4 (decl baseEntity))
        (,Nothing) <$> searchNeighborhood limit req sym repo
          (scmRevisions dbInfo) lang baseEntity
  where
    limit = fromIntegral $ case requestOptions_limit of
      Just x | x < rELATED_SYMBOLS_MAX_LIMIT -> x
      _ -> rELATED_SYMBOLS_MAX_LIMIT

searchFirstEntity
  :: Language
  -> [Text]
  -> Glean.ReposHaxl u w (SearchEntity (ResultLocation Code.Entity))
searchFirstEntity lang toks = do
  r <- Search.searchEntityLocation lang toks
  case r of
    None t -> throwM (ServerException t)
    One e -> return e
    Many { initial = e } -> return e
