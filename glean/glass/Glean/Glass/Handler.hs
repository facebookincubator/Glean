{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Handler
  (
  -- * listing symbols by file
    documentSymbolListX
  , documentSymbolIndex

  -- ** find references
  , findReferences
  , findReferenceRanges

  -- * working with symbol ids
  , resolveSymbolRange
  , describeSymbol

  -- * searching by string
  , searchSymbol

  -- ** by relationship
  , searchRelated
  , searchRelatedNeighborhood

  -- * C++ specific methods
  , fileIncludeLocations
  , clangUSRToDefinition

  ) where

import Control.Monad
import Control.Exception ( throwIO, SomeException )
import Control.Monad.Catch ( throwM, try )
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
import Data.Tuple.Extra ( fst3 )
import qualified Data.Set as Set
import Data.Set ( Set )
import qualified Control.Concurrent.Async as Async
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Haxl.Core as Haxl
import Glean.Util.ToAngle
import Util.Text ( textShow )
import Util.List ( uniq, uniqBy )

import Thrift.Protocol ( fromThriftEnum )

import Glean.Angle as Angle ( Angle )
import qualified Glean
import Glean.Haxl as Glean
import Glean.Haxl.Repos as Glean
import Glean.Util.Some as Glean
import qualified Glean.Util.Range as Range

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src

import qualified Glean.Glass.Attributes as Attributes
import Glean.Glass.Base
import Glean.Glass.Describe ( mkSymbolDescription, describeEntity )
import Glean.Glass.Digest
import Glean.Glass.Handler.Utils
import Glean.Glass.Logging
import Glean.Glass.Repos
import Glean.Glass.Path ( toGleanPath, fromGleanPath )
import Glean.Glass.Range
import Glean.Glass.SymbolId
import Glean.Glass.SymbolSig ( toSymbolSignatureText )
import Glean.Glass.Pretty.Cxx as Cxx (Qualified(..))
import Glean.Glass.Types
import Glean.Glass.RepoMapping ( supportsCxxDeclarationSources )
import Glean.Glass.Search.Class ( ResultLocation )

import qualified Glean.Glass.Env as Glass

import qualified Glean.Glass.Query as Query
import Glean.Glass.Query (
    SearchCase(..), SearchType(..),
    SearchScope(..), SearchMode(..),
    SingleSymbol, FeelingLuckyResult(..),
    QueryExpr(..)
  )
import qualified Glean.Glass.Query.Cxx as Cxx
import Glean.Glass.XRefs
  ( GenXRef(..), XRef, resolveEntitiesRange, splitXRefs )

import Glean.Glass.SymbolMap ( toSymbolIndex )
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

import Glean.Glass.SnapshotBackend
  ( SnapshotBackend(getSnapshot),
    SnapshotStatus() )
import qualified Glean.Glass.SnapshotBackend as Snapshot
import Glean.Glass.SymbolKind (findSymbolKind)
import Glean.Glass.Env (Env' (tracer, sourceControl, useSnapshotsForSymbolsList))
import Glean.Glass.SourceControl
import Glean.Glass.Tracing (traceSpan)

-- | Runner for methods that are keyed by a file path
runRepoFile
  :: (LogResult t)
  => Text
  -> (GleanDBInfo
    -> DocumentSymbolsRequest
    -> RequestOptions
    -> GleanBackend (Glean.Some Glean.Backend)
    -> Glean.Some SnapshotBackend
    -> Maybe Language
    -> IO (t, Maybe ErrorLogger))
  -> Glass.Env
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> IO t
runRepoFile sym fn env@Glass.Env{..} req opts =
  withRepoFile sym env opts req repo file $ \gleanDBs dbInfo mlang ->
      fn dbInfo req opts
         GleanBackend{..}
         snapshotBackend
         mlang
  where
    repo = documentSymbolsRequest_repository req
    file = documentSymbolsRequest_filepath req

-- | Discover navigable symbols in this file, resolving all bytespans and
-- adding any attributes
documentSymbolListX
  :: Glass.Env
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> IO DocumentSymbolListXResult
documentSymbolListX env r opts = do
  useSnapshots <- useSnapshotsForSymbolsList env
  fst3 <$>
    runRepoFile
      "documentSymbolListX"
      (if useSnapshots
        then fetchSymbolsAndAttributes env
        else (\dbInfo req opts be _ mlang -> do
                ((res, log), err) <-
                  fetchSymbolsAndAttributesGlean env dbInfo req opts be mlang
                return ((res, Snapshot.Unrequested, log), err)
              )
      )
      env r opts

-- | Same as documentSymbolList() but construct a line-indexed map for easy
-- cursor/position lookup, and add extra metadata
documentSymbolIndex
  :: Glass.Env
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> IO DocumentSymbolIndex
documentSymbolIndex env r opts =
  fst3 <$>
    runRepoFile
      "documentSymbolIndex"
      (fetchDocumentSymbolIndex env)
      env r opts

-- | Symbol-based find-refernces.
findReferences
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO [Location]
findReferences env@Glass.Env{..} sym opts@RequestOptions{..} =
  withSymbol "findReferences" env opts sym $
    \gleanDBs _dbInfo (repo, lang, toks) ->
      fetchSymbolReferences env repo lang toks limit
        GleanBackend{..}
  where
    limit = fmap fromIntegral requestOptions_limit

-- | Symbol-based find-refernces.
findReferenceRanges
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO [LocationRange]
findReferenceRanges env@Glass.Env{..} sym opts@RequestOptions{..} =
  withSymbol "findReferenceRanges" env opts sym
    $ \gleanDBs _dbInfo (repo, lang, toks) ->
      fetchSymbolReferenceRanges env repo lang toks limit
        GleanBackend{..}
  where
    limit = fmap fromIntegral requestOptions_limit


-- | Resolve a symbol identifier to its range-based location in the latest db
resolveSymbolRange
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO LocationRange
resolveSymbolRange env@Glass.Env{..} sym opts = do
  withSymbol "resolveSymbolRange" env opts sym
    $ \gleanDBs _dbInfo (repo, lang, toks) ->
      findSymbolLocationRange env GleanBackend{..} repo lang toks

-- | Describe characteristics of a symbol
describeSymbol
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
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
    , symbolDescription_comments = uniq $
        symbolDescription_comments x <>
        symbolDescription_comments y
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

fileIncludeLocations
  :: Glass.Env
  -> FileIncludeLocationRequest
  -> RequestOptions
  -> IO FileIncludeLocationResults
fileIncludeLocations env@Glass.Env{..} req opts = do
  fmap fst $ do
  withRepoFile "fileIncludeLocations" env opts
    req repo rootfile $ \gleanDBs _ _ ->
      backendRunHaxl GleanBackend{..} env $ do
        result <- firstOrErrors $ do
          rev <- getRepoHash <$> Glean.haxlRepo
          efile <- getFile (toGleanPath (SymbolRepoPath repo rootfile))
          case efile of
            Left err -> return (Left err)
            Right file -> do
              includes <- Cxx.fileIncludeLocationsForCxx depth mlimit file
              Right <$> processFileIncludes repo rev includes
        case result of
          Left err -> throwM $ ServerException $ errorsText err
          Right (efile, gleanDataLog) ->
            return ((efile, gleanDataLog), Nothing)
  where
    repo = fileIncludeLocationRequest_repository req
    rootfile = fileIncludeLocationRequest_filepath req
    depth_ = fileIncludeLocationRequest_depth req
    depth | depth_ <= 0 = 1
          | otherwise = fromIntegral depth_
    mlimit = fromIntegral <$> requestOptions_limit opts

-- | Lookup the USR in Glean to yield and entity,
-- compute entity declToDef, return the pairs and other info.
clangUSRToDefinition
  :: Glass.Env
  -> USR
  -> RequestOptions
  -> IO (USRSymbolDefinition, QueryEachRepoLog)
clangUSRToDefinition env@Glass.Env{..} usr@(USR hash) opts = do
  withRepoLanguage "clangUSRToDefinition" env usr repo mlang opts $
    \gleanDBs _ _ -> do
      backendRunHaxl GleanBackend{..} env $ do
        result <- firstOrErrors $ do
          rev <- getRepoHash <$> Glean.haxlRepo
          mdefn <- Cxx.usrHashToDeclaration hash
          case mdefn of
            Nothing -> -- either hash is unknown or decl is already defn
              pure (Left (GlassExceptionReason_entitySearchFail
                "No definition result for hash"))
            Just (Code.Location{..}, entity) -> do
              range <- case location_destination of
                Just Src.FileLocation{..} -> do
                  let rangeSpan = Code.RangeSpan_span fileLocation_span
                  rangeSpanToLocationRange repo fileLocation_file rangeSpan
                _ -> rangeSpanToLocationRange repo location_file
                  location_location
              path <- GleanPath <$> Glean.keyOf location_file
              sym <- toSymbolId (fromGleanPath repo path) entity
              pure (Right (USRSymbolDefinition {
                uSRSymbolDefinition_location = range,
                uSRSymbolDefinition_revision = rev,
                uSRSymbolDefinition_sym = sym
              }))
        case result of
          Left err -> throwM $ ServerException $ errorsText err
          Right defn -> return (defn, Nothing)
  where
    repo = RepoName "fbsource"
    mlang = Just Language_Cpp


-- | Scrub all glean types for export to the client
-- And flatten to lists for GraphQL.
processFileIncludes
  :: RepoName
  -> Revision
  -> Map.Map Src.File [(Src.File , Src.Range)]
  -> Glean.RepoHaxl u w FileIncludeLocationResults
processFileIncludes repo rev xmap = do
  forExport <- forM (Map.toList xmap) $ \(file, xrefs) -> do
    key <- GleanPath <$> Glean.keyOf file
    refs <- forM xrefs $ \(targetFile, srcRange) -> do
      targetPath <- GleanPath <$> Glean.keyOf targetFile
      let range = inclusiveRangeToExclusiveRange srcRange
      pure (FileXRefTarget (symbolPath $ fromGleanPath repo targetPath) range)
    pure FileIncludeXRef {
      fileIncludeXRef_source = symbolPath (fromGleanPath repo key),
      fileIncludeXRef_includes = refs
    }
  pure FileIncludeLocationResults {
    fileIncludeLocationResults_references = XRefFileList forExport,
    fileIncludeLocationResults_revision = rev
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
    querySpec = Query.SearchQuery{..}
    searchQs = Query.buildSearchQuery sFeelingLucky querySpec

    -- run the same query across more than one glean db
    searchSymbolsIn
      :: RepoName
      -> Set GleanDBName
      -> IO (Query.RepoSearchResult, Maybe ErrorLogger)
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
  :: Query.SearchQuery
  -> RepoName
  -> ScmRevisions
  -> Maybe Int
  -> Bool
  -> Text
  -> Query.AngleSearch
  -> RepoHaxl u w [(SymbolResult, Maybe SymbolDescription)]
runSearch querySpec repo scmRevs mlimit terse sString (Query.Search query) = do
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
  -> Query.SearchQuery
  -> NonEmpty Text
  -> RepoHaxl u w (Maybe Query.SymbolSearchData, [NonEmpty Text])
luckyParentSearch queryTimeLimit querySpec term = do
    parentSet <- mapM (runLuckyParentSearch queryTimeLimit) parentQs
    case findUnique parentSet of
      Found baseEntity -> do -- this is the anchor container, now search parents
        parents <- uniq . map Search.parentEntity <$>
          Search.searchRecursiveEntities maxInheritedDepth
            RelationDirection_Parent RelationType_Extends
              (Query.srEntity baseEntity)
        xs <- forM parents $ fmap flattenScope . eThrow <=< toQualifiedName
        return (Just baseEntity, xs)
      _ -> pure (Nothing, []) -- not sufficiently unique. can't proceed
  where
    parentQs = Query.buildLuckyContainerQuery querySpec term
    maxInheritedDepth = 1000

    -- this isn't the full scope (i.e. nested container parents) , but 2 levels
    flattenScope qn = case unName (qualifiedName_container qn) of
      "" -> unName (qualifiedName_localName qn) :| []
      parent -> parent :| [unName (qualifiedName_localName qn)]

runLuckyParentSearch
  :: Int -> Query.AngleSearch -> RepoHaxl u w [Query.SymbolSearchData]
runLuckyParentSearch timeLimit (Query.Search query) = do
  result <- case query of
    Complete q -> searchWithTimeLimit (Just 2) timeLimit q
    InheritedScope{} -> return [] -- no inner recursion please
  mapM Query.toSearchResult result

-- n.b. we need the RepoName (i.e. "fbsource" to construct symbol ids)
processSymbolResult
  :: Query.ToSearchResult a
  => Bool
  -> Text
  -> RepoName
  -> ScmRevisions
  -> Maybe Query.SymbolSearchData
  -> a
  -> RepoHaxl u w (SymbolResult, Maybe SymbolDescription)
processSymbolResult terse sString repo scmRevs mContext result = do
  Query.SymbolSearchData{..} <- Query.toSearchResult result
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
  :: RepoName -> Query.SymbolSearchData -> Glean.RepoHaxl u w SymbolContext
processContext repo Query.SymbolSearchData{..} = do
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
  -> [Query.RepoSearchResult]
  -> SymbolSearchResult
joinSearchResults mlimit terse sorted xs = SymbolSearchResult syms $
    if terse then [] else catMaybes descs
  where
    (syms,descs) = unzip $ nubOrd $ case (mlimit, sorted) of
      (Nothing, _) -> flattened
      (Just n, False) -> take n flattened
      -- codehub/aka "sorted" mode grouping, ranking and sampling
      (Just n, True) -> takeFairN n (concatMap sortResults xs)

    flattened = concat xs

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
  :: Query.RepoSearchResult
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

-- | Normalized (to Glean) paths
data FileReference =
  FileReference {
    _repoName :: !RepoName,
    _theGleanPath :: !GleanPath
  }

toFileReference :: RepoName -> Path -> FileReference
toFileReference repo path =
  FileReference repo (toGleanPath $ SymbolRepoPath repo path)

-- | Symbol search: try to resolve the line/col range of an entity
findSymbolLocationRange
  :: Glean.Backend b
  => Glass.Env
  -> GleanBackend b
  -> RepoName
  -> Language
  -> [Text]
  -> IO (LocationRange, Maybe ErrorLogger)
findSymbolLocationRange env b repo lang toks = backendRunHaxl b env $
  withEntity rangeSpanToLocationRange repo lang toks

-- | Symbol search for references
fetchSymbolReferences
  :: Glean.Backend b
  => Glass.Env
  -> RepoName
  -> Language
  -> [Text]
  -> Maybe Int
  -> GleanBackend b
  -> IO ([Location], Maybe ErrorLogger)
fetchSymbolReferences env scsrepo lang toks limit b = backendRunHaxl b env $ do
  er <- symbolToAngleEntities lang toks
  case er of
    Left err -> return ([], Just (err <> logError (gleanDBs b)))
    Right (entities, searchErr) -> do
      locs <- forM entities $ \(entityRepo, query) ->
        withRepo entityRepo $ do
          let convert (targetFile, rspan) =
                rangeSpanToLocation scsrepo targetFile rspan
          uses <- searchWithLimit limit $ Query.findReferenceRangeSpan query
          mapM convert uses
      return (nubOrd $ concat locs, fmap logError searchErr)

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


-- | Run an action on the result of looking up a symbol id
withEntity
  :: (RepoName -> Src.File -> Code.RangeSpan -> RepoHaxl u w a)
  -> RepoName
  -> Language
  -> [Text]
  -> Glean.ReposHaxl u w (a, Maybe ErrorLogger)
withEntity f scsrepo lang toks = do
  r <- Search.searchEntityLocation lang toks
  (SearchEntity{entityRepo, decl=(_,file,rangespan,_)}, err) <- case r of
    None t -> throwM (ServerException t)
    One e -> return (e, Nothing)
    Many { initial = e, message = t } ->
      return (e, Just (GlassExceptionReason_entitySearchFail t))
  (, fmap logError err) <$> withRepo entityRepo (f scsrepo file rangespan)

fetchSymbolsAndAttributesGlean
  :: Glean.Backend b
  => Glass.Env
  -> GleanDBInfo
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> GleanBackend b
  -> Maybe Language
  -> IO (
       (DocumentSymbolListXResult, QueryEachRepoLog),
       Maybe ErrorLogger
     )
fetchSymbolsAndAttributesGlean env@Glass.Env{..} dbInfo req opts be mlang = do
  (res1, gLogs, elogs) <- traceSpan tracer "fetchDocumentSymbols" $
    fetchDocumentSymbols env file path mlimit
      (requestOptions_revision opts)
      (requestOptions_exact_revision opts)
      includeRefs includeXlangRefs
      (shouldFetchContentHash opts) be mlang dbInfo
  let res2 = toDocumentSymbolResult res1
  return ((res2, gLogs), elogs)
  where
    repo = documentSymbolsRequest_repository req
    path = documentSymbolsRequest_filepath req
    file = toFileReference repo path
    includeRefs = documentSymbolsRequest_include_refs req
    includeXlangRefs = case opts of
      RequestOptions { requestOptions_feature_flags = Just
        FeatureFlags { featureFlags_include_xlang_refs = Just True } } -> True
      _ -> False

    mlimit = Just (fromIntegral (fromMaybe mAXIMUM_SYMBOLS_QUERY_LIMIT
      (requestOptions_limit opts)))

shouldFetchContentHash :: RequestOptions -> Bool
shouldFetchContentHash opts =
  not (requestOptions_exact_revision opts) &&
  (requestOptions_content_check opts ||
    requestOptions_matching_revision opts)

type FetchDocumentSymbols =
  ((DocumentSymbolListXResult, SnapshotStatus, QueryEachRepoLog),
    Maybe ErrorLogger)

-- | When an explicit revision is requested, we attempt to fetch both
-- Glean results and a snapshot. This function chooses which result to
-- use based on the requested revision, the revision of the result and
-- RequestOptions.
chooseGleanOrSnapshot
  :: RequestOptions
  -> Revision
  -> (
       (DocumentSymbolListXResult, QueryEachRepoLog),
       Maybe ErrorLogger
     )
     -- ^ Glean result
  -> Either
       SnapshotStatus
       (Revision, DocumentSymbolListXResult, Maybe Bool)
     -- ^ Snapshot result (with deferred fetch)
  -> IO FetchDocumentSymbols
chooseGleanOrSnapshot RequestOptions{..} revision glean esnapshot
  | isRevisionHit revision glean =
    returnGlean
  | Right (rev, res, _) <- esnapshot, rev == revision =
    return $ returnSnapshot res Snapshot.ExactMatch
  | requestOptions_matching_revision && not requestOptions_exact_revision =
    doMatching
  | otherwise =
    returnGlean
  where
    returnGlean = return $
      addStatus (either id (const Snapshot.Ignored) esnapshot) glean

    doMatching
      | Just True <- resultContentMatch glean = returnGlean
      | Right (_, res, match) <- esnapshot =
          return $ if match == Just True
            then returnSnapshot res Snapshot.CompatibleMatch
            else empty Snapshot.Ignored
      | Left s <- esnapshot = return $ empty s
      where
      empty status =
        ((toDocumentSymbolResult(emptyDocumentSymbols revision)
          , status
          , FoundNone)
        , Just $ logError $
            GlassExceptionReason_matchingRevisionNotAvailable $
              unRevision revision
        )

    addStatus st ((res, gleanLog), mlogger) =
      ((res, st, gleanLog), mlogger)

    getResultRevision ((x, _), _) = documentSymbolListXResult_revision x
    isRevisionHit rev = (== rev) . getResultRevision

    resultContentMatch ((r, _), _) =
      documentSymbolListXResult_content_match r

returnSnapshot
  :: DocumentSymbolListXResult
  -> SnapshotStatus
  -> FetchDocumentSymbols
returnSnapshot queryResult match =
  ((setContentMatch queryResult, match, QueryEachRepoUnrequested), Nothing)
  where
    -- set the content_match field appropriately if we used a snapshot
    setContentMatch res = case match of
      Snapshot.ExactMatch -> matchYes
      Snapshot.CompatibleMatch -> matchYes
      _ -> res
      where
      matchYes = res { documentSymbolListXResult_content_match = Just True }

-- | Fall back to the best snapshot available for new files (not yet in repo)
fallbackForNewFiles
  :: SnapshotBackend snapshotBackend
  => Glass.Env
  -> RequestOptions
  -> snapshotBackend
  -> RepoName
  -> Path
  -> FetchDocumentSymbols
  -> IO FetchDocumentSymbols
fallbackForNewFiles Glass.Env{..} RequestOptions{..} snapshotbe repo file res
  | ((_,_,_), Just ErrorLogger {errorTy}) <- res, all isNoSrcFileFact errorTy,
    -- assume it's a new file if no src.File fact
    Just revision <- requestOptions_revision,
    not requestOptions_exact_revision = do
      gen <- getGeneration sourceControl repo revision
      bestSnapshot <- getSnapshot tracer snapshotbe repo file Nothing gen
      case bestSnapshot of
        Right (_, fetch) -> do
          snap <- fetch
          return $ maybe res (`returnSnapshot` Snapshot.Latest) snap
        Left _ ->
          return res
  | otherwise =
    return res
  where
    isNoSrcFileFact GlassExceptionReason_noSrcFileFact{} = True
    isNoSrcFileFact _ = False

-- Find all symbols and refs in file and add all attributes
fetchSymbolsAndAttributes
  :: (Glean.Backend b, SnapshotBackend snapshotBackend)
  => Glass.Env
  -> GleanDBInfo
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> GleanBackend b
  -> snapshotBackend
  -> Maybe Language
  -> IO ((DocumentSymbolListXResult, SnapshotStatus, QueryEachRepoLog)
        , Maybe ErrorLogger)
fetchSymbolsAndAttributes env@Glass.Env{..} dbInfo req
  opts@RequestOptions{..} be snapshotbe mlang = do
  res <- case requestOptions_revision of
    Nothing ->
      addStatus Snapshot.Unrequested <$> getFromGlean
    Just revision -> do
      (esnapshot, glean) <- Async.concurrently
        (traceSpan tracer "getSnapshot" $ getFromSnapshot revision)
        (traceSpan tracer "getFromGlean" getFromGlean)
      chooseGleanOrSnapshot opts revision glean esnapshot

  fallbackForNewFiles env opts snapshotbe repo file res
  where
    file = documentSymbolsRequest_filepath req
    repo = documentSymbolsRequest_repository req

    addStatus st ((res, gleanLog), mlogger) = ((res, st, gleanLog), mlogger)

    getFromSnapshot revision = do
      let matching = requestOptions_matching_revision &&
            not requestOptions_exact_revision
      gen <-
        if matching
          then getGeneration sourceControl repo revision
          else return Nothing
      res <- getSnapshot tracer snapshotbe repo file (Just revision) gen
      case res of
        Left s -> return (Left s)
        Right (snapshotRev, fetch) -> do
          let result Nothing _ = Left Snapshot.InternalError
              result (Just res) match = Right (snapshotRev, res, match)
          if matching
            then do
              (content, match) <- Async.concurrently
                fetch
                (contentMatch snapshotRev revision)
              return $ result content match
            else do
              content <- fetch
              return $ result content Nothing

    contentMatch myrev wantedrev
      | myrev == wantedrev = return (Just True)
      | otherwise =
        backendRunHaxl be env $
          withRepo (snd (NonEmpty.head (gleanDBs be))) $ do
            wanted <- getFileContentHash sourceControl repo file wantedrev
            mine <- getFileContentHash sourceControl repo file myrev
            return (Just (isJust wanted && isJust mine && wanted == mine))

    getFromGlean =
      Glass.withAllocationLimit env $
        fetchSymbolsAndAttributesGlean env dbInfo req opts be mlang

-- Find all references and definitions in a file that might be in a set of repos
fetchDocumentSymbols
  :: Glean.Backend b
  => Glass.Env
  -> FileReference
  -> Path  -- ^ original path in the repo
  -> Maybe Int
  -> Maybe Revision
  -> Bool  -- ^ exact revision only?
  -> Bool  -- ^ include references?
  -> Bool  -- ^ include xlang references?
  -> Bool  -- ^ fetch file content hash?
  -> GleanBackend b
  -> Maybe Language
  -> GleanDBInfo
  -> IO (DocumentSymbols, QueryEachRepoLog, Maybe ErrorLogger)
fetchDocumentSymbols env@Glass.Env{..} (FileReference scsrepo path)
    repoPath mlimit wantedRevision
    exactRevision includeRefs includeXlangRefs fetchContentHash
    b mlang dbInfo =
  backendRunHaxl b env $ do
  --
  -- we pick the first db in the list that has the full FileInfo{..}
  -- and in exact_revision mode the rev also has to match precisely
  --
  efile <- firstOrErrors $ do
    repo <- Glean.haxlRepo
    if not (revisionAcceptable repo)
      then return $ Left $ GlassExceptionReason_exactRevisionNotAvailable $
        revisionSpecifierError wantedRevision
      else do
        res <- getFileInfo repo path
        return $ case res of
          Left _ -> res
          Right fi@FileInfo{..}
            | not isIndexed -> Left $ GlassExceptionReason_notIndexedFile $
              case indexFailure of
                Nothing -> "Not indexed: " <> gleanPath path
                Just Src.IndexFailure_key{..} -> Text.pack $
                    show indexFailure_key_reason
                    <> ": " <>
                    show indexFailure_key_details
            | otherwise -> Right fi

  case efile of
    Left err -> do
      let logs = logError err <> logError (gleanDBs b)
      return (emptyDocumentSymbols (revision b), FoundNone, Just logs)
      where
        -- Use first db's revision
        revision GleanBackend {gleanDBs = ((_, repo) :| _)} =
          getDBRevision (scmRevisions dbInfo) repo scsrepo

    Right (FileInfo{..}, gleanDataLog) -> do

      -- from Glean, fetch xrefs and defs in two batches
      (xrefs, defns, truncated) <- withRepo fileRepo $
        documentSymbolsForLanguage mlimit mlang includeRefs includeXlangRefs
          fileId
      (kindMap, merr) <- withRepo fileRepo $
        documentSymbolKinds mlimit mlang fileId

      let revision = getDBRevision (scmRevisions dbInfo) fileRepo scsrepo
      contentMatch <- case wantedRevision of
        Nothing -> return Nothing
        Just wanted
          | wanted == revision -> return (Just True)
          | fetchContentHash -> withRepo fileRepo $ do
            let getHash = getFileContentHash sourceControl scsrepo repoPath
            contentHash <- getHash revision
            wantedHash <- getHash wanted
            let match = isJust contentHash && isJust wantedHash &&
                  contentHash == wantedHash
            return (Just match)
          | otherwise ->
            return Nothing

      let xrefsPlain = [ (refloc, ent) | PlainXRef (refloc, ent) <- xrefs ]
      -- TODO handle IdlEntities without entity or with range annotations
      let xrefsIdl = [ (ent, rangeSpan) |
            IdlXRef (rangeSpan, Code.IdlEntity {
              idlEntity_entity = Just ent }) <- xrefs ]

      -- mark up symbols into normal format with static attributes
      -- Idl xrefs also needs db determination and range resolution

      refsPlain <- withRepo fileRepo $
        mapM (toReferenceSymbolPlain scsrepo srcFile offsets) xrefsPlain

      refsIdl <- case xrefsIdl of
        [] -> return []
        (ent, _) : _ ->
          -- TODO we assume all idl xrefs belong to the same db
          let lang = entityLanguage ent in
          case getLatestRepo (Glass.repoMapping env) dbInfo scsrepo lang of
            Nothing -> return []
            -- TODO we look only in the first repo available, this assumes
            --  the Idl repo comes first in the repomapping
            Just idlRepo -> do
              xrefs <- withRepo idlRepo $
                resolveEntitiesRange scsrepo fst xrefsIdl
              withRepo idlRepo $
                mapM (toReferenceSymbolIdl scsrepo srcFile offsets lang) xrefs

      let refs1 = refsPlain ++ refsIdl

      defs1 <- withRepo fileRepo $
        mapM (toDefinitionSymbol scsrepo srcFile offsets) defns

      xref_digests <- withRepo fileRepo $ do
        let fileMap = xrefFileMap refsPlain
        results <- fetchFileDigests (Map.size fileMap) (Map.keys fileMap)
        toDigestMap fileMap results

      let (refs, defs) = Attributes.extendAttributes
            (Attributes.fromSymbolId Attributes.SymbolKindAttr)
              kindMap (map (\XRefData{..} -> (xrefEntity, xrefSymbol)) refs1)
                defs1
      let digest = toDigest <$> fileDigest

      return (DocumentSymbols {..}, gleanDataLog, merr)

  where
    revisionAcceptable :: Glean.Repo -> Bool
    revisionAcceptable = case wantedRevision of
      Just rev | exactRevision -> \repo ->
        getDBRevision (scmRevisions dbInfo) repo scsrepo == rev
      _otherwise -> const True

    -- We will lookup digests by file id, but return to the user a map by
    -- glass path and scm repo
    xrefFileMap :: [XRefData] -> Map.Map (Glean.IdOf Src.File) (RepoName, Path)
    xrefFileMap xrefs = Map.fromList $ map (\XRefData{..} ->
        (xrefFile,
          let LocationRange{..} = referenceRangeSymbolX_target xrefSymbol
          in (locationRange_repository, locationRange_filepath)
      )) xrefs

-- | Wrapper for tracking symbol/entity pairs through processing
data DocumentSymbols = DocumentSymbols
  { refs :: [(Code.Entity, ReferenceRangeSymbolX)]
  , defs :: [(Code.Entity, DefinitionSymbolX)]
  , revision :: !Revision
  , contentMatch :: Maybe Bool
  , truncated :: !Bool
  , digest :: Maybe FileDigest
  , xref_digests :: Map.Map Text FileDigestMap
  }

emptyDocumentSymbols :: Revision -> DocumentSymbols
emptyDocumentSymbols revision =
  DocumentSymbols [] [] revision Nothing False Nothing mempty

-- | Drop any remnant entities after we are done with them
toDocumentSymbolResult :: DocumentSymbols -> DocumentSymbolListXResult
toDocumentSymbolResult DocumentSymbols{..} = DocumentSymbolListXResult{..}
  where
    documentSymbolListXResult_references = map snd refs
    documentSymbolListXResult_definitions = map snd defs
    documentSymbolListXResult_revision = revision
    documentSymbolListXResult_truncated = truncated
    documentSymbolListXResult_digest = digest
    documentSymbolListXResult_referenced_file_digests = xref_digests
    documentSymbolListXResult_content_match = contentMatch

type Defns = [(Code.Location,Code.Entity)]

-- | Which fileEntityLocations and fileEntityXRefLocation implementations to use
documentSymbolsForLanguage
  :: Maybe Int
  -> Maybe Language
  -> Bool  -- ^ include references?
  -> Bool -- ^ include xlang references
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w ([GenXRef], Defns, Bool)

-- For Cpp, we need to do a bit of client-side processing
documentSymbolsForLanguage
  mlimit (Just Language_Cpp) includeRefs includeXlangRefs fileId =
  Cxx.documentSymbolsForCxx mlimit includeRefs includeXlangRefs fileId

-- For everyone else, we just query the generic codemarkup predicates
documentSymbolsForLanguage mlimit _ includeRefs includeXlangRefs fileId = do
  (xrefs, trunc1) <- if includeRefs
    then searchRecursiveWithLimit mlimit $
      Query.fileEntityXRefLocations fileId includeXlangRefs
    else return ([], False)
  (defns, trunc2) <- searchRecursiveWithLimit mlimit $
    Query.fileEntityLocations fileId
  return (splitXRefs xrefs, defns, trunc1 || trunc2)

-- And build a line-indexed map of symbols, resolved to spans
-- With extra attributes loaded from any associated attr db
fetchDocumentSymbolIndex
  :: (Glean.Backend b, SnapshotBackend snapshotBackend)
  => Glass.Env
  -> GleanDBInfo
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> GleanBackend b
  -> snapshotBackend
  -> Maybe Language
  -> IO ((DocumentSymbolIndex, SnapshotStatus, QueryEachRepoLog),
       Maybe ErrorLogger)
fetchDocumentSymbolIndex env latest req opts be
    snapshotbe mlang = do
  ((DocumentSymbolListXResult{..}, status, gleanDataLog), merr1) <-
    fetchSymbolsAndAttributes env latest req opts be snapshotbe mlang

  --  refs defs revision truncated digest = result
  let lineIndex = toSymbolIndex documentSymbolListXResult_references
        documentSymbolListXResult_definitions
      totalSymCount = length documentSymbolListXResult_references +
        length documentSymbolListXResult_definitions

  let idxResult = DocumentSymbolIndex {
        documentSymbolIndex_symbols = lineIndex,
        documentSymbolIndex_revision = documentSymbolListXResult_revision,
        documentSymbolIndex_size = fromIntegral totalSymCount,
        documentSymbolIndex_truncated = documentSymbolListXResult_truncated,
        documentSymbolIndex_digest = documentSymbolListXResult_digest,
        documentSymbolIndex_referenced_file_digests =
          documentSymbolListXResult_referenced_file_digests,
        documentSymbolIndex_content_match =
          documentSymbolListXResult_content_match
      }
  return ((idxResult, status, gleanDataLog), merr1)

-- | Same repo generic attributes
documentSymbolKinds
  :: Maybe Int
  -> Maybe Language
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w
     (Map.Map Attributes.SymbolIdentifier Attributes, Maybe ErrorLogger)

-- It's not sound to key for all entities in file in C++ , due to traces
-- So we can't use the generic a attribute technique
documentSymbolKinds _mlimit (Just Language_Cpp) _fileId =
  return mempty

-- Anything else, just load from Glean
documentSymbolKinds mlimit _ fileId =
  searchFileAttributes Attributes.SymbolKindAttr mlimit fileId

searchFileAttributes
  :: (QueryType (Attributes.AttrRep key), Attributes.ToAttributes key)
  => key
  -> Maybe Int
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w
    (Map.Map Attributes.SymbolIdentifier Attributes, Maybe ErrorLogger)
searchFileAttributes key mlimit fileId = do
  eraw <- try $ Attributes.searchBy key mlimit $
                Attributes.queryFileAttributes key fileId
  repo <- Glean.haxlRepo
  case eraw of
    Left (err::SomeException) -- logic errors or transient errors
      -> return
        (mempty,
        Just (logError (GlassExceptionReason_attributesError $ textShow err)
              <> logError repo))
    Right raw
      -> return (Attributes.toAttrMap key raw, Nothing)

data XRefData = XRefData
  { xrefEntity :: !Code.Entity
  , xrefSymbol :: !ReferenceRangeSymbolX
  , xrefFile :: {-# UNPACK #-}!(Glean.IdOf Src.File)
  }

-- | Convert an Idl/Plain xref to a normal format
--   (includes attribute, source/target spans, symbol id)
toReferenceSymbolGen
  :: RepoName
  -> Src.File
  -> Maybe Range.LineOffsets
  -> Code.RangeSpan
  -> Src.File
  -> Code.Entity
  -> LocationRange
  -> Maybe Src.FileLocation
  -> Maybe Language
  -> Glean.RepoHaxl u w XRefData
toReferenceSymbolGen repoName file srcOffsets rangeSpanSrc xrefFile xrefEntity
  xrefRange mDestination mLang = do
  path <- GleanPath <$> Glean.keyOf file
  sym <- toSymbolId (fromGleanPath repoName path) xrefEntity
  attributes <- getStaticAttributes xrefEntity repoName sym mLang
  (target, xrefFile) <- case mDestination of
    -- if we have a best identifier location
    Just (Src.FileLocation fileLocation_file fileLocation_span) -> do
      let rangeSpan = Code.RangeSpan_span fileLocation_span
      t <- rangeSpanToLocationRange repoName fileLocation_file rangeSpan
      return (t, Glean.getId fileLocation_file)
    _ -> do
      return (xrefRange, Glean.getId xrefFile)
  -- resolved the local span to a location
  let range = rangeSpanToRange srcOffsets rangeSpanSrc
      xrefSymbol = ReferenceRangeSymbolX sym range target attributes
  return $ XRefData xrefEntity xrefSymbol xrefFile

-- | Convert plain entity to normal format
--   adapter to toReferenceSymbolGen
toReferenceSymbolPlain
  :: RepoName
  -> Src.File
  -> Maybe Range.LineOffsets
  -> XRef
  -> Glean.RepoHaxl u w XRefData
toReferenceSymbolPlain
  repoName file srcOffsets (Code.XRefLocation{..}, xrefEntity) = do
    -- reference target is a Declaration and an Entity
    let Code.Location{..} = xRefLocation_target
    xrefRange <- rangeSpanToLocationRange repoName location_file
      location_location
    toReferenceSymbolGen repoName file srcOffsets xRefLocation_source
      location_file xrefEntity xrefRange location_destination Nothing

-- | Convert idl entity to normal format
--   adapter to toReferenceSymbolGen
toReferenceSymbolIdl
  :: RepoName
  -> Src.File
  -> Maybe Range.LineOffsets
  -> Language
  -> ((Code.Entity, Code.RangeSpan), (Src.File, LocationRange))
  -> Glean.RepoHaxl u w XRefData
toReferenceSymbolIdl
  repoName file srcOffsets lang
  ((xrefEntity, rangeSpanSrc), (idlFile, xrefRange)) = do
    toReferenceSymbolGen repoName file srcOffsets rangeSpanSrc idlFile
      xrefEntity xrefRange Nothing (Just lang)

-- | Building a resolved definition symbol is just taking a direct xref to it,
-- and converting the bytespan, adding any static attributes
toDefinitionSymbol
  :: RepoName
  -> Src.File
  -> Maybe Range.LineOffsets
  -> (Code.Location, Code.Entity)
  -> Glean.RepoHaxl u w (Code.Entity, DefinitionSymbolX)
toDefinitionSymbol repoName file offsets (Code.Location {..}, entity) = do
  path <- GleanPath <$> Glean.keyOf file
  let symbolRepoPath@(SymbolRepoPath symbolRepo symbolPath) =
        fromGleanPath repoName path
  sym <- toSymbolId symbolRepoPath entity
  attributes <- getStaticAttributes entity repoName sym Nothing
  destination <- forM location_destination $ \Src.FileLocation{..} ->
    let rangeSpan = Code.RangeSpan_span fileLocation_span in
    rangeSpanToLocationRange repoName fileLocation_file rangeSpan

  -- full entity range (i.e. body of the method + signature)
  let range = rangeSpanToRange offsets location_location
  -- entity name/identifying location range (the name token for go-to-def)
  let nameRange = case destination of
        Just LocationRange{..} | symbolRepo == locationRange_repository &&
                                 symbolPath == locationRange_filepath
          -> Just locationRange_range
        _ -> Nothing

  return $ (entity,) $ DefinitionSymbolX sym range nameRange attributes

-- | Decorate an entity with 'static' attributes.
-- These are static in that they are derivable from the entity and
-- schema information alone, without additional repos.
-- They're expected to be cheap, as we call these once per entity in a file
getStaticAttributes
  :: Code.Entity
  -> RepoName
  -> SymbolId
  -> Maybe Language  -- Idl language
  -> Glean.RepoHaxl u w AttributeList
getStaticAttributes e repo sym mLang = memo $ do
  mLocalName <- toSymbolLocalName e
  mParent <- toSymbolQualifiedContainer e -- the "parent" of the symbol
  (mSignature, _xrefs) <- toSymbolSignatureText e repo sym Cxx.Unqualified
  mKind <- entityKind e -- optional glass-side symbol kind labels
  return $ AttributeList $ map (\(a,b) -> KeyedAttribute a b) $ catMaybes
    [ asLocalName <$> mLocalName
    , asParentAttr <$> mParent
    , asSignature  <$> mSignature
    , asKind <$> mKind
    , Just $ asLanguage (entityLanguage e)
    , asDefinitionType <$> entityDefinitionType e
    , asLangDeprecated =<< mLang
    , asLang =<< mLang
    ]
  where
    asLocalName (Name local) = ("symbolName", Attribute_aString local)
    asParentAttr (Name x) = ("symbolParent", Attribute_aString x)
    asSignature sig = ("symbolSignature", Attribute_aString sig)
    asKind kind = ("symbolKind",
      Attribute_aInteger (fromIntegral $ fromThriftEnum kind))
    asLanguage lang = ("symbolLanguage",
      Attribute_aInteger (fromIntegral $ fromThriftEnum lang))
    asDefinitionType kind = ("symbolDefinitionType",
      Attribute_aInteger (fromIntegral $ fromThriftEnum kind))
    asLangDeprecated Language_Thrift  = Just ("symbolIdl",
      Attribute_aString "thrift")
    asLangDeprecated _ = Nothing
    asLang Language_Thrift = Just ("crossLanguage",
      Attribute_aString "thrift")
    asLang _ = Nothing

    -- memoizing getStaticAttributes can save a lot of work because a
    -- symbol reference often occurs multiple times in a file.
    memo act
      | memoizable e = do
        gleanRepo <- Glean.haxlRepo
        Haxl.memo (GetStaticAttributes (gleanRepo, prune e, mLang)) act
      | otherwise = act

    -- languages that support Prune
    memoizable Code.Entity_cxx{} = True
    memoizable Code.Entity_hack{} = True
    memoizable Code.Entity_python{} = True
    memoizable _ = False

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
          (entityPairs, merr) <- case searchRelatedRequest_relatedBy of
            RelationType_Calls ->
              searchRelatedCalls repo searchRelatedRequest_relation entity lang
            _ -> do
              relatedLocatedEntities <-
                      Search.searchRelatedEntities limit
                        Search.ShowAll
                        searchRecursively
                        searchRelatedRequest_relation
                        searchRelatedRequest_relatedBy
                        (fst4 (decl entity))
                        repo
              return ((,Nothing) <$> relatedLocatedEntities, Nothing)

          descriptions <- if searchRelatedRequest_detailedResults
            then do
              let uniqSymIds = uniqBy (comparing snd) $ concat
                    [ [ e1, e2 ]
                    | (Search.RelatedLocatedEntities e1 e2, _) <- entityPairs ]
              let scmRevs = scmRevisions dbInfo
              descs <- mapM (mkDescribe repo scmRevs) uniqSymIds
                -- carefully in parallel!
              pure $ Map.fromAscList descs
            else pure mempty

          let symbolIdPairs =
                map (\(Search.RelatedLocatedEntities{..}, ranges) ->
                  RelatedSymbols (snd parentRL) (snd childRL) ranges
                ) entityPairs
          let result = SearchRelatedResult
                { searchRelatedResult_edges = symbolIdPairs
                , searchRelatedResult_symbolDetails = descriptions
                }
          pure (result, merr)
  where
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
      (xrefs_, _, _) <-
        documentSymbolsForLanguage Nothing (Just lang) True False
          (Glean.getId file)
      -- only consider non-Idl xrefs
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
