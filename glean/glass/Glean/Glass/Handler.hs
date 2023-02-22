{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.Handler
  (
  -- * listing symbols by file
    documentSymbolListX
  , documentSymbolIndex
  -- ** resolving spans to files
  , jumpTo

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

  -- * deprecated
  , searchBySymbolId

  -- * indexing requests
  , index

  -- * C++ specific methods
  , fileIncludeLocations
  , clangUSRToDefinition
  , clangUSRToReferenceRanges

  ) where

import Control.Monad ((<=<))
import Control.Exception ( throwIO, SomeException )
import Control.Monad.Catch ( throwM, try )
import Data.Either.Extra (eitherToMaybe, partitionEithers)
import Data.Default (def)
import Data.List as List ( sortOn )
import Data.List.Extra ( nubOrd, nubOrdOn, groupOn )
import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)
import Data.Maybe
import Data.Ord
import Data.Text ( Text )
import qualified Control.Concurrent.Async as Async
import qualified Data.Map.Strict as Map
import Data.HashMap.Strict ( HashMap )
import Data.HashSet ( HashSet )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Haxl.Prelude (forM, forM_)
import Logger.GleanGlass ( GleanGlassLogger )
import Logger.GleanGlassErrors ( GleanGlassErrorsLogger )
import Util.Logger ( loggingAction )
import Util.Text ( textShow )
import Util.List ( uniq, uniqBy )
import Util.Control.Exception (catchAll)
import Util.STM ( TVar, atomically, readTVar )
import qualified Logger.GleanGlass as Logger
import qualified Logger.GleanGlassErrors as ErrorsLogger

import Thrift.Protocol ( fromThriftEnum )
import Thrift.Api (Thrift)

import Glean.Angle as Angle ( Angle )
import Glean.Remote ( ThriftBackend(..), thriftServiceWithTimeout )
import qualified Glean
import Glean.Haxl.Repos as Glean
import qualified Glean.Repo as Glean
import qualified Glean.Util.Some as Glean
import qualified Glean.Util.Range as Range
import Glean.Util.ThriftService ( ThriftServiceOptions(..), runThrift )

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src

import qualified Glean.Glass.Attributes as Attributes
import Glean.Glass.Base
    ( GleanPath(..), SymbolRepoPath(..) )
import Glean.Glass.Logging
    ( LogRepo(..),
      ErrorTy(..),
      LogResult(..),
      LogRequest(..),
      LogError(..),
      ErrorLogger, errorText )
import Glean.Glass.Repos
    ( GleanDBName(unGleanDBName),
      ScmRevisions(..),
      firstAttrDB,
      filetype,
      findLanguages,
      findRepos,
      fromSCSRepo,
      lookupLatestRepos,
      toRepoName,
      selectGleanDBs,
      GleanDBAttrName(GleanDBAttrName, gleanAttrDBName) )
import Glean.Glass.Path
    ( toGleanPath, fromGleanPath )
import Glean.Glass.Range
    ( FileInfo(..),
      inclusiveRangeToExclusiveRange,
      getFile,
      getFileAndLines,
      rangeSpanToLocation,
      rangeSpanToLocationRange,
      memoRangeSpanToRange,
      rangeSpanToRange,
      resolveLocationToRange )
import Glean.Glass.SymbolId
    ( entityToAngle,
      symbolTokens,
      toQualifiedName,
      toSymbolLocalName,
      toSymbolQualifiedContainer,
      toShortCode,
      toSymbolId,
      entityDefinitionType,
      entityLanguage,
      entityKind,
      fromShortCode,
      languageToCodeLang
    )
import Glean.Glass.SymbolSig
    ( toSymbolSignatureText )
import Glean.Glass.Pretty.Cxx as Cxx (Qualified(..))
import qualified Glean.Glass.Relations.Hack as Hack
import Glean.Glass.SymbolKind ( findSymbolKind )
import Glean.Glass.Types
    ( SymbolPath(SymbolPath, symbolPath_range, symbolPath_repository,
                 symbolPath_filepath),
      SymbolKind(..),
      SymbolContext(..),
      QualifiedName(..),
      Name(..),
      Attribute(Attribute_aInteger, Attribute_aString),
      KeyedAttribute(KeyedAttribute),
      Name(Name),
      AttributeList(AttributeList),
      Attributes,
      FileIncludeLocationRequest(..),
      FileIncludeLocationResults(..),
      USRSymbolDefinition(..),
      USR(..),
      DefinitionSymbolX(DefinitionSymbolX),
      ReferenceRangeSymbolX(ReferenceRangeSymbolX),
      SearchBySymbolIdResult(SearchBySymbolIdResult),
      SearchRelatedRequest(..),
      SearchRelatedResult(..),
      SymbolSearchRequest(..),
      SymbolSearchResult(..),
      SymbolResult(..),
      SymbolSearchOptions(..),
      RelatedSymbols(..),
      RelationType(..),
      RelationDirection(..),
      ServerException(ServerException),
      SymbolDescription(..),
      SymbolComment(..),
      RelationDescription(..),
      SymbolId(..),
      Range,
      Location(..),
      LocationRange(..),
      DocumentSymbolIndex(..),
      DocumentSymbolListXResult(DocumentSymbolListXResult),
      Language(Language_Cpp, Language_Hack),
      Revision(Revision),
      Path,
      RepoName(..),
      RequestOptions(..),
      DocumentSymbolsRequest(..),
      FileXRefTarget(..),
      FileIncludeXRef(..),
      XRefFileList(..),
      RelatedNeighborhoodRequest(..),
      RelatedNeighborhoodResult(..),
      InheritedSymbols(..),
      RelationDirection (..),
      RelationType (..),
      rELATED_SYMBOLS_MAX_LIMIT,
      mAXIMUM_SYMBOLS_QUERY_LIMIT,
      Path (..), USRSymbolReference (USRSymbolReference) )
import Glean.Index.Types
  ( IndexRequest,
    IndexResponse)
import qualified Glean.Index.GleanIndexingService.Client as IndexingService
import Glean.Index.GleanIndexingService.Client ( GleanIndexingService )
import Glean.Impl.ThriftService ( ThriftService )

import qualified Glean.Glass.Env as Glass

import qualified Glean.Glass.Query as Query
import Glean.Glass.Query (
    SearchCase(..), SearchType(..),
    SearchScope(..), SearchMode(..),
    SingleSymbol, FeelingLuckyResult(..),
    QueryExpr(..)
  )
import qualified Glean.Glass.Query.Cxx as Cxx

import Glean.Glass.SymbolMap ( toSymbolIndex )
import Glean.Glass.Search as Search
    ( CodeEntityLocation(..),
      SearchEntity(..),
      SearchResult(Many, None, One, rest, message, initial),
      searchEntity,
      prefixSearchEntity )
import Glean.Glass.Utils
import qualified Data.Set as Set
import Data.Set ( Set )
import Glean.Glass.Attributes.SymbolKind
    ( symbolKindFromSymbolKind, symbolKindToSymbolKind )
import Glean.Glass.Annotations (getAnnotationsForEntity)
import Glean.Glass.Comments (getCommentsForEntity)
import qualified Glean.Glass.SearchRelated as Search
import Glean.Glass.Visibility (getInfoForEntity)
import Glean.Glass.SearchRelated (InheritedContainer, Recursive(NotRecursive))

-- | Runner for methods that are keyed by a file path
-- TODO : do the plumbing via a class rather than function composition
runRepoFile
  :: (LogResult t)
  => Text
  -> ( TVar Glean.LatestRepos
    -> DocumentSymbolsRequest
    -> RequestOptions
    -> GleanBackend (Glean.Some Glean.Backend)
    -> Maybe Language
    -> IO (t, Maybe ErrorLogger))
  -> Glass.Env
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> IO t
runRepoFile sym fn env req opts =
  withRepoFile sym env req repo file $ \(dbs,_) mlang ->
    fn repos req opts
        (GleanBackend (Glass.gleanBackend env) dbs)
          mlang
  where
    repos = Glass.latestGleanRepos env
    repo = documentSymbolsRequest_repository req
    file = documentSymbolsRequest_filepath req

-- | Discover navigable symbols in this file, resolving all bytespans and
-- adding any attributes
documentSymbolListX
  :: Glass.Env
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> IO DocumentSymbolListXResult
documentSymbolListX = runRepoFile "documentSymbolListX"
  fetchSymbolsAndAttributes

-- | Same as documentSymbolList() but construct a line-indexed map for easy
-- cursor/position lookup, and add extra metadata
documentSymbolIndex
  :: Glass.Env
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> IO DocumentSymbolIndex
documentSymbolIndex = runRepoFile "documentSymbolIndex" fetchDocumentSymbolIndex

firstOrErrors
  :: ReposHaxl u w [Either ErrorTy a] -> ReposHaxl u w (Either ErrorTy a)
firstOrErrors act = do
  result <- act
  let (fail, success) = partitionEithers result
  return $ case success of
    x:_ -> Right x
    [] -> Left $ AggregateError fail

-- | Given a Location , resolve it to a line:col range in the target file
jumpTo
  :: Glass.Env
  -> Location
  -> RequestOptions
  -> IO Range
jumpTo env@Glass.Env{..} req@Location{..} _opts =
  withRepoFile "jumpTo" env req repo file
    (\(gleanDBs,_) _lang -> backendRunHaxl GleanBackend{..} $ do
      result <-
        firstOrErrors $ queryEachRepo $ do
          repo <- Glean.haxlRepo
          resolveLocationToRange repo req
      case result of
        Left err -> throwM $ ServerException $ errorText err
        Right efile -> return (efile, Nothing))
  where
    repo = location_repository
    file = location_filepath

-- | Symbol-based find-refernces.
findReferences
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO [Location]
findReferences env@Glass.Env{..} sym RequestOptions{..} =
  withSymbol "findReferences" env sym (\(dbs,_revs,(repo, lang, toks)) ->
    fetchSymbolReferences repo lang toks limit
      (GleanBackend gleanBackend dbs))
  where
    limit = fmap fromIntegral requestOptions_limit

-- | Symbol-based find-refernces.
findReferenceRanges
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO [LocationRange]
findReferenceRanges env@Glass.Env{..} sym RequestOptions{..} =
  withSymbol "findReferenceRanges" env sym $ \(db,_revs,(repo, lang, toks)) ->
    fetchSymbolReferenceRanges repo lang toks limit
      (GleanBackend gleanBackend db)
  where
    limit = fmap fromIntegral requestOptions_limit

-- | Resolve a symbol identifier to its range-based location in the latest db
resolveSymbolRange
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO LocationRange
resolveSymbolRange env@Glass.Env{..} sym _opts =
  withSymbol "resolveSymbolRange" env sym $ \(db,_revs,(repo, lang, toks)) ->
    findSymbolLocationRange (GleanBackend gleanBackend db) repo lang toks

-- | Describe characteristics of a symbol
describeSymbol
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO SymbolDescription
describeSymbol env@Glass.Env{..} symId _opts =
  withSymbol "describeSymbol" env symId $
    \(gleanDBs, scmRevs, (scmRepo, lang, toks)) ->
      backendRunHaxl GleanBackend{..} $ do
        r <- Search.searchEntity lang toks
        (first :| rest, err) <- case r of
          None t -> throwM (ServerException t)
          One e -> return (e :| [], Nothing)
          Many { initial = e, rest = es } -> return (e :| es, Nothing) -- log?
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

toCodeEntityLoc :: SearchEntity Code.Entity -> CodeEntityLocation
toCodeEntityLoc SearchEntity{..} = CodeEntityLocation decl file rangespan name

-- | Given a description for a symbol, fold in extra comments, annotations
-- and locations from zero or more additional occurences of the same symbol
-- Should only be necessary for rare non-unique symbols, like Hack namespaces
--
combineDescriptions
  :: SymbolDescription -> SymbolDescription -> SymbolDescription
combineDescriptions y x =
  x { symbolDescription_annotations = symbolDescription_annotations x <>
        symbolDescription_annotations y -- <> Maybe [a]
    , symbolDescription_comments = symbolDescription_comments x <>
        symbolDescription_comments y
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
fileIncludeLocations env@Glass.Env{..} req opts =
  withRepoFile "fileIncludeLocations" env req repo rootfile $ \(gleanDBs,_) _ ->
    backendRunHaxl GleanBackend{..} $ do
      result <- firstOrErrors $ queryEachRepo $ do
        rev <- getRepoHash <$> Glean.haxlRepo
        efile <- getFile (toGleanPath (SymbolRepoPath repo rootfile))
        case efile of
          Left err -> return (Left err)
          Right file -> do
            includes <- Cxx.fileIncludeLocationsForCxx depth mlimit file
            Right <$> processFileIncludes repo rev includes
      case result of
        Left err -> throwM $ ServerException $ errorText err
        Right efile -> return (efile, Nothing)
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
  -> IO USRSymbolDefinition
clangUSRToDefinition env@Glass.Env{..} usr@(USR hash) _opts = withRepoLanguage
  "clangUSRToDefinition" env usr repo mlang $ \(gleanDBs,_) _ -> do
    backendRunHaxl GleanBackend{..} $ do
      result <- firstOrErrors $ queryEachRepo $ do
        rev <- getRepoHash <$> Glean.haxlRepo
        mdefn <- Cxx.usrHashToDeclaration hash
        case mdefn of
          Nothing -> -- either hash is unknown or decl is already defn
            pure (Left (EntitySearchFail "No definition result for hash"))
          Just (Code.Location{..},_entity) -> do
            range <- rangeSpanToLocationRange repo location_file
              location_location
            nameRange <- forM location_span
              (memoRangeSpanToRange location_file . Code.RangeSpan_span)
            pure (Right (USRSymbolDefinition {
              uSRSymbolDefinition_location = range,
              uSRSymbolDefinition_nameRange = nameRange,
              uSRSymbolDefinition_revision = rev
            }))
      case result of
        Left err -> throwM $ ServerException $ errorText err
        Right defn -> return (defn, Nothing)
  where
    repo = RepoName "fbsource"
    mlang = Just Language_Cpp

-- | Lookup the USR in Glean to yield and entity,
-- compute entity declToDef, return the pairs and other info.
clangUSRToReferenceRanges
  :: Glass.Env
  -> USR
  -> RequestOptions
  -> IO [USRSymbolReference]
clangUSRToReferenceRanges env@Glass.Env{..} usr@(USR hash) _opts =
  withRepoLanguage "clangUSRToReferenceRanges" env usr repo mlang  $
   \(gleanDBs,_) _ -> do
    backendRunHaxl GleanBackend{..} $ do
      result <- firstOrErrors $ queryEachRepo $ do
        refs <- Cxx.usrHashToXRefs mlimit hash
        res <- mapM convert refs
        pure (Right res)
      case result of
        Left err -> throwM $ ServerException $ errorText err
        Right defn -> return (defn, Nothing)
  where
    repo = RepoName "fbsource"
    mlang = Just Language_Cpp
    mlimit = fromIntegral <$> requestOptions_limit _opts
    convert (targetFile, rspan) = do
      r <- rangeSpanToLocationRange repo targetFile rspan
      return $ USRSymbolReference r

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

-- Worker to fill out symbol description metadata uniformly
mkSymbolDescription
  :: SymbolId
  -> ScmRevisions
  -> RepoName
  -> CodeEntityLocation
  -> Maybe SymbolContext
  -> Glean.RepoHaxl u w SymbolDescription
mkSymbolDescription symbolId scmRevs repo CodeEntityLocation{..} ctx = do
  range <- rangeSpanToLocationRange repo entityFile entityRange
  kind <- eitherToMaybe <$> findSymbolKind entity
  qname <- eThrow =<< toQualifiedName entity -- non-optional now
  let lang = entityLanguage entity
      score = mempty
  describeEntity scmRevs entity $
    SymbolResult symbolId range lang kind entityName score qname ctx

-- Worker to fill out symbol description metadata uniformly
mkBriefSymbolDescription
  :: SymbolId
  -> RepoName
  -> CodeEntityLocation
  -> Glean.RepoHaxl u w SymbolDescription
mkBriefSymbolDescription symbolId repo CodeEntityLocation{..} = do
  range <- rangeSpanToLocationRange repo entityFile entityRange
  kind <- eitherToMaybe <$> findSymbolKind entity
  qname <- eThrow =<< toQualifiedName entity -- non-optional now
  let lang = entityLanguage entity
      score = mempty
  briefDescribeEntity entity $
    SymbolResult symbolId range lang kind entityName score qname Nothing

-- | Search for entities by string name with kind and language filters
searchSymbol
  :: Glass.Env
  -> SymbolSearchRequest
  -> RequestOptions
  -> IO SymbolSearchResult
searchSymbol env@Glass.Env{..} req@SymbolSearchRequest{..} RequestOptions{..} =
  case selectGleanDBs scmRepo languageSet of
    Left err -> throwIO $ ServerException err
    Right rs -> case sFeelingLucky of
      Normal -> joinSearchResults mlimit terse sorted <$> Async.mapConcurrently
        (uncurry searchSymbolsIn) (Map.toList rs)
      -- lucky mode is quite different, as it has to make priority choices
      FeelingLucky -> joinLuckyResults <$> Async.mapConcurrently
        (uncurry searchLuckySymbolsIn) (Map.toList rs)
  where
    scmRepo = symbolSearchRequest_repo_name
    languageSet = symbolSearchRequest_language
    SymbolSearchOptions{..} = symbolSearchRequest_options
    terse = not symbolSearchOptions_detailedResults
    sorted = symbolSearchOptions_sortResults
    mlimit = fromIntegral <$> requestOptions_limit
    -- inner limit can be higher if we are sampling/sorting
    mlimitInner = if sorted then fmap (*10) mlimit else mlimit

    sCase = if symbolSearchOptions_ignoreCase then Insensitive else Sensitive
    sType = if symbolSearchOptions_exactMatch then Exact else Prefix
    sScope = if symbolSearchOptions_namespaceSearch then Scope else NoScope
    sString = Text.strip symbolSearchRequest_name -- drop leading whitespace
    sKinds = map symbolKindFromSymbolKind (Set.elems symbolSearchRequest_kinds)
    sLangs = mapMaybe languageToCodeLang (Set.elems languageSet)
    sFeelingLucky =
      if symbolSearchOptions_feelingLucky then FeelingLucky else Normal
    querySpec = Query.SearchQuery{..}
    searchQs = Query.buildSearchQuery sFeelingLucky querySpec

    -- run the same query across more than one glean db
    searchSymbolsIn
      :: RepoName
      -> Set GleanDBName
      -> IO Query.RepoSearchResult
    searchSymbolsIn repo dbs = case nonEmpty (Set.toList dbs) of
      Nothing -> pure []
      Just names -> withGleanDBs "searchSymbol" env req names $
        \gleanDBs scmRevs -> do
          res <- backendRunHaxl GleanBackend{..} $ Glean.queryAllRepos $ do
            res <- mapM (runSearch querySpec
                          repo scmRevs mlimitInner terse sString) searchQs
            return (nubOrd (concat res)) -- remove latter duplicates in n*log n
          pure (res, Nothing)

    -- In lucky mode, we avoid flattening, instead selecting from the first
    -- unique result found in priority order. We don't de-dup as we go.
    searchLuckySymbolsIn
      :: RepoName -> Set GleanDBName -> IO FeelingLuckyResult
    searchLuckySymbolsIn repo dbs = case nonEmpty (Set.toList dbs) of
      Nothing -> pure (FeelingLuckyResult [])
      Just names ->
        withGleanDBs "feelingLucky" env req names $ \gleanDBs scmRevs -> do
          res <- backendRunHaxl GleanBackend{..} $ Glean.queryEachRepo $ do
            -- we can conveniently limit to 2 matches per inner search
            rs <- mapM (runSearch querySpec repo scmRevs (Just 2) terse sString)
              searchQs
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
sortResults xs = map (List.sortOn relevance) (groupOn features xs)
  where
    -- we group on the language/kind to produce result sets
    -- then sort those results by score and alpha (note: not groupSortOn which
    -- would create too many classes to select from)
    features (SymbolResult{..},_desc) = Feature
      symbolResult_language
      symbolResult_kind

    relevance (SymbolResult{..},_desc) =
      (symbolResult_score, symbolResult_name,
        qualifiedName_container symbolResult_qname)

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
data Feature = Feature !Language !(Maybe SymbolKind)
  deriving Eq

-- | Glass needs to mark results by how good they are
-- For scope searches we don't care so much as the scope will filter
-- results. But for bare string searches we need to do a bit of work
data MatchType
  = ExactMatch             -- "FBID"
  | ExactInsensitiveMatch  -- "fbid" ~ "FBID"
  | Otherwise
  deriving (Eq, Ord, Bounded, Enum)

-- | Search for entities by symbol id prefix
-- (deprecated)
searchBySymbolId
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> IO SearchBySymbolIdResult
searchBySymbolId env@Glass.Env{..} symbolPrefix opts = do
  withLog "searchBySymbolId" env symbolPrefix $ \log -> do
    symids <- case partialSymbolTokens symbolPrefix of
          (Left pRepo, Left _, []) -> pure $ findRepos pRepo
          (Left pRepo, _, _) -> throwM $
            ServerException $ pRepo <> " is not a known repo"
          (Right repo, Left pLang, []) -> pure $
            findLanguages repo $ fromMaybe (Text.pack "") pLang
          (Right (RepoName repo), Left (Just pLang), _) -> throwM $
            ServerException $ pLang <> " is not a supported language in "<> repo
          (Right (RepoName repo), Left Nothing, _) -> throwM $
            ServerException $ "Missing language for " <> repo
          (Right repo, Right lang, tokens) -> findSymbols repo lang tokens
    return (SearchBySymbolIdResult symids, log, Nothing)

  where
    findSymbols :: RepoName -> Language -> [Text] -> IO [SymbolId]
    findSymbols repo lang tokens =
      withRepoLanguage "findSymbols" env symbolPrefix repo (Just lang) $
        \(gleanDBs, _) _ -> do
          backendRunHaxl GleanBackend{..} $ do
            symids <-  queryAllRepos $ do
              entities <- prefixSearchEntity lang limit tokens
              forM (take limit entities) $ \(entity, file, _, _) -> do
                path <- GleanPath <$> Glean.keyOf file
                toSymbolId (fromGleanPath repo path) entity
            return (take limit symids, Nothing)
    limit = maybe 50 fromIntegral $ requestOptions_limit opts

-- | Normalized (to Glean) paths
data FileReference =
  FileReference {
    _repoName :: !RepoName,
    theGleanPath :: !GleanPath
  }

toFileReference :: RepoName -> Path -> FileReference
toFileReference repo path =
  FileReference repo (toGleanPath $ SymbolRepoPath repo path)

-- | Bundle of glean db handle resources
data GleanBackend b =
  GleanBackend {
    gleanBackend :: b,
    gleanDBs :: NonEmpty (GleanDBName, Glean.Repo)
  }

backendRunHaxl
  :: Glean.Backend b => GleanBackend b -> (forall u. ReposHaxl u w a) -> IO a
backendRunHaxl GleanBackend{..} =
  runHaxlAllRepos gleanBackend (fmap snd gleanDBs)

-- | Symbol search: try to resolve the line/col range of an entity
findSymbolLocationRange
  :: Glean.Backend b
  => GleanBackend b
  -> RepoName
  -> Language
  -> [Text]
  -> IO (LocationRange, Maybe ErrorLogger)
findSymbolLocationRange b repo lang toks = backendRunHaxl b $
  withEntity rangeSpanToLocationRange repo lang toks

-- | Symbol search for references
fetchSymbolReferences
  :: Glean.Backend b
  => RepoName
  -> Language
  -> [Text]
  -> Maybe Int
  -> GleanBackend b
  -> IO ([Location], Maybe ErrorLogger)
fetchSymbolReferences scsrepo lang toks limit b = backendRunHaxl b $ do
  er <- symbolToAngleEntity lang toks
  case er of
    Left err -> return ([], Just err)
    Right (entityRepo, query, searchErr) -> do
      locs <- withRepo entityRepo $ do
        let convert (targetFile, rspan) =
              rangeSpanToLocation scsrepo targetFile rspan
        uses <- searchWithLimit limit $ Query.findReferenceRangeSpan query
        mapM convert uses
      return (locs, fmap logError searchErr)

-- | Symbol search for references as ranges.
fetchSymbolReferenceRanges
  :: Glean.Backend b
  => RepoName
  -> Language
  -> [Text]
  -> Maybe Int
  -> GleanBackend b
  -> IO ([LocationRange], Maybe ErrorLogger)
fetchSymbolReferenceRanges scsrepo lang toks limit b = backendRunHaxl b $ do
  er <- symbolToAngleEntity lang toks
  case er of
    Left err -> return ([], Just err)
    Right (entityRepo, query, searchErr) -> do
      ranges <- withRepo entityRepo $ do
        let convert (targetFile, rspan) =
              rangeSpanToLocationRange scsrepo targetFile rspan
        uses <- searchWithLimit limit $ Query.findReferenceRangeSpan query
        mapM convert uses
      return (ranges, fmap logError searchErr)

-- | Search for a symbol and return an Angle query that identifies the entity
-- Return the angle query and the Glean.repo to which it applies.
--
-- Todo: this entity query only works on the db it came from.
-- Should be in the type
--
symbolToAngleEntity
  :: Language
  -> [Text]
  -> ReposHaxl u w
      (Either GleanGlassErrorsLogger
        (Glean.Repo, Angle Code.Entity, Maybe ErrorTy))
symbolToAngleEntity lang toks = do
  r <- Search.searchEntity lang toks -- search everywhere for something a match
  (SearchEntity{..}, searchErr) <- case r of
    None t -> throwM (ServerException t) -- return [] ?
    One e -> return (e, Nothing)
      -- n.b. picks the first entity only
    Many { initial = e, message = t } -> return (e, Just (EntitySearchFail t))
  return $ case entityToAngle decl of
      Left err -> Left (logError (EntityNotSupported err))
      Right query -> Right (entityRepo, query, searchErr)

-- | Run an action on the result of looking up a symbol id
withEntity
  :: (RepoName -> Src.File -> Code.RangeSpan -> RepoHaxl u w a)
  -> RepoName
  -> Language
  -> [Text]
  -> Glean.ReposHaxl u w (a, Maybe GleanGlassErrorsLogger)
withEntity f scsrepo lang toks = do
  r <- Search.searchEntity lang toks
  (SearchEntity{..}, err) <- case r of
    None t -> throwM (ServerException t)
    One e -> return (e, Nothing)
    Many { initial = e, message = t } -> return (e, Just (EntitySearchFail t))
  (, fmap logError err) <$> withRepo entityRepo (f scsrepo file rangespan)


-- Find all symbols and refs in file and add all attributes
fetchSymbolsAndAttributes
  :: Glean.Backend b
  => TVar Glean.LatestRepos
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> GleanBackend b
  -> Maybe Language
  -> IO (DocumentSymbolListXResult, Maybe ErrorLogger)
fetchSymbolsAndAttributes latest req opts be mlang = do
  let
    file = toFileReference
      (documentSymbolsRequest_repository req)
      (documentSymbolsRequest_filepath req)
    mlimit = Just (fromIntegral (fromMaybe mAXIMUM_SYMBOLS_QUERY_LIMIT
      (requestOptions_limit opts)))
    includeRefs = documentSymbolsRequest_include_refs req
  (res1, logs) <- fetchDocumentSymbols file mlimit includeRefs be mlang
  res2 <- addDynamicAttributes latest file mlimit be res1
  return (res2, logs)

-- Find all references and definitions in the file
fetchDocumentSymbols
  :: Glean.Backend b
  => FileReference
  -> Maybe Int
  -> Bool  -- ^ include references?
  -> GleanBackend b
  -> Maybe Language
  -> IO (DocumentSymbols, Maybe ErrorLogger)

fetchDocumentSymbols (FileReference scsrepo path) mlimit includeRefs b mlang =
  backendRunHaxl b $ do
    efile <- firstOrErrors $ queryEachRepo $ do
      repo <- Glean.haxlRepo
      getFileAndLines repo path

    case efile of
      Left err ->
        return $ (, Just (logError err)) $
          DocumentSymbols [] [] (revision b)
        where
          -- Use first db's revision
          revision GleanBackend {gleanDBs = ((_, repo) :| _)} =
            Revision $ Glean.repo_hash repo

      Right FileInfo{..} -> do

      -- from Glean, fetch xrefs and defs in batches
      (xrefs,defns) <- withRepo fileRepo $
        documentSymbolsForLanguage mlimit mlang includeRefs fileId
      (kindMap, merr) <- withRepo fileRepo $
        documentSymbolKinds mlimit mlang fileId

      -- mark up symbols into normal format with static attributes
      refs1 <- withRepo fileRepo $
        mapM (toReferenceSymbol scsrepo srcFile offsets) xrefs
      defs1 <- withRepo fileRepo $
        mapM (toDefinitionSymbol scsrepo srcFile offsets) defns

      let (refs, defs) = Attributes.extendAttributes
            (Attributes.fromSymbolId Attributes.SymbolKindAttr)
              kindMap refs1 defs1
      let revision = getRepoHash fileRepo

      return (DocumentSymbols {..}, merr)

-- TODO (T122759515): Get repo revision from db properties
getRepoHash :: Glean.Repo -> Revision
getRepoHash repo = Revision (Text.take 40 (Glean.repo_hash repo))

getRepoHashForLocation
  :: LocationRange -> ScmRevisions -> Glean.Repo -> Revision
getRepoHashForLocation LocationRange{..} scmRevs repo =
  maybe (getRepoHash repo) Revision $ do
    scmRepoToHash <- HashMap.lookup repo $ scmRevisions scmRevs
    HashMap.lookup (unRepoName locationRange_repository) scmRepoToHash

-- | Wrapper for tracking symbol/entity pairs through processing
data DocumentSymbols = DocumentSymbols
  { refs :: [(Code.Entity, ReferenceRangeSymbolX)]
  , defs :: [(Code.Entity, DefinitionSymbolX)]
  , revision :: !Revision
  }

-- | Drop any remnant entities after we are done with them
toDocumentSymbolResult :: DocumentSymbols -> DocumentSymbolListXResult
toDocumentSymbolResult DocumentSymbols{..} = DocumentSymbolListXResult
  (map snd refs) (map snd defs) revision

--
-- | Check if this db / lang pair has additional dynamic attributes
-- and add them if so
--
addDynamicAttributes
  :: Glean.Backend b
  => TVar Glean.LatestRepos
  -> FileReference
  -> Maybe Int
  -> GleanBackend b
  -> DocumentSymbols
  -> IO DocumentSymbolListXResult
addDynamicAttributes latestRepos repofile mlimit be syms = do
  -- combine additional dynamic attributes
  mattrs <- getSymbolAttributes latestRepos repofile mlimit be
  return $ extend mattrs syms
  where
    extend [] syms = toDocumentSymbolResult syms
    extend ((GleanDBAttrName _ attrKey, attrMap) : xs) syms =
      let keyFn = Attributes.fromSymbolId attrKey
          (refs',defs') = Attributes.extendAttributes keyFn attrMap
            (refs syms)
            (defs syms)
      in extend xs $ syms { refs = refs' , defs = defs' }

type XRefs = [(Code.XRefLocation,Code.Entity)]
type Defns = [(Code.Location,Code.Entity)]

-- | Which fileEntityLocations and fileEntityXRefLocation implementations to use
documentSymbolsForLanguage
  :: Maybe Int
  -> Maybe Language
  -> Bool  -- ^ include references?
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w (XRefs, Defns)

-- For Cpp, we need to do a bit of client-side processing
documentSymbolsForLanguage mlimit (Just Language_Cpp) includeRefs fileId =
  Cxx.documentSymbolsForCxx mlimit includeRefs fileId

-- For everyone else, we just query the generic codemarkup predicates
documentSymbolsForLanguage mlimit _ includeRefs fileId = do
  xrefs <- if includeRefs
    then searchRecursiveWithLimit mlimit $ Query.fileEntityXRefLocations fileId
    else return []
  defns <- searchRecursiveWithLimit mlimit $ Query.fileEntityLocations fileId
  return (xrefs,defns)

-- And build a line-indexed map of symbols, resolved to spans
-- With extra attributes loaded from any associated attr db
fetchDocumentSymbolIndex
  :: Glean.Backend b
  => TVar Glean.LatestRepos
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> GleanBackend b
  -> Maybe Language
  -> IO (DocumentSymbolIndex, Maybe ErrorLogger)
fetchDocumentSymbolIndex latest req opts be mlang = do
  (DocumentSymbolListXResult refs defs revision, merr1) <-
    fetchSymbolsAndAttributes latest req opts be mlang

  return $ (,merr1) DocumentSymbolIndex {
    documentSymbolIndex_symbols = toSymbolIndex refs defs,
    documentSymbolIndex_revision = revision,
    documentSymbolIndex_size = fromIntegral $ length defs + length refs
  }

-- Work out if we have extra attribute dbs and then run the queries
getSymbolAttributes
  :: Glean.Backend b
  => TVar Glean.LatestRepos
  -> FileReference
  -> Maybe Int
  -> GleanBackend b
  -> IO
     [(GleanDBAttrName, Map.Map Attributes.SymbolIdentifier Attributes)]
getSymbolAttributes repos repofile mlimit be@GleanBackend{..} = do
  mAttrDBs <- forM (map fst $ toList gleanDBs) $ getLatestAttrDB repos
  attrs <- backendRunHaxl be $ do
    forM (catMaybes mAttrDBs) $
      \(attrDB, attr@(GleanDBAttrName _ attrKey){- existential key -}) ->
        withRepo attrDB $ do
        (attrMap,_merr2) <- genericFetchFileAttributes attrKey
          (theGleanPath repofile) mlimit
        return $ Just (attr, attrMap)
  return $ catMaybes attrs

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

-- | External (non-local db) Attributes of symbols. Just Hack only for now
genericFetchFileAttributes
  :: (QueryType (Attributes.AttrRep key)
     , Attributes.ToAttributes key)
  => key
  -> GleanPath
  -> Maybe Int
  -> RepoHaxl u w
      (Map.Map Attributes.SymbolIdentifier Attributes, Maybe ErrorLogger)

genericFetchFileAttributes key path mlimit = do
  efile <- getFile path
  case efile of
    Left err ->
      return (mempty, Just (logError err))
    Right fileId -> do
      searchFileAttributes key mlimit (Glean.getId fileId)

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
  case eraw of
    Left (err::SomeException) -- logic errors or transient errors
      -> return (mempty, Just (logError $ AttributesError $ textShow err))
    Right raw
      -> return (Attributes.toAttrMap key raw, Nothing)

-- | Like toReferenceSymbol but we convert the xref target to a src.Range
toReferenceSymbol
  :: RepoName
  -> Src.File
  -> Maybe Range.LineOffsets
  -> (Code.XRefLocation, Code.Entity)
  -> Glean.RepoHaxl u w (Code.Entity, ReferenceRangeSymbolX)
toReferenceSymbol repoName file srcOffsets (Code.XRefLocation {..}, entity) = do
  path <- GleanPath <$> Glean.keyOf file
  sym <- toSymbolId (fromGleanPath repoName path) entity
  attributes <- getStaticAttributes entity repoName

  target <- rangeSpanToLocationRange repoName location_file
    location_location
  targetNameRange <- forM location_span
    (memoRangeSpanToRange location_file . Code.RangeSpan_span)

  return $ (entity,) $
    ReferenceRangeSymbolX sym range target attributes targetNameRange
  where
    -- reference target is a Declaration and an Entity
    Code.Location{..} = xRefLocation_target
    -- resolved the local span to a location
    range = rangeSpanToRange srcOffsets xRefLocation_source

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
  sym <- toSymbolId (fromGleanPath repoName path) entity
  attributes <- getStaticAttributes entity repoName
  return $ (entity,) $ DefinitionSymbolX sym range attributes nameRange
  where
    range = rangeSpanToRange offsets location_location
    nameRange = rangeSpanToRange offsets . Code.RangeSpan_span <$>
      location_span

-- | Decorate an entity with 'static' attributes.
-- These are static in that they are derivable from the entity and
-- schema information alone, without additional repos.
-- They're expected to be cheap, as we call these once per entity in a file
getStaticAttributes
  :: Code.Entity -> RepoName -> Glean.RepoHaxl u w AttributeList
getStaticAttributes e repo = do
  mLocalName <- toSymbolLocalName e
  mParent <- toSymbolQualifiedContainer e -- the "parent" of the symbol
  (mSignature, _xrefs) <- toSymbolSignatureText e repo Cxx.Unqualified
  mKind <- entityKind e -- optional glass-side symbol kind labels
  return $ AttributeList $ map (\(a,b) -> KeyedAttribute a b) $ catMaybes
    [ asLocalName <$> mLocalName
    , asParentAttr <$> mParent
    , asSignature  <$> mSignature
    , asKind <$> mKind
    , Just $ asLanguage (entityLanguage e)
    , asDefinitionType <$> entityDefinitionType e
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

-- | Given an SCS repo name, and a candidate path, find latest Glean dbs or
-- throw. Returns the chosen db name and Glean repo handle
getGleanRepos
  :: TVar Glean.LatestRepos
  -> TVar ScmRevisions
  -> RepoName
  -> Maybe Language
  -> IO (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
getGleanRepos latestGleanDBs scmRevisions scsrepo mlanguage = do
  let gleanDBNames = fromSCSRepo scsrepo mlanguage
  case gleanDBNames of
    [] ->  throwIO $ ServerException $ "No repository found for: " <>
      unRepoName scsrepo <>
        maybe "" (\x -> " (" <> toShortCode x <> ")") mlanguage
    (x:xs) -> getSpecificGleanDBs latestGleanDBs scmRevisions (x :| xs)

-- | If you already know the set of dbs you need, just get them.
getSpecificGleanDBs
  :: TVar Glean.LatestRepos
  -> TVar ScmRevisions
  -> NonEmpty GleanDBName
  -> IO (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
getSpecificGleanDBs latestGleanDBs scmRevisions gleanDBNames = do
  (dbs, scmRevs) <- atomically $ do
    dbs <- lookupLatestRepos latestGleanDBs $ toList gleanDBNames
    scmRevs <- readTVar scmRevisions
    pure (dbs, scmRevs)
  case dbs of
    [] -> throwIO $ ServerException $ "No Glean dbs found for: " <>
            Text.intercalate ", " (map unGleanDBName $ toList gleanDBNames)
    db:dbs -> return (db :| dbs, scmRevs)

-- | Get glean db for an attribute type
getLatestAttrDB
  :: TVar Glean.LatestRepos
  -> GleanDBName
  -> IO (Maybe (Glean.Repo, GleanDBAttrName))
getLatestAttrDB allRepos gleanDBName = case firstAttrDB gleanDBName of
  Nothing -> return Nothing
  Just attrDBName -> atomically $ do
    dbs <- lookupLatestRepos allRepos [gleanAttrDBName attrDBName]
    return $ case dbs of
      [] -> Nothing
      db:_ -> Just (snd db, attrDBName)

withLog
  :: (LogRequest req, LogError req, LogResult res)
  => Text
  -> Glass.Env
  -> req
  -> (GleanGlassLogger -> IO (res, GleanGlassLogger, Maybe ErrorLogger))
  -> IO res
withLog cmd env req action = do
  fst <$> loggingAction
    (runLog env cmd)
    logResult
    (do
      (res, log, merr) <- action $ logRequest req
      forM_ merr $ \e -> runErrorLog env cmd (e <> logError req)
      return (res, log))

-- | Wrapper to enable perf logging, log the db names, and stats for
-- intermediate steps, and internal errors.
withLogDB
  :: (LogRequest req, LogError req, LogError dbs, LogRepo dbs, LogResult res)
  => Text
  -> Glass.Env
  -> req
  -> IO dbs
  -> Maybe Language
  -> (dbs -> Maybe Language -> IO (res, Maybe ErrorLogger))
  -> IO res
withLogDB cmd env req fetch mlanguage run =
  withLog cmd env req $ \log -> do
    db <- fetch
    (res,merr) <- run db mlanguage
    let err = fmap (<> logError db) merr
    return (res, log <> logRepo db, err)

withGleanDBs
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> a
  -> NonEmpty GleanDBName
  -> (NonEmpty (GleanDBName, Glean.Repo)
        -> ScmRevisions -> IO (b, Maybe ErrorLogger))
  -> IO b
withGleanDBs method env@Glass.Env{..} req dbNames fn = do
  withLogDB method env req
    (getSpecificGleanDBs latestGleanRepos repoScmRevisions dbNames)
    Nothing
    (\(dbs,revs) _mlang -> fn dbs revs)

-- | Run an action that provides a repo and maybe a language, log it
withRepoLanguage
  :: (LogError a, LogRequest a, LogResult b)
  => Text
  -> Glass.Env
  -> a
  -> RepoName
  -> Maybe Language
  -> (  (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
     -> Maybe Language
     -> IO (b, Maybe ErrorLogger))
  -> IO b
withRepoLanguage method env@Glass.Env{..} req repo mlanguage fn = do
  withLogDB method env req
    (getGleanRepos latestGleanRepos repoScmRevisions repo mlanguage)
    mlanguage
    fn

-- | Run an action that provides a repo and filepath, log it
withRepoFile :: (LogError a, LogRequest a, LogResult b) => Text
  -> Glass.Env
  -> a
  -> RepoName
  -> Path
  -> (  (NonEmpty (GleanDBName,Glean.Repo), ScmRevisions)
     -> Maybe Language
     -> IO (b, Maybe ErrorLogger))
  -> IO b
withRepoFile method env req repo file fn = do
  withRepoLanguage method env req repo (filetype file) fn

-- | Run an action that provides a symbol id, log it
withSymbol
  :: LogResult c
  => Text
  -> Glass.Env
  -> SymbolId
  -> ((NonEmpty (GleanDBName, Glean.Repo),
        ScmRevisions, (RepoName, Language, [Text]))
  -> IO (c, Maybe ErrorLogger))
  -> IO c
withSymbol method env@Glass.Env{..} sym fn =
  withLogDB method env sym
    (case symbolTokens sym of
      Left err -> throwM $ ServerException err
      Right req@(repo, lang, _toks) -> do
        (dbs, revs) <-
          getGleanRepos latestGleanRepos repoScmRevisions repo (Just lang)
        return (dbs, revs, req))
    Nothing
    (\db _mlang -> fn db)

runLog :: Glass.Env -> Text -> GleanGlassLogger -> IO ()
runLog env cmd log = Logger.runLog (Glass.logger env) $
  log <> Logger.setMethod cmd

runErrorLog :: Glass.Env -> Text -> GleanGlassErrorsLogger -> IO ()
runErrorLog env cmd err = ErrorsLogger.runLog (Glass.logger env) $
  err <> ErrorsLogger.setMethod cmd

-- | Return a brief description with only required fields set.
-- More efficient for cases where we don't need everything
-- TBD should be a different type in thrift
briefDescribeEntity
  :: Code.Entity -> SymbolResult -> Glean.RepoHaxl u w SymbolDescription
briefDescribeEntity ent SymbolResult{..} = do
  symbolDescription_signature <- fst <$> toSymbolSignatureText ent repo
    Cxx.Qualified
  pure SymbolDescription{..}
  where
    symbolDescription_repo_hash = Revision mempty
    symbolDescription_annotations = mempty
    symbolDescription_comments = mempty
    symbolDescription_pretty_comments = mempty
    symbolDescription_visibility = Nothing
    symbolDescription_modifiers = mempty
    symbolDescription_type_xrefs  = mempty
    symbolDescription_sym_other_locations = mempty
    symbolDescription_extends_relation  = def
    symbolDescription_contains_relation  = def
    symbolDescription_location = def

    symbolDescription_sym = symbolResult_symbol
    symbolDescription_kind = symbolResult_kind
    symbolDescription_language = symbolResult_language
    symbolDescription_name = symbolResult_qname

    repo = locationRange_repository symbolResult_location

    symbolDescription_sym_location = LocationRange {
      locationRange_range = locationRange_range symbolResult_location,
      locationRange_repository = locationRange_repository symbolResult_location,
      locationRange_filepath = locationRange_filepath symbolResult_location
    }

-- | Return a description for a single Entity with a unique location.
describeEntity
  :: ScmRevisions
  -> Code.Entity
  -> SymbolResult
  -> Glean.RepoHaxl u w SymbolDescription
describeEntity scmRevs ent SymbolResult{..} = do
  symbolDescription_repo_hash <-
    getRepoHashForLocation symbolResult_location scmRevs <$> Glean.haxlRepo
  let symbolDescription_name = symbolResult_qname
  symbolDescription_annotations <- eThrow =<< getAnnotationsForEntity repo ent
  symbolDescription_pretty_comments <- eThrow =<< getCommentsForEntity repo ent
  -- backwards compat until deprecated, we just make a copy
  let symbolDescription_comments = map symbolComment_location
        symbolDescription_pretty_comments
  (symbolDescription_visibility, symbolDescription_modifiers)
     <- eThrow =<< getInfoForEntity ent
  (symbolDescription_signature, symbolDescription_type_xrefs)
    <- toSymbolSignatureText ent repo Cxx.Qualified
  symbolDescription_extends_relation <-
    relationDescription RelationType_Extends
  symbolDescription_contains_relation <-
    relationDescription RelationType_Contains
  pure SymbolDescription{..}
  where
    symbolDescription_sym = symbolResult_symbol
    symbolDescription_kind = symbolResult_kind
    symbolDescription_language = symbolResult_language

    symbolDescription_sym_location = symbolResult_location
    symbolDescription_sym_other_locations = []

    repo = locationRange_repository symbolResult_location

    -- deprecated. we already have sym_location
    symbolDescription_location = SymbolPath {
      symbolPath_range = locationRange_range symbolResult_location,
      symbolPath_repository = locationRange_repository symbolResult_location,
      symbolPath_filepath = locationRange_filepath symbolResult_location
    }

    relationDescription relatedBy = do
      parents <- describeRelation RelationDirection_Parent
      children <- describeRelation RelationDirection_Child

      let firstParent = Search.parentRL <$> listToMaybe parents
          firstChild = Search.childRL <$> listToMaybe children
          relationDescription_firstParent = snd <$> firstParent
          relationDescription_firstChild = snd <$> firstChild
          relationDescription_hasMoreParents =  length parents > 1
          relationDescription_hasMoreChildren = length children > 1

      relationDescription_firstParentName <- case firstParent of
        Nothing -> pure Nothing
        Just (p,_) -> eitherToMaybe <$> toQualifiedName (fst4 p)
      relationDescription_firstChildName <- case firstChild of
        Nothing -> pure Nothing
        Just (p,_) -> eitherToMaybe <$> toQualifiedName (fst4 p)

      pure RelationDescription{..}
      where
        describeRelation relation = Search.searchRelatedEntities 2
          NotRecursive relation relatedBy ent repo

parentContainer
  :: RepoName -> Code.Entity -> Glean.RepoHaxl u w (Maybe SymbolContext)
parentContainer repo ent = do
  parents <- Search.searchRelatedEntities 2 NotRecursive
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

eThrow :: Either Text a -> RepoHaxl u w a
eThrow (Right x) = pure x
eThrow (Left err) = throwM $ ServerException err

fst4 :: (a,b,c,d) -> a
fst4 (x,_,_,_) = x

partialSymbolTokens
  :: SymbolId
  -> (Either Text RepoName, Either (Maybe Text) Language, [Text])
partialSymbolTokens (SymbolId symid) =
  (repoName, language, fromMaybe [] partialSym)
  where
    tokens = Text.split (=='/') symid
    (partialRepoName, partialLang, partialSym) = case tokens of
      [] -> error "partialSymbolTokens: the impossible has happened"
      [f] -> (f, Nothing, Nothing)
      [f, s] -> (f, Just s, Nothing)
      (f:s:rest) -> (f, Just s, Just rest)

    repoName = case toRepoName partialRepoName of
                Just repoName -> Right repoName
                Nothing -> Left partialRepoName
    language = case (partialSym, fromShortCode =<< partialLang) of
                  (Just _, Just lang) -> Right lang
                  (Just _, Nothing) -> Left partialLang
                  _ -> Left partialLang

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
searchRelated env@Glass.Env{..}
    sym RequestOptions{..} SearchRelatedRequest{..} =
  withSymbol "searchRelated" env sym $
    \(gleanDBs, scmRevs, (repo, lang, toks)) ->
      backendRunHaxl GleanBackend{..} $ do
        entity <- searchFirstEntity lang toks
        (entityPairs, descriptions) <- withRepo (entityRepo entity) $ do
          edgePairs <- withRepo (entityRepo entity) $ do
              Search.searchRelatedEntities limit
                searchRecursively
                searchRelatedRequest_relation
                searchRelatedRequest_relatedBy
                (decl entity)
                repo

          descs <- if searchRelatedRequest_detailedResults
            then do
              let uniqSymIds = uniqBy (comparing snd) $ concat
                    [ [ e1, e2 ]
                    | Search.RelatedLocatedEntities e1 e2 <- edgePairs ]
              descs <- mapM (mkDescribe repo scmRevs) uniqSymIds
                -- carefully in parallel!
              pure $ Map.fromAscList descs
            else pure mempty
          pure (edgePairs, descs)

        let symbolIdPairs = map (\Search.RelatedLocatedEntities{..} ->
                RelatedSymbols (snd parentRL) (snd childRL)
              ) entityPairs
        let result = SearchRelatedResult
              { searchRelatedResult_edges = symbolIdPairs
              , searchRelatedResult_symbolDetails = descriptions
              }
        pure (result, Nothing)
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

--
-- Extract the "API" neighborhood of a symbol
--
searchRelatedNeighborhood
  :: Glass.Env
  -> SymbolId
  -> RequestOptions
  -> RelatedNeighborhoodRequest
  -> IO RelatedNeighborhoodResult
searchRelatedNeighborhood env@Glass.Env{..} sym RequestOptions{..}
    RelatedNeighborhoodRequest{..} =
  withSymbol "searchRelatedNeighborhood" env sym $
    \(gleanDBs, scmRevs, (repo, lang, toks)) ->
      backendRunHaxl GleanBackend{..} $ do
        baseEntity <- searchFirstEntity lang toks
        let lang = entityLanguage (decl baseEntity)
        NeighborRawResult a b c d e descs <- searchNeighbors
          repo scmRevs lang baseEntity
        let result = RelatedNeighborhoodResult {
              relatedNeighborhoodResult_childrenContained = a,
              relatedNeighborhoodResult_childrenExtended = b,
              relatedNeighborhoodResult_parentsExtended = d,

              relatedNeighborhoodResult_containsParents = symbolIdPairs c,
              relatedNeighborhoodResult_inheritedSymbols =
                map inheritedSymbolIdSets e,
              relatedNeighborhoodResult_symbolDetails = descs
            }
        return (result, Nothing)
  where
    -- being careful to keep things with clear data dependencies
    searchNeighbors
      :: RepoName -> ScmRevisions -> Language -> SearchEntity Code.Entity
      -> ReposHaxl u w NeighborRawResult
    searchNeighbors repo scmRevs lang baseEntity =
      withRepo (entityRepo baseEntity) $ do
        a <- childrenContains1Level (decl baseEntity) repo
        b <- childrenExtends1Level (decl baseEntity) repo
        c <- parentContainsNLevel (decl baseEntity) repo
        d <- parentExtends1Level (decl baseEntity) repo
        -- all contained symbols of inherited parents
        -- n.b. not all are actually in scope after we resolve names
        (eFull, edges, kinds) <- inheritedNLevel (decl baseEntity) repo
        -- now filter out any names that are shadowed
        let (!eFinal,!overrides) = partitionInheritedScopes lang sym edges
              kinds a eFull
        -- syms visible to the client, we need their full details
        let !syms = uniqBy (comparing snd) $ fromSearchEntity sym baseEntity :
                a ++ flattenEdges c ++ concatMap snd eFinal
                  -- full descriptions of final methods
        descs0 <- Map.fromAscList <$> mapM (mkDescribe repo scmRevs) syms
        overrides' <- mapM addQName overrides
        let !descriptions = patchDescriptions lang descs0 overrides'
        -- brief descriptions for inherited things
        descs1 <- Map.fromAscList <$> mapM (mkBriefDescribe repo)
            (uniqBy (comparing snd) $ (b ++ d) ++ map fst eFinal)
        return (NeighborRawResult (map snd a) (map snd b) c (map snd d) eFinal
          (Map.union descriptions descs1))

    -- building map of sym id -> descriptions, by first occurence
    mkDescribe repo scmRevs e@(_,SymbolId rawSymId) =
      (rawSymId,) <$> describe repo scmRevs e
    describe repo scmRevs ((entity, entityFile, entityRange, entityName), symId)
      = mkSymbolDescription symId scmRevs repo CodeEntityLocation{..} Nothing

    -- for children by inheritance, we only need name, kind, parent name, sym id
    -- and signature. In particular, we don't e.g. need comments or annotations
    -- or type xrefs
    mkBriefDescribe repo e@(_,SymbolId rawSymId) =
      (rawSymId,) <$> briefDescribe repo e
    briefDescribe repo
        ((entity, entityFile, entityRange, entityName), symId)
      = mkBriefSymbolDescription symId repo CodeEntityLocation{..}

    -- for overrides, we just need to know the parent qname of the entity
    -- no need to compute the full details()
    addQName ((entity, _file, _span, _name), symId)  = do
      qName <- eThrow =<< toQualifiedName entity
      return (symId, qName)

    childrenContains1Level :: SearchRelatedList u w
    childrenContains1Level baseEntity repo = map Search.childRL <$>
        Search.searchRelatedEntities
      (fromIntegral relatedNeighborhoodRequest_children_limit)
      Search.NotRecursive
      RelationDirection_Child
      RelationType_Contains
      baseEntity
      repo
    -- children by `extends`. typically a list. this includes method overrides
    -- note that some methods have a lot of overrides (50k+) so be careful with
    -- the limit values
    childrenExtends1Level :: SearchRelatedList u w
    childrenExtends1Level baseEntity repo = map Search.childRL <$>
        Search.searchRelatedEntities
      (fromIntegral relatedNeighborhoodRequest_inherited_limit)
      Search.NotRecursive
      RelationDirection_Child
      RelationType_Extends
      baseEntity
      repo
    -- Direct inheritance parents
    parentExtends1Level :: SearchRelatedList u w
    parentExtends1Level baseEntity repo = map Search.parentRL <$>
        Search.searchRelatedEntities limit
      Search.NotRecursive
      RelationDirection_Parent
      RelationType_Extends
      baseEntity
      repo
    -- N levels of container hierarchy
    parentContainsNLevel :: SearchRelatedQuery u w
    parentContainsNLevel baseEntity repo = Search.searchRelatedEntities
      (fromIntegral relatedNeighborhoodRequest_parent_depth)
      Search.Recursive
      RelationDirection_Parent
      RelationType_Contains
      baseEntity
      repo
    -- Inherited symbols: the contained children of N levels of extended parents
    inheritedNLevel :: Code.Entity -> RepoName
        -> RepoHaxl u w
            ([InheritedContainer]
             ,HashMap SymbolId (HashSet SymbolId)
             ,HashMap SymbolId SymbolKind
             )
    inheritedNLevel baseEntity repo = do
      topoEdges <- Search.searchRelatedEntities
        (fromIntegral relatedNeighborhoodRequest_inherited_limit)
        Search.Recursive
        RelationDirection_Parent
        RelationType_Extends
        baseEntity
        repo
      -- keep topological ordering handy
      let symTable = Search.edgesToTopoMap topoEdges
      -- reduce to just the unique parent symbols
      let parents = uniq (map Search.parentRL topoEdges)
      -- fetch the parent kinds (class, trait etc)
      kinds <- mapM (\e -> (snd e,) <$> findSymbolKind (toEntity e)) parents
      -- and fetch their children concurrently
      inherited <- mapM (childrenOf repo) parents
      return (inherited,symTable,toKindTable kinds)

    childrenOf repo parent = (parent,) <$>
      childrenContains1Level (toEntity parent) repo

    toEntity ((entity, _file, _rangespan, _name), _symId) = entity

    symbolIdPairs = map (\Search.RelatedLocatedEntities{..} ->
      RelatedSymbols (snd parentRL) (snd childRL))

    inheritedSymbolIdSets :: InheritedContainer -> InheritedSymbols
    inheritedSymbolIdSets (parent, children) = InheritedSymbols {
        inheritedSymbols_base = snd parent,
        inheritedSymbols_provides = map snd children
      }

    fromSearchEntity symId SearchEntity{..} =
      ((decl, file, rangespan, name), symId)

    flattenEdges pairs = concat
      [ [ e1, e2 ] | Search.RelatedLocatedEntities e1 e2 <- pairs ]

    limit = fromIntegral $ case requestOptions_limit of
      Just x | x < rELATED_SYMBOLS_MAX_LIMIT -> x
      _ -> rELATED_SYMBOLS_MAX_LIMIT

toKindTable :: [(SymbolId,Either Text SymbolKind)] ->HashMap SymbolId SymbolKind
toKindTable xs = HashMap.fromList [ (symId, kind) | (symId, Right kind) <- xs ]

-- | Apply any additional client-side filtering of what is in scope,
-- according to language rules
partitionInheritedScopes
  :: Language
  -> SymbolId
  -> HashMap SymbolId (HashSet SymbolId)
  -> HashMap SymbolId SymbolKind
  -> [Search.LocatedEntity]
  -> [InheritedContainer]
  -> ([InheritedContainer], HashMap SymbolId Search.LocatedEntity)
partitionInheritedScopes lang symId edges kinds locals inherited = case lang of
  Language_Hack -> Hack.difference edges kinds symId locals inherited
  _ -> (inherited, mempty)

-- | And once we filter out hidden inherited things, infer any missing
-- synthetic extends relationships
patchDescriptions
  :: Language
  -> Map.Map Text SymbolDescription
  -> HashMap SymbolId (SymbolId, QualifiedName)
  -> Map.Map Text SymbolDescription
patchDescriptions lang descs overrides = case lang of
  Language_Hack -> Hack.patchDescriptions descs overrides
  _ -> descs

type SearchRelatedQuery u w = Code.Entity -> RepoName
      -> RepoHaxl u w [Search.RelatedLocatedEntities]
type SearchRelatedList u w = Code.Entity -> RepoName
      -> RepoHaxl u w [Search.LocatedEntity]

data NeighborRawResult = NeighborRawResult {
    _containedChildren :: ![SymbolId],
    _extendedChildren :: ![SymbolId],
    _parentContains :: ![Search.RelatedLocatedEntities],
    _extendedParents :: ![SymbolId],
    _inherited :: ![InheritedContainer],
    _descriptions :: !(Map.Map Text SymbolDescription)
  }

searchFirstEntity
  :: Language -> [Text] -> Glean.ReposHaxl u w (SearchEntity Code.Entity)
searchFirstEntity lang toks = do
  r <- Search.searchEntity lang toks
  case r of
    None t -> throwM (ServerException t)
    One e -> return e
    Many { initial = e } -> return e

-- Processing indexing requests
index :: Glass.Env -> IndexRequest -> IO IndexResponse
index env r = withIndexingService env $ IndexingService.index r

withIndexingService
  :: Glass.Env
  -> Thrift GleanIndexingService a
  -> IO a
withIndexingService env act =
  case mThriftBackend of
    Nothing -> err "no remote service connection available"
    Just (ThriftBackend config evb _ _ _) -> do
      let service :: ThriftService GleanIndexingService
          service = thriftServiceWithTimeout config opts
          onErr e = err $ "glean error: " <> Text.pack (show e)
      runThrift evb service act `catchAll` onErr
  where
    Glass.IndexBackend mThriftBackend = Glass.gleanIndexBackend env
    opts = def { processingTimeout = Just timeout }
    err e = throwIO $ ServerException e
    timeout = 60 -- seconds
