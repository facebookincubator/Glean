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

module Glean.Glass.Handler.Documents
  (
  -- * listing symbols by file
    documentSymbolListX
  , documentSymbolListXSnapshot
  , documentSymbolIndex
  ) where

import Control.Monad
import Control.Exception ( SomeException )
import Control.Monad.Catch ( try )
import Data.Hashable
import Data.List as List ( find )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Data.Text ( Text, isPrefixOf )
import qualified Control.Concurrent.Async as Async
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import qualified Haxl.Core as Haxl
import Glean.Util.ToAngle
import Util.Text ( textShow )

import Thrift.Protocol ( fromThriftEnum )

import qualified Glean
import Glean.Haxl as Glean ( keyOf, haxlRepo )
import Glean.Haxl.Repos as Glean
import Glean.Util.Some as Glean ( Some )
import qualified Glean.Util.Range as Range

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src

import qualified Glean.Glass.Attributes as Attributes
import Glean.Glass.Base
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
import Glean.Glass.RepoMapping (
   mirrorConfig, Mirror(Mirror)
  )
import qualified Glean.Glass.Env as Glass
import Glean.Glass.XRefs
  ( GenXRef(..), XRef, resolveEntitiesRange )
import Glean.Glass.SymbolMap ( toSymbolIndex )

import Glean.Glass.SnapshotBackend
  ( SnapshotBackend(getSnapshot),
    SnapshotStatus() )
import qualified Glean.Glass.SnapshotBackend as Snapshot
import Glean.Glass.Env (Env' (tracer, sourceControl, useSnapshotsForSymbolsList))
import Glean.Glass.SourceControl
import Glean.Glass.Tracing (traceSpan)
import qualified Glean.Glass.Utils as Utils
import Glean.Glass.Utils ( fst4 )
import Logger.GleanGlass (GleanGlassLogger)


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
  fst4 <$>
    runRepoFile
      "documentSymbolListX"
      (if useSnapshots
        then fetchSymbolsAndAttributes env
        else (\dbInfo req opts be _ mlang -> do
                ((res, log, attrLog), err) <- fetchSymbolsAndAttributesGlean
                  env dbInfo req opts be Nothing mlang
                return ((res, Snapshot.Unrequested, log, attrLog), err)
              )
      )
      env r opts

-- | Variation of documentSymbolListX
-- Always use Glean (and not XDB snapshot backend).
-- Possibly use a different backend o resolve xlang
-- entities to their location.
--
-- Use case: constructing document symbols lists
-- snapshots from local dbs, and resolving xlang
-- entities location from global dbs not locally indexed.
documentSymbolListXSnapshot
  :: Glass.Env
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> Maybe (Some Glean.Backend, GleanDBInfo)
  -> IO DocumentSymbolListXResult
documentSymbolListXSnapshot env r opts mGleanBe = do
  fst4 <$>
    runRepoFile
      "documentSymbolListXSnapshot"
      (\dbInfo req opts be _ mlang -> do
          ((res, log, attrLog), err) <- fetchSymbolsAndAttributesGlean
            env dbInfo req opts be mGleanBe mlang
          return ((res, Snapshot.Unrequested, log, attrLog), err)
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
  fst4 <$>
    runRepoFile
      "documentSymbolIndex"
      (fetchDocumentSymbolIndex env)
      env r opts

-- | Normalized (to Glean) paths
data FileReference =
  FileReference {
    _repoName :: !RepoName,
    theGleanPath :: !GleanPath
  }

toFileReference :: RepoName -> Path -> FileReference
toFileReference repo path =
  FileReference repo (toGleanPath $ SymbolRepoPath repo path)

repoPathToMirror :: RepoName -> Path -> Maybe Mirror
repoPathToMirror repository (Path filepath) =
  let isReqInMirror (Mirror mirrorRepo prefix _) =
        repository == mirrorRepo && isPrefixOf prefix filepath in
  find isReqInMirror mirrorConfig

translateMirroredRepoListXResult
  :: DocumentSymbolsRequest
  -> DocumentSymbolListXResult
  -> DocumentSymbolListXResult
translateMirroredRepoListXResult
  (DocumentSymbolsRequest repository path _ _ _) res =
  case repoPathToMirror repository path of
    Just (Mirror mirror prefix origin) ->
      Utils.translateDocumentSymbolListXResult origin mirror prefix Nothing res
    _ -> res

fetchSymbolsAndAttributesGlean
  :: Glean.Backend b
  => Glass.Env
  -> GleanDBInfo
  -> DocumentSymbolsRequest
  -> RequestOptions
  -> GleanBackend b
  -> Maybe (Some Glean.Backend, GleanDBInfo)
  -> Maybe Language
  -> IO (
       (DocumentSymbolListXResult, QueryEachRepoLog, GleanGlassLogger),
       Maybe ErrorLogger
     )
fetchSymbolsAndAttributesGlean
  env@Glass.Env{tracer, gleanBackend}
  dbInfo req opts be mOtherBackend mlang = do
  (res1, gLogs, elogs) <- traceSpan tracer "fetchDocumentSymbols" $
    fetchDocumentSymbols env file path mlimit
      (requestOptions_revision opts)
      (requestOptions_exact_revision opts)
      withContentHash
      ExtraSymbolOpts{..} be mlang dbInfo

  (res2, attributesLog) <- traceSpan tracer "addDynamicAttributes" $
    addDynamicAttributes env dbInfo repo opts
      file mlimit be res1

  let be = fromMaybe (gleanBackend, dbInfo) mOtherBackend
  res3 <- resolveXlangXrefs env path res2 repo be

  let res4 = toDocumentSymbolResult res3
  let res5 = translateMirroredRepoListXResult req res4
  return ((res5, gLogs, attributesLog), elogs)
  where
    repo = documentSymbolsRequest_repository req
    path = documentSymbolsRequest_filepath req
    file = toFileReference repo path
    oIncludeRefs = documentSymbolsRequest_include_refs req
    oLineRange = documentSymbolsRequest_range req
    withContentHash = shouldFetchContentHash opts
    oIncludeXlangRefs = case opts of
      RequestOptions { requestOptions_feature_flags = Just
        FeatureFlags { featureFlags_include_xlang_refs = Just True } } -> True
      _ -> documentSymbolsRequest_include_xlang_refs req

    mlimit = Just (fromIntegral (fromMaybe mAXIMUM_SYMBOLS_QUERY_LIMIT
      (requestOptions_limit opts)))

shouldFetchContentHash :: RequestOptions -> Bool
shouldFetchContentHash opts =
  not (requestOptions_exact_revision opts) &&
  (requestOptions_content_check opts ||
    requestOptions_matching_revision opts)

type FetchDocumentSymbols =
  ((DocumentSymbolListXResult, SnapshotStatus,
    QueryEachRepoLog, GleanGlassLogger), Maybe ErrorLogger)

-- | When an explicit revision is requested, we attempt to fetch both
-- Glean results and a snapshot. This function chooses which result to
-- use based on the requested revision, the revision of the result and
-- RequestOptions.
chooseGleanOrSnapshot
  :: RequestOptions
  -> Revision
  -> (
       (DocumentSymbolListXResult, QueryEachRepoLog, GleanGlassLogger),
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
          , FoundNone, mempty)
        , Just $ logError $
            GlassExceptionReason_matchingRevisionNotAvailable $
              unRevision revision
        )

    addStatus st ((res, gleanLog, attributesLog), mlogger) =
      ((res, st, gleanLog, attributesLog), mlogger)

    getResultRevision ((x, _ ,_), _) = documentSymbolListXResult_revision x
    isRevisionHit rev = (== rev) . getResultRevision

    resultContentMatch ((r, _, _), _) =
      documentSymbolListXResult_content_match r

returnSnapshot
  :: DocumentSymbolListXResult
  -> SnapshotStatus
  -> FetchDocumentSymbols
returnSnapshot queryResult match =
  ((setContentMatch queryResult, match,
    QueryEachRepoUnrequested, mempty), Nothing)
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
  | ((_,_,_,_), Just ErrorLogger {errorTy}) <- res, all isNoSrcFileFact errorTy,
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
  -> IO (
      (DocumentSymbolListXResult, SnapshotStatus, QueryEachRepoLog,
         GleanGlassLogger),
      Maybe ErrorLogger
    )
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

    addStatus st ((res, gleanLog, attributeLog), mlogger) =
      ((res, st, gleanLog, attributeLog), mlogger)

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
            return $ (==) <$> wanted <*> mine
              -- Nothing if either getContentHash failed

    getFromGlean =
      Glass.withAllocationLimit env $
        fetchSymbolsAndAttributesGlean env dbInfo req opts be Nothing mlang

xRefDataToRefEntitySymbol :: XRefData -> (Code.Entity, ReferenceRangeSymbolX)
xRefDataToRefEntitySymbol XRefData{..} = (xrefEntity, xrefSymbol)

-- Find all references and definitions in a file that might be in a set of repos
fetchDocumentSymbols
  :: Glean.Backend b
  => Glass.Env
  -> FileReference
  -> Path  -- ^ original path in the repo
  -> Maybe Int
  -> Maybe Revision
  -> Bool  -- ^ exact revision only?
  -> Bool  -- ^ fetch file content hash?
  -> ExtraSymbolOpts
  -> GleanBackend b
  -> Maybe Language
  -> GleanDBInfo
  -> IO (DocumentSymbols, QueryEachRepoLog, Maybe ErrorLogger)
fetchDocumentSymbols env@Glass.Env{..} (FileReference scsrepo path)
    repoPath mlimit wantedRevision exactRevision fetchContentHash extraOpts
    b mlang dbInfo = do
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
          documentSymbolsForLanguage
            mlimit mlang extraOpts
            fileId
        -- todo this could be done in Glean in a single pass
        (kinds, merr) <- withRepo fileRepo $
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
              return $ (==) <$> contentHash <*> wantedHash
                -- Nothing if either getContentHash failed
            | otherwise ->
              return Nothing

        let xrefsPlain = [ (refloc, ent) | PlainXRef (refloc, ent) <- xrefs ]

        refsPlain <- withRepo fileRepo $
          mapM (toReferenceSymbolPlain scsrepo srcFile offsets) xrefsPlain

        -- mark up symbols into normal format with static attributes

        defs1 <- withRepo fileRepo $
          mapM (toDefinitionSymbol scsrepo srcFile offsets) defns

        xref_digests <- withRepo fileRepo $ do
          let fileMap = xrefFileMap refsPlain
          results <- fetchFileDigests (Map.size fileMap) (Map.keys fileMap)
          toDigestMap fileMap results

        let digest = toDigest <$> fileDigest

        -- only handle XlangEntities with known entity
        let unresolvedXrefsXlang = [ (ent, rangeSpan) |
              XlangXRef (rangeSpan, Code.IdlEntity {
                idlEntity_entity = Just ent }) <- xrefs ]

        let (refs, defs, _) = Attributes.augmentSymbols
              Attributes.SymbolKindAttr
              kinds
              (xRefDataToRefEntitySymbol <$> refsPlain)
              defs1
        return (DocumentSymbols { srcFile = Just srcFile, .. },
                gleanDataLog, merr)

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

-- | Xlang xrefs needs db determination and range resolution
--   possibly using a separate backend than the one computed
--   from the origin query.
--   Always choose latest db, as exactRevision can't usually be
--   enforced for xlang refs
resolveXlangXrefs
  :: Glean.Backend b
  => Glass.Env
  -> Path
  -> DocumentSymbols
  -> RepoName
  -> (b, GleanDBInfo)
  -> IO DocumentSymbols
resolveXlangXrefs
  env@Glass.Env{tracer, sourceControl, repoMapping}
  path
  docSyms@DocumentSymbols{..}
  scsrepo
  (gleanBackend, dbInfo) = do
  case (unresolvedXrefsXlang, srcFile) of
    ((ent, _) : _, Just srcFile) -> do
       -- we assume all xlang xrefs belong to the same db
       -- we pick the xlang dbs based on target lang and
       -- repo. Corner case: the document is in a mirror repo,
       -- use to origin repo to determine xlang db
      let lang = entityLanguage ent
          targetRepo = case repoPathToMirror scsrepo path of
              Just (Mirror _mirror _prefix origin) -> origin
              Nothing -> scsrepo
      gleanDBs <- getGleanRepos tracer sourceControl repoMapping dbInfo
        targetRepo (Just lang) ChooseLatest Nothing
      let gleanBe = GleanBackend {gleanDBs, tracer, gleanBackend}
      xlangRefs <- backendRunHaxl gleanBe env $ do
        xrefsXlang <- withRepo (snd (NonEmpty.head gleanDBs)) $ do
          xrefs <- resolveEntitiesRange targetRepo fst unresolvedXrefsXlang
          mapM (toReferenceSymbolXlang targetRepo srcFile offsets lang) xrefs
        return $ xRefDataToRefEntitySymbol <$> xrefsXlang
      return $ docSyms  { refs = refs ++ xlangRefs, unresolvedXrefsXlang = [] }
    _ -> return docSyms

-- | Wrapper for tracking symbol/entity pairs through processing
data DocumentSymbols = DocumentSymbols
  { refs :: [(Code.Entity, ReferenceRangeSymbolX)]
  , defs :: [(Code.Entity, DefinitionSymbolX)]
  , revision :: !Revision
  , contentMatch :: Maybe Bool
  , truncated :: !Bool
  , digest :: Maybe FileDigest
  , xref_digests :: Map.Map Text FileDigestMap
  , unresolvedXrefsXlang :: [(Code.Entity, Code.RangeSpan)]
  , srcFile :: Maybe Src.File
  , offsets :: Maybe Range.LineOffsets
  }

emptyDocumentSymbols :: Revision -> DocumentSymbols
emptyDocumentSymbols revision =
  DocumentSymbols [] [] revision Nothing False Nothing mempty mempty Nothing
    Nothing

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
  -> IO ((
    DocumentSymbolIndex, SnapshotStatus,
    QueryEachRepoLog, GleanGlassLogger),
    Maybe ErrorLogger)
fetchDocumentSymbolIndex env latest req opts be
    snapshotbe mlang = do
  ((DocumentSymbolListXResult{..}, status, gleanDataLog, attrLog), merr1) <-
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
  return ((idxResult, status, gleanDataLog, attrLog), merr1)

-- | Same repo generic attributes
documentSymbolKinds
  :: Maybe Int
  -> Maybe Language
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w
     ([Attributes.AttrRep Attributes.SymbolKindAttr], Maybe ErrorLogger)

-- It's not sound to key for all entities in file in C++ , due to traces
-- So we can't use the generic a attribute technique
documentSymbolKinds _mlimit (Just Language_Cpp) _fileId =
  return mempty

-- Anything else, just load from Glean
documentSymbolKinds mlimit _ fileId =
  searchFileAttributes Attributes.SymbolKindAttr mlimit fileId

searchFileAttributes
  :: Attributes.ToAttributes key
  => key
  -> Maybe Int
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w
    ([Attributes.AttrRep key], Maybe ErrorLogger)
searchFileAttributes key mlimit fileId = do
  eraw <- try $ Attributes.queryForFile key mlimit fileId
  repo <- Glean.haxlRepo
  case eraw of
    Left (err::SomeException) -- logic errors or transient errors
      -> return
        (mempty,
        Just (logError (GlassExceptionReason_attributesError $ textShow err)
              <> logError repo))
    Right raw
      -> return (raw, Nothing)

data XRefData = XRefData
  { xrefEntity :: !Code.Entity
  , xrefSymbol :: !ReferenceRangeSymbolX
  , xrefFile :: {-# UNPACK #-}!(Glean.IdOf Src.File)
  }

-- | Convert an Xlang/Plain xref to a normal format
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

-- | Convert xlang entity to normal format
--   adapter to toReferenceSymbolGen
toReferenceSymbolXlang
  :: RepoName
  -> Src.File
  -> Maybe Range.LineOffsets
  -> Language
  -> ((Code.Entity, Code.RangeSpan), (Src.File, LocationRange))
  -> Glean.RepoHaxl u w XRefData
toReferenceSymbolXlang
  repoName file srcOffsets lang
  ((xrefEntity, rangeSpanSrc), (xlangFile, xrefRange)) = do
    toReferenceSymbolGen repoName file srcOffsets rangeSpanSrc xlangFile
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
  -> Maybe Language  -- Xlang language
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
    asLang Language_Thrift = Just ("crossLanguage",
      Attribute_aString "thrift")
    asLang Language_Python = Just ("crossLanguage",
      Attribute_aString "python")
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

-- -----------------------------------------------------------------------------
-- Attributes

--
-- | Check if this db / lang pair has additional dynamic attributes
-- and add them if so
--
addDynamicAttributes
  :: Glean.Backend b
  => Glass.Env
  -> GleanDBInfo
  -> RepoName
  -> RequestOptions
  -> FileReference
  -> Maybe Int
  -> GleanBackend b
  -> DocumentSymbols
  -> IO (DocumentSymbols, GleanGlassLogger)
addDynamicAttributes env dbInfo repo opts repofile mlimit be syms = do
  -- combine additional dynamic attributes
  mattrs <- getSymbolAttributes env
    dbInfo repo opts repofile mlimit be
  return $ extend mattrs mempty [] syms
  where
    extend [] log dblog syms = (syms, log <> logResult dblog)
    extend (augment : xs) log dblog syms =
      extend xs newLog newDBLog (syms { refs = refs' , defs = defs' })
      where
      (refs',defs',log', dblog') = augment (refs syms) (defs syms)
      newLog = log <> log'
      newDBLog = dblog' : dblog
      -- Note: it'll only log one if multiple attrs use the same fields

type Augment =
   [Attributes.RefEntitySymbol] ->
   [Attributes.DefEntitySymbol] ->
   ([Attributes.RefEntitySymbol],
    [Attributes.DefEntitySymbol],
    GleanGlassLogger, AttrDBsLog)

-- Work out if we have extra attribute dbs and then run the queries
getSymbolAttributes
  :: Glean.Backend b
  => Glass.Env
  -> GleanDBInfo
  -> RepoName
  -> RequestOptions
  -> FileReference
  -> Maybe Int
  -> GleanBackend b
  -> IO [Augment]
getSymbolAttributes env dbInfo repo opts repofile mlimit
    be@GleanBackend{..} = do
  mAttrDBs <-
    getLatestAttrDBs tracer (sourceControl env) (Glass.repoMapping env)
      dbInfo repo opts
  backendRunHaxl be env $ do
    forM mAttrDBs $
      \(attrDB, GleanDBAttrName _ attrKey{- existential key -}) ->
        withRepo attrDB $ do
          (attrs,_merr2) <- genericFetchFileAttributes attrKey
            (theGleanPath repofile) mlimit
          return $ \refs defs ->
            case Attributes.augmentSymbols attrKey attrs refs defs of
               (refs, defs, log) ->
                (refs, defs, logResult log, AttrDBsLog attrDB)

-- | External (non-local db) Attributes of symbols. Just Hack only for now
genericFetchFileAttributes
  :: Attributes.ToAttributes key
  => key
  -> GleanPath
  -> Maybe Int
  -> RepoHaxl u w ([Attributes.AttrRep key], Maybe ErrorLogger)

genericFetchFileAttributes key path mlimit = do
  efile <- getFile path
  repo <- Glean.haxlRepo
  case efile of
    Left err ->
      return (mempty, Just (logError err <> logError repo))
    Right fileId -> do
      searchFileAttributes key mlimit (Glean.getId fileId)
