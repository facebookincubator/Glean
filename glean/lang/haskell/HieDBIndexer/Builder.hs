{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

-- @lint-ignore-every TODOHACK
-- @lint-ignore-every Arc(LINEWRAP)

module HieDBIndexer.Builder (buildXrefMapFiles) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Concurrent (getNumCapabilities)
import Control.Exception (
  Exception (displayException),
  SomeException,
  catch,
  handle,
 )
import Control.Monad (unless, when, filterM)
import Data.Array.Unboxed (
  listArray,
  (!),
 )
import Data.Char (isAlphaNum)
import Data.Containers.ListUtils (nubOrd)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Database.SQLite.Simple (
  NamedParam (..),
  Query,
  foldNamed,
  queryNamed,
 )
#if __GLASGOW_HASKELL__ >= 902
import Data.IORef
import GHC.Plugins (mkSplitUniqSupply)
import GHC.Iface.Ext.Binary (HieFileResult (hie_file_result), readHieFile,
  NameCacheUpdater(..))
import GHC.Iface.Env (updNameCache)
import GHC.Iface.Ext.Types (HieFile (..))
import GHC.Unit.Module.Name (moduleNameString)
import GHC.Types.Name.Cache (NameCache, initNameCache)
import GHC.Types.Name.Occurrence (occNameString)
#else
import GhcPlugins (mkSplitUniqSupply)
import HieBin (HieFileResult (hie_file_result), readHieFile)
import HieTypes (HieFile (..))
import Module (moduleNameString)
import NameCache (NameCache, initNameCache)
import OccName (occNameString)
#endif
import HieDBIndexer.Options (HieDBIndexerOptions (..))
import HieDBIndexer.Trace
import HieDBIndexer.Types (
  ByteSpan (..),
  Definition (..),
  FileLineMap,
  FileLocation (..),
  FileXRefMap,
  IndexerBatchOutput (IndexerBatchOutput),
  LineLengthArray,
  NodeDefinition (..),
  Range (..),
  Vertex,
  XReferences (..),
 )
import HieDb (
  HieDb (getConn),
  RefRow (..),
  setHieTrace,
  withHieDb,
 )
import System.Directory (doesFileExist)
import System.FilePath (makeRelative, (</>))
import qualified Text.Printf as Text
import TextShow (showt)

{- | Build a Glean DB with XReference facts that will be used to power
 click-to-definition.
-}
buildXrefMapFiles ::
  Tracer Text ->
  HieDBIndexerOptions FilePath ->
  IO (FileLineMap, [IndexerBatchOutput])
buildXrefMapFiles logger opts@HieDBIndexerOptions {..} = do
  itExists <- doesFileExist sources
  unless itExists $ error $ "hiedb not found at " <> sources
  withHieDb sources $ \db -> do
    when hiedbTrace $
      setHieTrace db (Just $ logMsg logger)
    logMsg logger $ "Path of hiedb = " <> Text.pack sources
    logMsg logger $ "Repo path = " <> Text.pack repoPath

    allVertices <- getDefsVertices logger repoPath db

    let numOfVertices = length allVertices
    logMsg logger $
      "Number of vertices in the graph = " <> showt numOfVertices

    let repoFiles = nubOrd (map (srcFile repoPath) allVertices)
    missingFiles <- Set.fromList <$>
      filterM (fmap not . doesFileExist) repoFiles

    let filteredVertices =
          filter ((`Set.notMember` missingFiles) . srcFile repoPath) allVertices
        numOfFilteredVertices = length filteredVertices
        difference = numOfVertices - numOfFilteredVertices

    unless (difference == 0) $
      logMsg logger $
        "Ignoring " <> showt difference <>
        " vertices due to missing src files: \n" <>
          Text.unlines [ " - " <> Text.pack m | m <- Set.toList missingFiles]

    -- We need the fileLinesSumMap to convert range to bytespan and the
    -- fileLinesLengthMap to create the `Src.FileLines` facts needed by
    -- codemarkup.
    (fileLinesSumMap, fileLineLengthMap) <-
      mkFileLinesMap logger filteredVertices

    numCapabilities <- getNumCapabilities
    let newChunkSize = max 1 (div numOfFilteredVertices numCapabilities)
        splitVertices = chunksOf newChunkSize filteredVertices

    batchOutputs <-
      mapConcurrently
        (buildXRefMapForBatch logger opts db fileLinesSumMap)
        $ zip [1 .. ] splitVertices

    when (Map.size fileLineLengthMap == 0) $
      error "File line map is empty!!"

    return (fileLineLengthMap, batchOutputs)

srcFile :: FilePath -> Vertex -> FilePath
srcFile repoPath (_, srcFp, _, _, _, _, _, _) = repoPath </> srcFp

{- | Mapping of file to the length of each of its lines.
 This is need to convert ranges in the files to byte spans, which is the
 notation used by Glean.

The first array contains in index N the number of characters up to line N.
It is used to convert Ranges to FileLocations.

The second contains the length of line N and is used to save the `FileLines`
facts needed for code browing (e.g. Shiny needs it).
 TBD: explain both maps.
-}
mkFileLinesMap ::
  Tracer Text ->
  [Vertex] ->
  IO (FileLineMap, FileLineMap)
mkFileLinesMap logger allVertices = traceMsg logger "mkFileLinesMap" $ do
  uniqSupply <- mkSplitUniqSupply 'z'
  let nc = initNameCache uniqSupply []
  bothLineMaps <-
    catMaybes <$> mapM (mkLineLengths logger nc) filepathsSet
  let fileLinesSumMap =
        Map.fromList
          $! map (\llo -> (srcFp llo, fileLinesSum llo)) bothLineMaps
      fileLinesLengthMap =
        Map.fromList $! map (\llo -> (srcFp llo, fileLines llo)) bothLineMaps

  return (fileLinesSumMap, fileLinesLengthMap)
  where
    !filepathsSet = Set.toList $ Set.fromList $
      map getFilePathsFromVertex allVertices

data LineLengthBuilderOutput = LineLengthBuilderOutput
  { srcFp :: !FilePath
  , hieFp :: !FilePath
  , fileLinesSum :: !LineLengthArray
  , fileLines :: !LineLengthArray
  }

{- | Given a vertex, read its source file and return a list with the lengths
 of each of its lines.
 This is need to convert ranges in the files to byte spans, which is the
 notation used by Glean.
-}
mkLineLengths ::
  Tracer Text ->
  NameCache ->
  (FilePath, FilePath) ->
  IO (Maybe LineLengthBuilderOutput)
mkLineLengths logger nc (srcFp, hieFp) = handle exHandler $ do
  !fileLines <- sourceFileLineLengths nc hieFp

  let mkLinesSum :: [Int] -> Int -> [Int] -> [Int]
      mkLinesSum [] _ (x : xs) = mkLinesSum [x] x xs
      mkLinesSum sums _ [] = sums
      mkLinesSum sums !lastSum (x : xs) =
        mkLinesSum (newLast : sums) newLast xs
        where
          newLast = x + lastSum

      !linesSum = reverse $ mkLinesSum [] 0 fileLines

  -- Should be 1-indexed to match HieDB line numbers
  return $
    Just $
      LineLengthBuilderOutput
        { srcFp = srcFp
        , hieFp = hieFp
        , fileLinesSum = listArray (1, length linesSum) linesSum
        , fileLines = listArray (1, length fileLines) fileLines
        }
  where
    exHandler ::
      SomeException ->
      IO (Maybe LineLengthBuilderOutput)
    exHandler = \e -> do
      logMsg logger $
        Text.unwords
          [ "Exception when getting lines from file"
          , Text.pack hieFp
          , ":"
          , Text.pack $ displayException e
          ]

      return Nothing

-- | Given the path to the hie file, get the lengths of its lines.
sourceFileLineLengths ::
  NameCache -> FilePath -> IO [Int]
sourceFileLineLengths nc hieFp = do
#if __GLASGOW_HASKELL__ >= 902
  ref <- newIORef nc
  let ncu = NCU (updNameCache ref)
  srcContent <- hie_hs_src . hie_file_result <$> readHieFile ncu hieFp
#else
  srcContent <- hie_hs_src . hie_file_result . fst <$> readHieFile nc hieFp
#endif
  let !lineLengths =
        map ((1 +) . Text.length) $ -- +1 to count the newlines
          Text.lines $ Text.decodeUtf8 srcContent

  -- Have to add the last line that's just a newline character.
  return $ lineLengths ++ [1]

-- Get the source and hie filepaths from the Vertex.
getFilePathsFromVertex :: Vertex -> (FilePath, FilePath)
getFilePathsFromVertex (_, srcFp, hieFp, _, _, _, _, _) =
  (srcFp, hieFp)

buildXRefMapForBatch ::
  Tracer Text ->
  HieDBIndexerOptions a ->
  HieDb ->
  FileLineMap ->
  (Int, [Vertex]) ->
  IO IndexerBatchOutput
buildXRefMapForBatch
  logger
  HieDBIndexerOptions{}
  db
  fileLinesSumMap
  (counter, vertices) = traceMsg logger fullTraceMsg $ do
    logMsg logger $
      prependBatchNum $
        "Number of vertices in this batch: " <> show (length vertices)

    let !nodeDefinitions = mkNodeDefinitions fileLinesSumMap vertices

        !srcAndHiePaths = nubOrd $
          map getFilePathsFromVertex vertices

    logMsg logger $
      prependBatchNum $
        "length of nodeDefinitions = " <> show (length nodeDefinitions)

    logMsg logger $
      prependBatchNum $
        "Getting xreferences for = " <> show (length srcAndHiePaths) <> " files"

    -- Xref map by hie file
    fileXRefMaps <-
      Map.unions
        <$> mapM
          (getXReferencesFromFile logger db fileLinesSumMap)
          srcAndHiePaths

    logMsg logger $
      prependBatchNum $
        "length of fileXRefMaps = " <> show (Map.size fileXRefMaps)

    return $ IndexerBatchOutput nodeDefinitions (Map.toList fileXRefMaps)
    where
      prependBatchNum :: String -> Text
      prependBatchNum = Text.pack . Text.printf "[Batch %d] %s" counter

      fullTraceMsg =
        Text.pack $
          "Creating partial FileXRefMap and for batch: " <> show counter

mkFileLocation ::
  FileLineMap ->
  FilePath ->
  Int ->
  Int ->
  Int ->
  Int ->
  Maybe FileLocation
mkFileLocation fileLinesSumMap filePath sLine sCol eLine eCol = do
  let range = Range filePath sLine sCol eLine eCol
  bSpan <- rangeToByteSpan fileLinesSumMap range

  Just $ FileLocation filePath bSpan

{- | Create a NodeDefintion from a Vertex. Use the map of file lines length
 to convert the ranges to byte spans.
-}
mkNodeDefinition :: FileLineMap -> Vertex -> Maybe NodeDefinition
mkNodeDefinition fLines (modName, srcPath, _hiePath, occNameStr, sL, sC, eL, eC) = do
  fileLoc <- mkFileLocation fLines srcPath sL sC eL eC
  let definition =
        Definition
          qualName
          name
          (Text.pack occNameStr)
          (Text.pack modName)
          fileLoc

  {-
    I think that c stands for Constructor and t for type. Which is why we get
    both for some data types (e.g. tNodePageRecommendationInfo,
    cNodePageRecommendationInfo).
  -}
  Just $ case head occNameStr of
    'v' -> Name definition
    't' -> Type definition -- Data Type. It can be a new data type or alias.
    'c' -> Constructor definition -- Constructor
    _ -> Type definition
  where
    symNameToName :: [Char] -> Text
    symNameToName = cleanUpDerivedOccName . tail

    name = symNameToName occNameStr
    qualName = Text.pack modName <> "." <> name

{- | Create Definitions from the vertices, containing necessary information
 such as fully qualified name, module name and definition location.
-}
mkNodeDefinitions :: FileLineMap -> [Vertex] -> [NodeDefinition]
mkNodeDefinitions fileLines vertices =
  mapMaybe (mkNodeDefinition fileLines) vertices

-- | Convert a range to bytespan using the length of each line in the file.
rangeToByteSpan :: FileLineMap -> Range -> Maybe ByteSpan
rangeToByteSpan fileLinesSumMap Range {..} = do
  lineLens <- Map.lookup fp fileLinesSumMap
  -- Column numbers start at 1, but bytespans are offsets and start at 0, so
  -- we have to subtract 1 from bsStart and bsEnd when using column numbers to
  -- calculate them. See D30371432 for more info.
  let startSum = if lineBegin > 1 then lineLens ! (lineBegin - 1) else 0
      bsStart =
        if (startSum + columnBegin) > 0
          then startSum + columnBegin - 1
          else 0
      endSum = if lineEnd > 1 then lineLens ! (lineEnd - 1) else 0
      bsEnd = if (endSum + columnEnd) > 0 then endSum + columnEnd - 1 else 0
      bsLength = bsEnd - bsStart

  Just $ ByteSpan bsStart bsLength

{- | If the OccName is a derived one (https://fburl.com/h9akjpz9), remove the
 prefixes to avoid having duplicate definitions.
-}
cleanUpDerivedOccName :: String -> Text
cleanUpDerivedOccName occNameStr =
  Text.pack $
    -- Logic from https://fburl.com/l3stf415.
    case occNameStr of
      -- E.g.  $wfoo
      '$' : c : name | isAlphaNum c -> name
      -- E.g.  N:blah -> newtype coercions
      c : ':' : name | isAlphaNum c -> name
      name -> name

type XrefTup = (FilePath, HashMap Text (Set ByteSpan))

getXReferencesFromFile ::
  Tracer Text ->
  HieDb ->
  FileLineMap ->
  (FilePath, FilePath) ->
  IO FileXRefMap
getXReferencesFromFile logger hiedb fileLineMap (srcFp, hieFp) = do
  catch call handler
  where
    call = getXReferencesFromFile_ logger hiedb fileLineMap (srcFp, hieFp)
    handler :: SomeException -> IO FileXRefMap
    handler e = do
      logMsg logger $
        Text.pack $
          "Exception getting xreferences for file: " <> show (srcFp, hieFp)
      logMsg logger $ Text.pack $ "Exception: " <> show e
      return Map.empty

getXReferencesFromFile_ ::
  Tracer Text ->
  HieDb ->
  FileLineMap ->
  (FilePath, FilePath) ->
  IO FileXRefMap
getXReferencesFromFile_ _logger hiedb fileLineMap (srcFp, hieFp) = do
  xrefTups <-
    -- Using foldNamed to avoid unnecessary memory usage.
    -- See the SQLite docs: https://fburl.com/uyoswvro
    foldNamed (getConn hiedb) refsQuery [":hiefp" := hieFp] [] refRowToXrefTup
  let groupedBySrcFile =
        Map.fromListWith groupBySrcFile xrefTups

  return $ Map.map convertToXrefTargets groupedBySrcFile
  where
    refsQuery :: Query
    refsQuery =
      "SELECT hiefile,occ,mod,unit,sl,sc,el,ec FROM refs WHERE hiefile = :hiefp"

    refRowToXrefTup :: [XrefTup] -> RefRow -> IO [XrefTup]
    refRowToXrefTup allTups refRow =
      return $ maybe allTups (: allTups) (mkXRefTup refRow)

    mkXRefTup :: RefRow -> Maybe (FilePath, HashMap Text (Set ByteSpan))
    mkXRefTup rr@RefRow {..} = do
      FileLocation {..} <- fileLocFromRr rr
      Just (srcFp, Map.singleton targetName $ Set.singleton locSpan)
      where
        nameString = cleanUpDerivedOccName $ occNameString refNameOcc
        qualName = Text.pack (moduleNameString refNameMod) <> "." <> nameString
        targetName = qualName

    groupBySrcFile ::
      HashMap Text (Set ByteSpan) ->
      HashMap Text (Set ByteSpan) ->
      HashMap Text (Set ByteSpan)
    groupBySrcFile currMap newXrefsForFile =
      Map.unionWith Set.union currMap newXrefsForFile
    convertToXrefTargets :: HashMap Text (Set ByteSpan) -> [XReferences]
    convertToXrefTargets = map mkXRefs . Map.toList
      where
        mkXRefs (target, bSpans) = XReferences target $ Set.toList bSpans

    fileLocFromRr :: RefRow -> Maybe FileLocation
    fileLocFromRr RefRow {..} =
      -- We get the definition name, but not the definition fact: P429413951.
      -- Now we get 2 definition facts for the same target: P429416047
      -- One is for the data type decl and the other for the constructor!
      mkFileLocation fileLineMap srcFp refSLine refSCol refELine refECol

-- TODO(T96241762): change vertex type alias and get OccName instead of Text.
getDefsVertices ::
  Tracer Text ->
  FilePath ->
  HieDb ->
  IO [Vertex]
getDefsVertices logger repoPath hiedb = traceMsg logger "getDefsVertices" $ do
  withRepoPrefix <- queryNamed (getConn hiedb) defsQuery [] :: IO [Vertex]
  let cleanUpLocalPrefix :: Vertex -> Vertex
      cleanUpLocalPrefix (mn, src, hiePath, occ, sL, sC, eL, eC) =
        (mn, newSrc, hiePath, occ, sL, sC, eL, eC)
        where
          newSrc = makeRelative repoPath src

  return $ cleanUpLocalPrefix <$> withRepoPrefix
  where
    defsQuery =
      "SELECT mods.mod,mods.hs_src,defs.* FROM defs JOIN mods ON defs.hieFile = mods.hieFile WHERE mods.hs_src <> ''"
