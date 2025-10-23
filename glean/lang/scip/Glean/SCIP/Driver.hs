{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Generic indexer for SCIP, "semantic code intelligence protocol":
a typed lsif-like indexer format from SourceGraph.

See https://github.com/sourcegraph/scip

-}

module Glean.SCIP.Driver (

    ScipIndexerParams(..),
    runIndexer,
    processSCIP,
  ) where

import Data.Maybe ( fromMaybe )
import Control.Monad
import System.Directory
import System.FilePath ( (</>), takeBaseName )
import System.IO.Temp ( withSystemTempDirectory )
import System.Process ( callProcess )
import Text.Printf ( printf )
import Util.Log ( logInfo )
import qualified Data.Aeson as Aeson

data ScipIndexerParams = ScipIndexerParams
  { scipBinary :: FilePath
  , scipArgs :: FilePath -> [String]
  , scipOutDir :: Maybe FilePath
  , scipRoot :: FilePath
  , scipWritesLocal :: Bool
     -- ^ e.g. rust-analyzer always writes index.scip to repoDir
  , scipLanguage :: Maybe String -- ^ a default language if known
  , scipToGlean :: FilePath
  }

-- | Run a generic SCIP-producing indexer, and convert to a Glean's scip.angle
-- database returning a single JSON value that can be sent to the Glean server
runIndexer :: ScipIndexerParams -> IO Aeson.Value
runIndexer params@ScipIndexerParams{..} = do
  repoDir <- makeAbsolute scipRoot
  withDirOrTmp scipOutDir $ \scipDir -> do
    let scipFile = scipDir </> "index.scip"
    runSCIPIndexer params { scipRoot = repoDir } scipFile
    when scipWritesLocal $ do
        copyFile (repoDir </> "index.scip") scipFile
        removeFile (repoDir </> "index.scip")
    processSCIP
      scipLanguage scipToGlean False Nothing Nothing scipFile scipDir

withDirOrTmp :: Maybe FilePath -> (FilePath -> IO a) -> IO a
withDirOrTmp Nothing f = withSystemTempDirectory "glean-scip" f
withDirOrTmp (Just dir) f = withCurrentDirectory dir $ f dir

-- | Run a SCIP indexer on a repository, put scip dump output into outputFile
runSCIPIndexer :: ScipIndexerParams -> FilePath -> IO ()
runSCIPIndexer ScipIndexerParams{..} outputFile =
  withCurrentDirectory scipRoot $ do
    logInfo $ printf "Indexing %s with %s" (takeBaseName scipRoot) scipBinary
    let args = scipArgs outputFile
    logInfo $ printf "Running command: %s %s" scipBinary (unwords args)
    callProcess scipBinary args

-- | Convert an scip protobufs encoded file into Glean lsif.angle JSON object
processSCIP
  :: Maybe String
  -> FilePath
  -> Bool
  -> Maybe FilePath
  -> Maybe FilePath
  -> FilePath
  -> FilePath
  -> IO Aeson.Value
processSCIP
    mlang
    scipToGlean
    inferLanguage
    mPathPrefix
    mStripPrefix
    scipFile
    outDir = do
  let jsonFile = outDir </> "index.json"
  logInfo $ "Using SCIP from " <> scipFile
  let langArgs = case mlang of
        Nothing -> []
        Just lang -> ["--language", lang]
  let inferLanguageArgs = (["--infer-language" | inferLanguage])
  let rootPrefixArgs = case mPathPrefix of
        Nothing -> []
        Just path -> ["--root-prefix", path]
  let stripPrefixArgs = case mStripPrefix of
        Nothing -> []
        Just path -> ["--strip-prefix", path]
  callProcess scipToGlean
    (["--input", scipFile, "--output", jsonFile]
      <> langArgs
      <> inferLanguageArgs
      <> rootPrefixArgs
      <> stripPrefixArgs)
  mJson <- Aeson.decodeFileStrict jsonFile
  return $ fromMaybe (error "scip-to-glean indexer did not produce JSON") mJson
