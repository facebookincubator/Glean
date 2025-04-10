{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, TypeApplications #-}

module Glean.Indexer.Angle
  (main
  ) where


import Data.Default
import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Control.Exception
import Options.Applicative
import System.FilePath ( (</>))
import Util.EventBase
import Util.IO (listDirectoryRecursive)

import Glean
import Glean.Database.Config
  (ProcessedSchema (..), processSchema, catSchemaFiles)
import Glean.LocalOrRemote
import Glean.Util.ConfigProvider
import Glean.Impl.ConfigProvider
import Glean.Schema.Builtin.Types (schema_id)
import Glean.Indexer.Utils
import Glean.Angle.Types
import Glean.Schema.Types
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Src as Src (allPredicates)


data Options = Options
  { optDir ::  String
  , optRepoPath :: String
  , optDb :: Glean.Repo
  , optService :: Service
  }

-- Combining all info needed to build facts for an angle schema
-- such that we can easily iterate them on a "per schema" basis.
data AngleSchema =
  AngleSchema SourceSchema ResolvedSchemaRef FilePath ToByteSpan

opts :: ParserInfo Options
opts = info (helper <*> parser) fullDesc
  where
    parser :: Parser Options
    parser = do
      optDir <- strOption
        (long "dir"
        <> metavar "DIR"
        <> help "dir of schemas to index. Relative to the repo-path")
      optDb <- option (maybeReader Glean.parseRepo)
        (  long "db"
        <> metavar "NAME"
        <> help "database to write facts to" )
      optRepoPath <- strOption
        (long "repo-path"
        <> metavar "REPO-PATH"
        <> help ("path to repo root."
          <> "indexed files will have names relative this root"
          )
        )
      optService <- options
      return Options{..}

buildFacts :: ProcessedSchema -> [SourceFileInfo] -> FactBuilder
buildFacts ProcessedSchema{..} fileinfos = do
  schemaFileInfo <- mapM buildFileLines fileinfos
  let schemaFileInfoMap = toFileInfoMap schemaFileInfo
      resolvedSchemaMap = toResolvedSchemaMap procSchemaResolved

  schemas <- mapM (\sSchema -> do
    let name = sourceRefName $ schemaName sSchema
    rSchema <- case HashMap.lookup name resolvedSchemaMap of
      Just rSchema -> return rSchema
      Nothing -> fail $ "Couldn't find resolved schema" <> show name
    (file, toBs) <- case HashMap.lookup name schemaFileInfoMap of
      Just SchemaFileInfo{..} -> return (filepath sourceFileInfo, toByteSpan)
      Nothing -> fail $ "Couldn't find file info for schema " <> show name
    return $ AngleSchema sSchema rSchema file toBs
    ) $ srcSchemas procSchemaSource

  mapM_ buildSchemaFacts schemas

buildSchemaFacts :: AngleSchema -> FactBuilder
buildSchemaFacts _schema = do
  -- TODO: not implemented yet
  return ()

readSchemas :: FilePath -> IO ProcessedSchema
readSchemas dir = do
  files <- listDirectoryRecursive dir
  bytestr <- catSchemaFiles files
  case processSchema Nothing bytestr of
    Left err -> throwIO $ ErrorCall err
    Right schema -> return schema

buildFileLines ::forall m. NewFact m => SourceFileInfo -> m SchemaFileInfo
buildFileLines fileInfo = do
  let SourceFileInfo _name filepath bytestr = fileInfo
      byteLines = BC.lines bytestr
      byteLinesWithNewLine = map (<> "\n") byteLines
      byteFileLines = map (Glean.Nat . fromIntegral . BC.length)
        byteLinesWithNewLine

  file <- makeFact @Src.File (Text.pack filepath)
  let fileLinesKey = Src.FileLines_key
          { fileLines_key_file = file
          , fileLines_key_lengths = byteFileLines
          , fileLines_key_endsInNewline = True
          , fileLines_key_hasUnicodeOrTabs = False
          }
  makeFact_ @Src.FileLines fileLinesKey

  -- When writing the later facts, we need:
  --   filepath
  --   A function: SrcSpan -> ByteSpan.
  let toByteSpan = fromSrcSpan fileLinesKey bytestr
  return $ SchemaFileInfo fileInfo toByteSpan

send :: Backend be => be -> ProcessedSchema -> Options -> IO ()
send be schemas opts = do
  -- pre-processing before building facts
  fileInfos <- sourceFileInfos (optRepoPath opts) (optDir opts)

  withSender be (optDb opts) refs def $ \sender ->
    withWriter sender def $ \writer ->
      writeFacts writer (buildFacts schemas fileInfos)

    where
      -- TODO: add predicates sent, e.g., [ Anglelang.allPredicates, Src., ..]
      refs = [Src.allPredicates]

main :: IO ()
main = do
  withConfigOptions opts $ \(opts, cfgOpts) -> do
    let service = optService opts
        indexDir = optRepoPath opts </> optDir opts
    schemas <- readSchemas indexDir
    withEventBaseDataplane $ \evb ->
      withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
      withBackendWithDefaultOptions evb cfgAPI service (Just schema_id)
        $ \backend -> do
          send backend schemas opts
