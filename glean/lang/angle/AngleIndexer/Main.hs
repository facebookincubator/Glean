{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module AngleIndexer.Main where

import Data.Default
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
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
import Glean.Schema.Builtin.Types ( schema_id)
import AngleIndexer.Utils
import qualified Glean.Schema.Src as Src (allPredicates)
import qualified Glean.Schema.Anglelang as Anglelang (allPredicates)
import AngleIndexer.Builder (buildFacts)

data Command = CmdIndex Options | CmdSchemaId
data Options = Options
  { optDir ::  String
  , optRepoPath :: String
  , optDb :: Glean.Repo
  , optService :: Service
  }

opts :: ParserInfo Command
opts = info (helper <*> parser) fullDesc
  where
    parser :: Parser Command
    parser = subparser
      ( command "index"
        ( info (CmdIndex <$> idxParser)
        ( progDesc "Run angle indexer for all files in the given directory" ))
      <> command "schema-id"
        ( info (pure CmdSchemaId)
        ( progDesc "Get schema id. Returns scehma id and exits" )))

    idxParser :: Parser Options
    idxParser = do
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

readSchemas :: FilePath -> IO ProcessedSchema
readSchemas dir = do
  files <- listDirectoryRecursive dir
  bytestr <- catSchemaFiles files
  case processSchema Nothing bytestr of
    Left err -> throwIO $ ErrorCall err
    Right schema -> return schema

send :: Backend be => be -> ProcessedSchema -> Options -> IO ()
send be schemas opts = do
  -- pre-processing before building facts
  fileInfos <- sourceFileInfos (optRepoPath opts) (optDir opts)

  withSender be (optDb opts) refs def $ \sender ->
    withWriter sender def $ \writer ->
      writeFacts writer (buildFacts schemas fileInfos)

    where
      refs = [Src.allPredicates, Anglelang.allPredicates]

main :: IO ()
main = do
  withConfigOptions opts $ \(cmd, cfgOpts) -> do
    case cmd  of
      CmdSchemaId -> putStrLn $ LBS.unpack $ Aeson.encodePretty $
              Aeson.object ["schema_id" Aeson..= Aeson.toJSON schema_id]
      CmdIndex opts -> do
        let service = optService opts
            indexDir = optRepoPath opts </> optDir opts
        schemas <- readSchemas indexDir
        withEventBaseDataplane $ \evb ->
          withConfigProvider cfgOpts $ \(cfgAPI :: ConfigAPI) ->
          withBackendWithDefaultOptions evb cfgAPI service (Just schema_id)
            $ \backend -> do
              send backend schemas opts
