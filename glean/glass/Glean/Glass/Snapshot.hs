{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-
 glass-snapshot F1 ... Fn --o OUTPUT

 pre-computes the DocumentSymbolListX queries for files passed as input,
 store the results in OUTPUT as a serialized thrift value Snapshot
-}

module Glean.Glass.Snapshot
  ( main ) where

import Text.Printf
import Glean.Glass.Main ( withEnv )
import Glean.Init ( withOptions )
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Options as Glass
import qualified Glean.Glass.Handler as Handler
import qualified Glean.Glass.Types as Types
import qualified Glean.Snapshot.Types as Types
import Options.Applicative
    ( fullDesc,
      help,
      info,
      long,
      short,
      metavar,
      progDesc,
      strOption,
      strArgument,
      helper,
      Parser,
      ParserInfo,
      some )
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath.Posix (splitDirectories, joinPath)
import Control.Monad ( forM )
import Data.Proxy ( Proxy(..) )
import Thrift.Protocol.Compact (Compact)
import Thrift.Protocol ( serializeGen )
import Data.ByteString as BS ( writeFile )

type FileToSnapshot = (Types.RepoName, Types.Path)

splitPath :: Text -> FileToSnapshot
splitPath path = (Types.RepoName $ Text.pack first,
                  Types.Path $ Text.pack $ joinPath rest)
  where
    first : rest = splitDirectories $ Text.unpack path

fileToSnapshot :: Parser FileToSnapshot
fileToSnapshot =  splitPath <$> strArgument (metavar "FILE [FILE]..."
  <> help "input")

filesToSnapshot :: Parser [FileToSnapshot]
filesToSnapshot = some fileToSnapshot

outputString :: Parser FilePath
outputString = strOption
  (  long "output"
  <> short 'o'
  <> metavar "FILE"
  <> help "serialized snapshot"
  )

data Config = Config
  { glassConfig :: Glass.Config -- TODO we don't need all these options
  , files :: [FileToSnapshot]
  , output :: FilePath
  }

configParser :: Parser Config
configParser =
  Config <$> Glass.configParser <*> filesToSnapshot <*> outputString

options :: ParserInfo Config
options = info (helper <*> configParser) (fullDesc <>
    progDesc ("pre-computes the DocumentSymbolListX queries for input files, "
    <> "generates a serialized thrift value Snapshot"))

main :: IO ()
main =
  withOptions options $ \_config@Config{..} ->
  withEnv
    (Glass.serviceName glassConfig)
    (Glass.gleanService glassConfig)
    (Glass.snapshotTier glassConfig)
    (Glass.configKey glassConfig)
    (Glass.refreshFreq glassConfig) $ \env@Glass.Env{..} ->
  do
    let opts = Types.RequestOptions Nothing Nothing Nothing
    snapShotItems <- forM files $ \(repo, path) -> do
      -- TODO move this loop to the Haxl monad for automatic parallelism
      let req = Types.DocumentSymbolsRequest repo path Nothing True
      symbolList <- Handler.documentSymbolListX env req opts
      let defs = length $ Types.documentSymbolListXResult_definitions symbolList
      let refs = length $ Types.documentSymbolListXResult_references symbolList
      let rev = Types.documentSymbolListXResult_revision symbolList
      printf "%s %s %s: %d defs %d refs\n" (Types.unRepoName repo)
        (Types.unPath path) (Types.unRevision rev) defs refs
      return $ Types.DocumentSymbolListXQuery req opts symbolList
    let snapShots = Types.Snapshot snapShotItems
    BS.writeFile output $ serializeGen (Proxy :: Proxy Compact) snapShots
