{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{- |
 glass-snapshot F1 ... Fn --o OUTPUT

 pre-computes the DocumentSymbolListX queries for files passed as input,
 store the results in OUTPUT as a serialized thrift value Snapshot
-}

{-# LANGUAGE NamedFieldPuns #-}
module Glean.Glass.Snapshot
  ( main ) where

import Control.Concurrent.Stream (stream)
import Control.Concurrent (getNumCapabilities)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Int (Int64)
import Data.IORef.Extra
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text, pack, unpack)
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.Text.Encoding as TE
import qualified Database.MySQL.Simple as DB
import Options.Applicative (
  Parser,
  ParserInfo,
  auto,
  fullDesc,
  help,
  helper,
  info,
  long,
  maybeReader,
  metavar,
  option,
  optional,
  progDesc,
  short,
  showDefault,
  some,
  strArgument,
  strOption,
  value,
  switch,
 )
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.FilePath ((</>), joinPath, splitDirectories, takeDirectory)
import Thrift.Protocol (serializeGen)
import Thrift.Protocol.Compact (Compact)
import Text.Printf

import Facebook.Db (InstanceRequirement (Master), withConnection)
import Util.Log.String (logError, logInfo, logWarning)
import Codec.Compression.GZip (compress)

import qualified Glean
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Handler as Handler
import Glean.Glass.Main (withEnv)
import qualified Glean.Glass.Options as Glass
import qualified Glean.Glass.Types as Types
import Glean.Init (withOptions)

import qualified Glean.Snapshot.Types as Types

type FileToSnapshot = (Types.RepoName, Types.Path)

splitPath :: Text -> FileToSnapshot
splitPath path = (Types.RepoName $ pack first,
                  Types.Path $ pack $ joinPath rest)
  where
    first : rest = splitDirectories $ unpack path

fileToSnapshot :: Parser FileToSnapshot
fileToSnapshot =  splitPath <$> strArgument (metavar "FILE [FILE]..."
  <> help "input")

filesToSnapshot :: Parser [FileToSnapshot]
filesToSnapshot = some fileToSnapshot

optRev :: Parser (Maybe Types.Revision)
optRev = optional $ Types.Revision <$> strOption
        (long "revision"
        <> short 'r'
        <> metavar "REVISION"
        <> help "An optional revision to be used in the snapshot" )

thresholdParser :: Parser (Maybe Int)
thresholdParser = optional $ option auto (
        long "threshold"
        <> short 't'
        <> metavar "THRESHOLD"
        <> help "Snapshot bigger than THRESHOLD KB aren't uploaded")

outputString :: Parser (Maybe FilePath)
outputString = optional $ strOption
  (  long "output"
  <> short 'o'
  <> metavar "FOLDER"
  <> help "Don't upload snapshot, save to file instead"
  )

compressParser :: Parser Bool
compressParser = switch
  (  long "no-compression"
  <> help "Don't compress snapshots"
  )


newtype SnapshotTier = SnapshotTier Text

snapshotTierParser :: Parser SnapshotTier
snapshotTierParser = SnapshotTier <$> (option auto (mconcat
  [ long "snapshot-tier"
  , help "snapshot tier"
  , value "xdb.glass_snapshot_dev"
  , showDefault
  ]) :: Parser Text)

gleanDBNameParser :: Parser (Maybe Glean.Repo)
gleanDBNameParser =
  optional $ option (maybeReader Glean.parseRepo)
    (  long "db"
    <> metavar "NAME/INSTANCE"
    <> help "identifies the database"
    )

data Config = Config
  { glassConfig :: Glass.Config -- TODO we don't need all these options
  , files :: [FileToSnapshot]
  , output :: Maybe FilePath
  , rev :: Maybe Types.Revision
  , tier :: SnapshotTier
  , threshold :: Maybe Int
  , gleanDBName :: Maybe Glean.Repo
  , doCompress :: Bool
  }

configParser :: Parser Config
configParser =
  Config <$> Glass.configParser <*> filesToSnapshot <*> outputString <*>
  optRev <*> snapshotTierParser <*> thresholdParser <*> gleanDBNameParser <*>
  (not <$> compressParser)

options :: ParserInfo Config
options = info (helper <*> configParser) (fullDesc <>
    progDesc ("pre-computes the DocumentSymbolListX results for input files, "
    <> "and upload to XDB tier"))

data BuildSnapshot = BuildSnapshot
  {
    bytes :: !BS.ByteString,
    revision :: Types.Revision,
    sizeKB :: !Int,
    symbolList :: Types.DocumentSymbolListXResult
  }

isEmptySymbols :: Types.DocumentSymbolListXResult -> Bool
isEmptySymbols Types.DocumentSymbolListXResult{..} =
    null documentSymbolListXResult_definitions &&
    null documentSymbolListXResult_definitions

-- | Throws GlassException
buildSnapshot
  :: Glass.Env
  -> Maybe Types.Revision
  -> FileToSnapshot
  -> Bool
  -> IO BuildSnapshot
buildSnapshot env rev (repo, path) doCompress = do
    let opts = def{Types.requestOptions_strict = True}
    let req = Types.DocumentSymbolsRequest repo path Nothing True
    symList <- Handler.documentSymbolListX env req opts
    let symbolList = case rev of
          Nothing -> symList
          Just r -> symList { Types.documentSymbolListXResult_revision = r }
    let defs = length $ Types.documentSymbolListXResult_definitions symbolList
    let refs = length $ Types.documentSymbolListXResult_references symbolList
    let revision = Types.documentSymbolListXResult_revision symbolList
    let snapshots = Types.Snapshot
          [Types.DocumentSymbolListXQuery req opts symbolList]
    let transform = if doCompress then toStrict . compress . fromStrict else id
    let ser = transform $ serializeGen (Proxy :: Proxy Compact) snapshots
    let snapshotSizeKB = BS.length ser `div` 1000
    let logCompress :: String = if doCompress then "(compressed)" else ""
    logInfo $ printf "Building snapshot %s %s: %d defs %d refs %dKB %s"
      (Types.unPath path) (Types.unRevision revision) defs refs
      snapshotSizeKB logCompress
    return $! BuildSnapshot ser revision snapshotSizeKB symbolList

uploadToXdb
  :: SnapshotTier
  -> Types.RepoName
  -> Types.Revision
  -> Types.Path
  -> BuildSnapshot
  -> IO ()
uploadToXdb
  (SnapshotTier xdbTier)
  (Types.RepoName repo)
  (Types.Revision rev)
  (Types.Path path)
  BuildSnapshot{bytes, sizeKB, symbolList}
    | isEmptySymbols symbolList =
      logWarning $ printf "%s: Refusing to upload empty snapshot" path
    | otherwise = do
    let num_rows :: IO Int64 = withConnection (TE.encodeUtf8 xdbTier) Master $
          \conn -> do
          DB.execute conn
            "INSERT IGNORE INTO snapshot (repo, revision, file, snapshot)\
            \VALUES (?, ?, ?, ?)"
            (repo, rev, path, bytes)
    res <- try num_rows
    case res of
      Right 1 -> do
        logInfo $ printf "%s: successfully uploaded %d KB\n" path sizeKB
      Right 0 ->
        logError $ printf "%s: already present - skipping" path
      Right _ ->
        logError $ printf "%s: Couldn't upload" path
      Left (exc::SomeException) -> do
         logError $ printf "%s: %s" path (show exc)

main :: IO ()
main =
  withOptions options $ \_config@Config{..} ->
  withEnv
    (Glass.serviceName glassConfig)
    (Glass.gleanService glassConfig)
    (Glass.snapshotTier glassConfig)
    (Glass.configKey glassConfig)
    (Glass.refreshFreq glassConfig)
    gleanDBName $ \env@Glass.Env{..} -> do
      numCores <- getNumCapabilities
      errorsCounterRef <- newIORef 0
      stream numCores (forM_ files) $ \file@(repo, path@(Types.Path p)) -> do
        snapOrError <- try $ buildSnapshot env rev file doCompress
        case snapOrError of
          Left Types.GlassException{..} -> do
            atomicModifyIORef'_ errorsCounterRef succ
            logError $ printf "%s: %s" p (show $ head glassException_reasons)
          Right snap@BuildSnapshot{bytes, revision, sizeKB}
            | Nothing <- output
            , sizeKB < fromMaybe maxBound threshold
            -> uploadToXdb tier repo revision path snap
            | Nothing <- output
            -> logError $
                printf
                  "%s: Snapshot size above threshold (%d kB), not uploading"
                  p
                  sizeKB
            | Just output_ <- output
            -> do
              let out = output_ </> unpack p
              createDirectoryIfMissing True (takeDirectory out)
              BS.writeFile out bytes

      -- Exit with error code = number of failed snapshots
      errorsCount <- readIORef errorsCounterRef
      when (errorsCount > 0) $
        exitWith (ExitFailure errorsCount)
