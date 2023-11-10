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

import Control.Concurrent.Stream (forConcurrently_unordered)
import Control.Concurrent (getNumCapabilities)
import Control.Exception (SomeException, try)
import Control.Monad (forM, when)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Int (Int64)
import Data.Foldable (asum)
import Data.Maybe (fromMaybe, catMaybes)
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
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode (ExitFailure))
import System.FilePath ((</>), joinPath, splitDirectories, takeDirectory)
import Thrift.Protocol (serializeGen, deserializeGen)
import Thrift.Protocol.Compact (Compact)
import Text.Printf
import TextShow

import Facebook.Db (InstanceRequirement (Master), withConnection)
import qualified Logger.GleanDiffTimeCoverage as Logger
import Util.Log.String (logError, logInfo, logWarning)
import Codec.Compression.GZip (compress, decompress)

import qualified Glean
import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Handler as Handler
import Glean.Glass.Main (withEnv)
import qualified Glean.Glass.Options as Glass
import Glean.Glass.SnapshotBackend (SnapshotBackend(NoSnapshotBackend))
import qualified Glean.Glass.Types as Types
import Glean.Init (withOptions)

import qualified Glean.Snapshot.Types as Types
import Glean.Glass.Types (GlassException(glassException_reasons))

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

phabricatorVersionParser :: Parser (Maybe Text)
phabricatorVersionParser =
  optional $ strOption (long "phabricator-version-number")

data Config = Config
  { glassConfig :: Glass.Config -- TODO we don't need all these options
  , files :: [FileToSnapshot]
  , output :: Maybe FilePath
  , rev :: Maybe Types.Revision
  , tier :: SnapshotTier
  , threshold :: Maybe Int
  , gleanDBName :: Maybe Glean.Repo
  , doCompress :: Bool
  , phabricatorVersionNumber :: Maybe Text
  }

configParser :: Parser Config
configParser =
  Config <$> Glass.configParser <*> filesToSnapshot <*> outputString <*>
  optRev <*> snapshotTierParser <*> thresholdParser <*> gleanDBNameParser <*>
  (not <$> compressParser) <*> phabricatorVersionParser

options :: ParserInfo (Either Config FilePath)
options = info (helper <*> parser) desc
  where
    parser = asum
      [
        Left <$> configParser,
        Right <$> printSnapshotParser
      ]
    printSnapshotParser = strOption (long "print-snapshot")
    desc =
      fullDesc <>
      progDesc ("pre-computes the DocumentSymbolListX results for input files, "
      <> "and upload to XDB tier")

data BuildSnapshot = BuildSnapshot
  {
    bytes :: !BS.ByteString,
    revision :: Types.Revision,
    sizeKB :: !Int,
    symbolList :: Types.DocumentSymbolListXResult,
    defs :: !Int,
    refs :: !Int
  }

isEmptySymbols :: Types.DocumentSymbolListXResult -> Bool
isEmptySymbols Types.DocumentSymbolListXResult{..} =
    null documentSymbolListXResult_definitions &&
    null documentSymbolListXResult_definitions

data SnapshotError
 = SizeAboveThreshold { kb :: Int }
 | GlassException Types.GlassException
 | EmptySymbolList
 | InsertError
 | UploadError SomeException
 | AlreadyPresent

showError :: SnapshotError -> Text
showError EmptySymbolList = "empty symbol list"
showError SizeAboveThreshold{..} = "size above threshold (" <> showt kb <> "kB)"
showError InsertError = "insertion error (XDB)"
showError (UploadError e) = "upload error (" <> showt e <> ")"
showError (GlassException g) = case glassException_reasons g of
  [] -> "glass: unknown"
  Types.GlassExceptionReason_noSrcFileFact{} : _ -> "no src.File fact"
  Types.GlassExceptionReason_noSrcFileLinesFact{} : _ -> "no src.FileLines fact"
  Types.GlassExceptionReason_notIndexedFile{} : _ -> "not indexed"
  Types.GlassExceptionReason_entitySearchFail{} : _ -> "entity search failed"
  Types.GlassExceptionReason_entityNotSupported{} : _ -> "entity not supported"
  Types.GlassExceptionReason_attributesError{} : _ -> "attributes error"
  Types.GlassExceptionReason_exactRevisionNotAvailable{} : _ ->
    "exact revision not available"
  Types.GlassExceptionReason_EMPTY : _ -> "glass: unknown"
showError AlreadyPresent = "already present"

-- | Throws GlassException
buildSnapshot
  :: Glass.Env
  -> Maybe Types.Revision
  -> FileToSnapshot
  -> Bool
  -> IO BuildSnapshot
buildSnapshot env rev (repo, path) doCompress = do
    let opts = def
          { Types.requestOptions_strict = True
          , Types.requestOptions_limit = Just maxBound
          }
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
    return $! BuildSnapshot ser revision snapshotSizeKB symbolList defs refs

uploadToXdb
  :: SnapshotTier
  -> Types.RepoName
  -> Types.Revision
  -> Types.Path
  -> BuildSnapshot
  -> IO (Maybe SnapshotError)
uploadToXdb
  (SnapshotTier xdbTier)
  (Types.RepoName repo)
  (Types.Revision rev)
  (Types.Path path)
  BuildSnapshot{bytes, sizeKB, symbolList}
    | isEmptySymbols symbolList = do
      logWarning $ printf "%s: Refusing to upload empty snapshot" path
      return $ Just EmptySymbolList
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
        return Nothing
      Right 0 -> do
        logError $ printf "%s: already present - skipping" path
        return $ Just  AlreadyPresent
      Right _ -> do
        logError $ printf "%s: Couldn't upload" path
        return $ Just InsertError
      Left (exc::SomeException) -> do
        logError $ printf "%s: %s" path (show exc)
        return $ Just $ UploadError exc

main :: IO ()
main =
  withOptions options $ \case
    Left config -> realMain config
    Right snapshotPath -> printSnapshot snapshotPath

realMain :: Config -> IO ()
realMain _config@Config{..} =
  withEnv
    (Glass.serviceName glassConfig)
    (Glass.gleanService glassConfig)
    (Glass.snapshotTier glassConfig)
    (Glass.configKey glassConfig)
    (Glass.refreshFreq glassConfig) Nothing
    gleanDBName $ \env0@Glass.Env{..} -> do
      let env = env0 { Glass.snapshotBackend = NoSnapshotBackend}
      numCores <- getNumCapabilities
      skycastleJobId <- lookupEnv "SKYCASTLE_WORKFLOW_RUN_ID"
      let logJobProperties =
            foldMap (Logger.setWorkflowId . pack) skycastleJobId <>
            foldMap Logger.setDiffVersion phabricatorVersionNumber

      snaps <- forConcurrently_unordered numCores files $ \file -> do
        let (_, Types.Path p) = file
        snapOrError <- try $ buildSnapshot env rev file doCompress
        case snapOrError of
          Left e@Types.GlassException{..} -> do
            let reason = printf "%s: %s" p (show $ head glassException_reasons)
            logError reason
            return (file, Left $ GlassException e)
          Right snap ->
            return (file, Right snap)

      mbErrors <- forM snaps $ \((repo, path), snapOrError) -> do
        result <- case snapOrError of
          Left e -> return (Left e)
          Right snap -> do
            let BuildSnapshot{bytes, revision, sizeKB} = snap
            if
                | Nothing <- output
                , sizeKB < fromMaybe maxBound threshold
                -> do
                  mbError <- uploadToXdb tier repo revision path snap
                  return (maybe (Right snap) Left mbError)
                | Nothing <- output
                -> do
                  logError $
                    printf
                      "%s: Snapshot size above threshold (%d kB), not uploading"
                      (Types.unPath path)
                      sizeKB
                  return (Left $ SizeAboveThreshold sizeKB)
                | Just output_ <- output
                -> do
                  let out = output_ </> unpack (Types.unPath path)
                  createDirectoryIfMissing True (takeDirectory out)
                  BS.writeFile out bytes
                  return (Right snap)

        Logger.runLog logger $
          logJobProperties <>
          Logger.setPath (Types.unPath path) <>
          Logger.setRepo (Types.unRepoName repo) <>
          case result of
            Left e ->
              Logger.setError (showError e)
            Right BuildSnapshot{..} ->
              Logger.setDefinitions defs <>
              Logger.setReferences refs

        return (either Just (const Nothing) result)

      -- Exit with error code = number of failed snapshots
      let errors = catMaybes mbErrors
      let errorsCount = length $ filter isFatalError errors
      when (errorsCount > 0) $
        exitWith (ExitFailure errorsCount)

isFatalError :: SnapshotError -> Bool
isFatalError = \case
  SizeAboveThreshold{} -> True
  GlassException{} -> True
  EmptySymbolList{} -> True
  InsertError{} -> True
  UploadError{} -> True
  AlreadyPresent -> False

printSnapshot :: FilePath -> IO ()
printSnapshot path = do
  bytes <- BS.readFile path
  let decomp = toStrict $ decompress $ fromStrict bytes
      deser :: Either String Types.Snapshot
      deser = deserializeGen (Proxy :: Proxy Compact) decomp
  either error print deser
