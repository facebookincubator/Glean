{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Merge (MergeCommand) where

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Aeson as Aeson
import Data.Default
import Data.IORef
import Options.Applicative
import System.Directory
import System.Directory.Extra (listFiles)
import System.FilePath
import System.IO
import System.Process
import Control.Concurrent.Stream
import Util.OptParse
import Util.Log ( logInfo )
import Thrift.Protocol.Compact

import qualified Glean.Database.Config as GleanDB
import Glean.Database.Schema
import Glean.Database.Schema.Types
import Glean.LocalOrRemote (loadDbSchema)
import qualified Glean.LocalOrRemote as Glean
import Glean.Types
import Glean.RTS.Types
import Glean.RTS.Foreign.FactSet (FactSet)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Define (DefineFlags(..), defineBatch)
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.RTS.Foreign.Ownership

import GleanCLI.Types
import GleanCLI.Common (dbOpts, fileFormatOpt, FileFormat (..))
import Glean.Write (fileToBatches, schemaIdToOpts)
import Glean.Write.JSON (buildJsonBatch)


data MergeCommand = MergeCommand
  { mergeFiles :: [FilePath]
  , mergeFileSize :: Int
  , mergeOutDir :: FilePath
  , fileFormat :: FileFormat
  , inventorySource :: Maybe (Either Repo FilePath)
  }

inventoryOpt :: Parser FilePath
inventoryOpt = strOption $
    long "inventory" <>
    metavar "FILE" <>
    help ("Inventory created with --write-serialized-inventory and which "
      <> "was used to create binary format files of facts")

data SchemaData
  = HaveSchema DbSchema
  | HaveInventory Inventory.Inventory
  | HaveNothing

instance Plugin MergeCommand where
  parseCommand = commandParser "merge" (progDesc "Merge fact files") $ do
    mergeFiles <- many $ strArgument (
      metavar "PATH" <>
      help ("File or directory of facts, either in json or binary format. "
       <> "For json format specify the database"))
    mergeFileSize <- option auto $
      long "max-file-size" <>
      metavar "BYTES" <>
      help "Create output files with (approximately) this size in bytes" <>
      showDefault <>
      value (256 * 1024 * 1024)
    mergeOutDir <- strOption $
      long "output" <>
      metavar "DIR" <>
      help "Destination directory for the merged fact files"
    inventorySource <- optional $ Left <$> dbOpts <|> Right <$> inventoryOpt
    fileFormat <- fileFormatOpt BinaryFormat
    return MergeCommand{..}

  withService evb cfgAPI svc MergeCommand{..} = do
    -- * --file-format=binary requires an inventory, from either --db or
    --   --inventory.
    -- * --file-format=json requires a dbSchema, from either --db or
    --   schema_id in the JSON file. It's pointless to use --inventory with
    --   --file-format=json, because the inventory isn't used.
    schemaData <- case inventorySource of
      Just (Left repo) -> do
        dbSchema <- Glean.withBackendWithDefaultOptions
          evb cfgAPI svc Nothing $ \backend -> do
            loadDbSchema backend repo
        logInfo("db's schema ID is: "  <> show (schemaId dbSchema))
        return (HaveSchema dbSchema)
      Just (Right mergeInventory) -> do
        case fileFormat of
          JsonFormat -> do
            hPutStrLn stderr $
              "Warning: --inventory is ignored with --file-format=json"
            return HaveNothing
          _ -> do
          inventory <- Inventory.deserialize <$> B.readFile mergeInventory
          return (HaveInventory inventory)
      Nothing ->
        return HaveNothing
    createDirectoryIfMissing True mergeOutDir
    hSetBuffering stderr LineBuffering
    outputs <- newIORef []
    expandedMergeFiles <- mapM expandFile mergeFiles
    stream 1
      (merge fileFormat schemaData (concat expandedMergeFiles))
      (writeToFile outputs)
      -- stream overlaps writing with reading
    files <- readIORef outputs
    L.putStrLn (Aeson.encode (Aeson.toJSON files))
    where
      expandFile :: FilePath -> IO [FilePath]
      expandFile file = do
        isDirectory <- doesDirectoryExist file
        if isDirectory
          then listFiles file
          else return [file]
      factSetSize :: FactSet -> IO Int
      factSetSize f = do
        c <- FactSet.factCount f
        m <- FactSet.factMemory f
          -- FactSet has an overhead of 40 bytes per fact.  serialized
          -- batch has an overhead of ~4 bytes per fact, using
          -- varints so it is slightly larger for larger facts, but
          -- this seems like a good enough approximation.
        return (m - (c * 36))

      -- if a merge will contain only a single file, we avoid
      -- deserialising and reserialising it, just copy-on-write the
      -- whole file.
      writeToFile
        :: IORef [FilePath]
        -> (Int, Either FilePath (FactSet, [FactOwnership]))
        -> IO ()
      writeToFile ref (n, factSet) = do
        let out = mergeOutDir </> show n <.> "bin"
        modifyIORef' ref (out:)
        case factSet of
          Left file -> do
            size <- getFileSize file
            hPutStrLn stderr $ "Copying " <> file <> " (" <> show size <>
              ") to " <> out
            callProcess "cp" ["--reflink=auto", file, out]
          Right (factSet, ownership) -> do
            batch0 <- FactSet.serialize factSet
            let batch = serializeCompact $
                  batch0 { batch_owned =
                    ownershipUnits (unionOwnership ownership) }
            logInfo $ "Writing " <> out <>
              " (" <> show (B.length batch) <> " bytes)"
            B.writeFile out batch

      merge fileFormat schemaData files write =
        loop 0 0 Nothing schemaData files
        where
        read
          :: FilePath
          -> Int
          -> FactSet
          -> SchemaData
          -> IO (FactOwnership, SchemaData)
        read file size factSet schemaData = do
          logInfo $ "Reading " <> file <> " (" <> show size <> " bytes)"

          let
            define batch inventory newSchemaData = do
              subst <- defineBatch factSet inventory batch
                DefineFlags {
                  trustRefs = True,
                  ignoreRedef = True }
              own <- substOwnership subst $ FactOwnership (batch_owned batch)
              return (own, newSchemaData)

          case fileFormat of
            JsonFormat -> do
              (batches, schema_id_file) <- fileToBatches file
              dbSchema <- case schemaData of
                HaveSchema dbSchema
                  | Just schema_id <- schema_id_file,
                    schema_id /= schemaId dbSchema ->
                    throwIO $ ErrorCall $
                      "Schema ID mismatch:\ndb: "
                      <> show (schemaId dbSchema) <> "\nvs\nFile: "
                      <> file <> " has " <> show schema_id
                  | otherwise -> return dbSchema
                _otherwise -> do
                  -- the first time (only), we load the schema we'll
                  -- be using to parse the JSON files.
                  schema_id <- case schema_id_file of
                    Nothing -> throwIO $ ErrorCall $
                      file <> ": missing schema_id"
                    Just id -> return id
                  let cfg = case svc of
                        Glean.Local cfg _ -> cfg
                        Glean.Remote{} -> def
                  index <- GleanDB.loadSchemaIndex cfg cfgAPI
                  newDbSchema Nothing index (SpecificSchemaId schema_id)
                    readWriteContent def
              batch <- buildJsonBatch dbSchema
                (schemaIdToOpts $ Just (schemaId dbSchema)) batches
              define batch (schemaInventory dbSchema) (HaveSchema dbSchema)

            BinaryFormat -> do
              inventory <- case schemaData of
                HaveSchema dbSchema -> return (schemaInventory dbSchema)
                HaveInventory inventory -> return inventory
                HaveNothing -> throwIO $ ErrorCall $
                  "--file-format=binary requires either --inventory, " <>
                  "--db, or --db-name and --db-instance"
              bytes <- B.readFile file
              case deserializeCompact bytes of
                Left err -> throwIO $ ErrorCall $
                  "failed to deserialize " <> file <> ": " <> err
                Right batch ->
                  define batch inventory (HaveInventory inventory)

        loop !_ _ Nothing _ [] = return ()
        loop !n _ (Just set) _ [] = write (n, Right set)
        loop !n currentSize acc schema (f : files) = do
          size <- fromIntegral <$> getFileSize f
          if size > mergeFileSize && fileFormat == BinaryFormat
            -- just copy huge binary files
            then do
              write (n, Left f)
              loop (n+1) currentSize acc schema files
            else do
              (factSet, ownership) <- case acc of
                Nothing -> (,[]) <$> FactSet.new lowestFid
                Just facts -> return facts
              (owners, schema) <- read f size factSet schema
              newSize <- factSetSize factSet
              let facts = (factSet, owners : ownership)
              (n, acc) <- if newSize > mergeFileSize
                then do write (n, Right facts); return (n+1, Nothing)
                else return (n, Just facts)
              loop n newSize acc schema files
