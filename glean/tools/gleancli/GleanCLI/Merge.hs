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

import Glean.LocalOrRemote (loadDbSchema)
import qualified Glean.LocalOrRemote as Glean
import Glean.Types
import Glean.RTS.Types
import Glean.RTS.Foreign.FactSet (FactSet)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Define (DefineFlags(..), defineBatch)
import qualified Glean.RTS.Foreign.Inventory as Inventory
import Glean.RTS.Foreign.Ownership
import qualified Glean.Database.Config as GleanDB
import Glean.Database.Schema.Types
import qualified Glean.Util.ThriftSource as ThriftSource
import Glean.Database.Config (cfgServerConfig)
import Glean.Database.Schema

import GleanCLI.Types
import GleanCLI.Common (dbOpts, fileFormatOpt, FileFormat (..))
import Glean.Write (fileToBatches, schemaIdToOpts)
import Glean.Write.JSON (buildJsonBatch)


data MergeCommand = MergeCommand
  { mergeFiles :: [FilePath]
  , mergeFileSize :: Int
  , mergeOutDir :: FilePath
  , fileFormat :: FileFormat
  , inventorySource :: Either Repo FilePath
  }

inventoryOpt :: Parser FilePath
inventoryOpt = strOption $
    long "inventory" <>
    metavar "FILE" <>
    help ("Inventory created with --write-serialized-inventory and which "
      <> "was used to create binary format files of facts")

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
    inventorySource <- Left <$> dbOpts <|> Right <$> inventoryOpt
    fileFormat <- fileFormatOpt BinaryFormat
    return MergeCommand{..}

  withService evb cfgAPI svc MergeCommand{..} = do
    (inventory, dbSchema) <- case inventorySource of
      Left repo -> do
        dbSchema <- Glean.withBackendWithDefaultOptions
          evb cfgAPI svc Nothing $ \backend -> do
            loadDbSchema backend repo
        logInfo("db's schema ID is: "  <> show(schemaId dbSchema))
        return (schemaInventory dbSchema, Just dbSchema)
      Right mergeInventory -> do
        inventory <- Inventory.deserialize <$> B.readFile mergeInventory
        -- Get the schema_id from the JSON's schema ID field
        --- this needs to be done when the file is considered later on
        -- dbSchema <-
        return (inventory, Nothing)
    createDirectoryIfMissing True mergeOutDir
    hSetBuffering stderr LineBuffering
    outputs <- newIORef []
    expandedMergeFiles <- mapM expandFile mergeFiles
    stream 1 (merge fileFormat inventory dbSchema $ concat expandedMergeFiles)
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

      merge fileFormat inventory dbSchema files write = loop 0 0 Nothing files
        where
        read :: FilePath -> Int -> FactSet -> IO FactOwnership
        read file size factSet = do
          logInfo $ "Reading " <> file <> " (" <> show size <> " bytes)"
          -- Merge can take an existing db or an inventory.
          -- A DB has a dbSchema, so we can use that to build the batches
          -- to merge
          -- An inventory doesn't have a dbSchema, so leads to "Nothing"
          -- This means that we need to get the schema from somewhere
          --- this will be the first JSON file we see
          -- read the 'schema_id' field in that file and create a
          -- new dbSchema intance with that using configerator's
          -- stored default schema index

          batch <- case fileFormat of
            JsonFormat -> do
              (batches, schema_id_file) <- fileToBatches file
              case dbSchema of
                Nothing -> do
                  schema_id <- case schema_id_file of
                    Nothing -> throwIO $ ErrorCall "missing schema ID"
                    Just id -> return id
                  let cfg = case svc of
                        Glean.Local cfg _ -> cfg
                        Glean.Remote{} -> def
                  serverConfig <- ThriftSource.load cfgAPI (cfgServerConfig cfg)
                  loc <- GleanDB.schemaLocation cfg serverConfig
                  let (schemaSource, _) = GleanDB.cfgSchemaHook cfg loc
                  index <- ThriftSource.load cfgAPI schemaSource
                  dbSchema <- newDbSchema Nothing index
                    (SpecificSchemaId schema_id) readWriteContent def
                  buildJsonBatch dbSchema
                    (schemaIdToOpts $ Just (schemaId dbSchema)) batches
                Just schema -> do
                  if Just(schemaId schema) == schema_id_file then
                    logInfo(
                      "Schema matches with db schema. Merging data from "
                      <>  file
                      )
                  else
                    throwIO $ ErrorCall $
                        "ERROR - ABORTING MERGE\nSchema ID mismatch:\ndb: "
                        <> show (schemaId schema) <> "\nvs\nFile: "
                        <> file <> " has " <> show schema_id_file
                  let getSchemaId theschema = Just(schemaId theschema) in
                    buildJsonBatch schema
                        (schemaIdToOpts $ getSchemaId schema) batches

            BinaryFormat -> do
              bytes <- B.readFile file
              case deserializeCompact bytes of
                Left err -> throwIO $ ErrorCall $
                  "failed to deserialize " <> file <> ": " <> err
                Right batch -> return batch
          subst <- defineBatch factSet inventory batch
            DefineFlags {
              trustRefs = True,
              ignoreRedef = True }
          substOwnership subst $ FactOwnership (batch_owned batch)

        loop !_ _ Nothing [] = return ()
        loop !n _ (Just set) [] = write (n, Right set)
        loop !n currentSize acc (f : files) = do
          size <- fromIntegral <$> getFileSize f
          if size > mergeFileSize && fileFormat == BinaryFormat
            -- just copy huge binary files
            then do
              write (n, Left f)
              loop (n+1) currentSize acc files
            else do
              (factSet, ownership) <- case acc of
                Nothing -> (,[]) <$> FactSet.new lowestFid
                Just facts -> return facts
              owners <- read f size factSet
              newSize <- factSetSize factSet
              let facts = (factSet, owners : ownership)
              (n, acc) <- if newSize > mergeFileSize
                then do write (n, Right facts); return (n+1, Nothing)
                else return (n, Just facts)
              loop n newSize acc files
