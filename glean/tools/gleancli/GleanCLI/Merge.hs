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
import Data.IORef
import Options.Applicative
import System.Directory
import System.FilePath
import System.IO
import System.Process

import Control.Concurrent.Stream
import Util.OptParse
import Thrift.Protocol.Compact

import Glean.RTS.Types
import Glean.RTS.Foreign.FactSet (FactSet)
import qualified Glean.RTS.Foreign.FactSet as FactSet
import Glean.RTS.Foreign.Define (DefineFlags(..), defineBatch)
import qualified Glean.RTS.Foreign.Inventory as Inventory

import GleanCLI.Types

data MergeCommand = MergeCommand
  { mergeInventory :: FilePath
  , mergeFiles :: [FilePath]
  , mergeFileSize :: Int
  , mergeOutDir :: FilePath
  }

instance Plugin MergeCommand where
  parseCommand = commandParser "merge" (progDesc "Merge fact files") $ do
    mergeFiles <- many $ strArgument (
      metavar "FILE" <>
      help "File of facts, in binary format") -- TODO: accept json
    mergeInventory <- strOption $
      long "inventory" <>
      metavar "FILE" <>
      help "Inventory created with --write-serialized-inventory"
    mergeFileSize <- option auto $
      long "max-file-size" <>
      help "Create output files with (approximately) this size" <>
      showDefault <>
        value (256 * 1024 * 1024)
    mergeOutDir <- strOption $
      long "output" <>
      metavar "DIR" <>
      help "Desination directory for the merged fact files"
    return MergeCommand{..}

  withService _evb _cfgAPI _svc MergeCommand{..} = do
    createDirectoryIfMissing True mergeOutDir
    inventory <- Inventory.deserialize <$> B.readFile mergeInventory
    hSetBuffering stderr LineBuffering
    outputs <- newIORef []
    stream 1 (merge inventory mergeFiles) (writeToFile outputs)
      -- stream overlaps writing with reading
    files <- readIORef outputs
    L.putStrLn (Aeson.encode (Aeson.toJSON files))
    where
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
        -> (Int, Either FilePath FactSet)
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
          Right factSet -> do
            batch <- serializeCompact <$> FactSet.serialize factSet
            hPutStrLn stderr $ "Writing " <> out <>
              " (" <> show (B.length batch) <> ")"
            B.writeFile out batch

      merge inventory files write = loop 0 0 Nothing files
        where
        read :: FilePath -> Int -> FactSet -> IO ()
        read file size factSet = do
          hPutStrLn stderr $ "Reading " <> file <> " (" <> show size <> ")"
          bytes <- B.readFile file
          case deserializeCompact bytes of
            Left err -> throwIO $ ErrorCall $
              "failed to deserialize " <> file <> ": " <> err
            Right batch -> do
              _subst <- defineBatch factSet inventory batch
                DefineFlags {
                  trustRefs = True,
                  ignoreRedef = True }
              return ()

        loop !_ _ Nothing [] = return ()
        loop !n _ (Just set) [] = write (n, Right set)
        loop !n currentSize acc (f : files) = do
          size <- fromIntegral <$> getFileSize f
          if size > mergeFileSize  -- just copy huge files
            then do
              write (n, Left f)
              loop (n+1) currentSize acc files
            else do
              factSet <- case acc of
                Nothing -> FactSet.new lowestFid
                Just factSet -> return factSet
              read f size factSet
              newSize <- factSetSize factSet
              (n, acc) <- if newSize > mergeFileSize
                then do write (n, Right factSet); return (n+1, Nothing)
                else return (n, Just factSet)
              loop n newSize acc files
