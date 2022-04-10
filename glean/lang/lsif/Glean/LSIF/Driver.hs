{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Generic indexer for LSIF. Supply name of lsif binary in $PATH to run
predicates for typescript.

-}

{-# LANGUAGE OverloadedStrings #-}

module Glean.LSIF.Driver (
    indexerLang,
    Language(..),
    processLSIF,
    writeJSON,
    runIndexer,
    testArgs
 ) where

import Control.Exception ( throwIO, ErrorCall(ErrorCall) )
import Data.Aeson.Encoding ( encodingToLazyByteString )
import Data.Text ( Text )
import System.Directory
    ( getHomeDirectory, withCurrentDirectory, makeAbsolute )
import System.FilePath
    ( takeBaseName, dropExtension, (</>), addExtension )
import System.IO.Temp ( withSystemTempDirectory )
import System.Process ( callProcess, callCommand )
import Text.Printf ( printf )
import Util.Log ( logInfo )
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as V

import qualified Foreign.CPP.Dynamic

import qualified Data.LSIF.Angle as LSIF
import Data.LSIF.Types (LSIF(..))

-- | Languages we can index via LSIF
data Language
  = Go
  | Rust
  | TypeScript
  deriving (Show, Eq, Bounded, Enum)

lsifIndexer :: Language -> String
lsifIndexer lang = case lang of
  Go -> "lsif-go"
  Rust -> "rust-analyzer"
  TypeScript -> "lsif-tsc"

indexerLang :: String -> Maybe Language
indexerLang "lsif-go" = Just Go
indexerLang "lsif-tsc" = Just TypeScript
indexerLang "rust-analyzer" = Just Rust
indexerLang _ = Nothing

lsifArgs :: Language -> FilePath -> Either [String] [String]
lsifArgs Go outFile = Right [ "--no-animation", "-o", outFile ]
lsifArgs TypeScript outFile = Right [ "-p", ".", "--out", outFile ]
lsifArgs Rust _outFile = Left [ "lsif", "." ] -- readProcess

-- snapshot tests for different lsif indexers all use the same frontend
testArgs :: FilePath -> Language -> [String]
testArgs dir lang =
    [ "--binary", lsifIndexer lang
    , "--lsif"
    , "--root", dir
    ]

-- | Run an LSIF indexer, and convert to a Glean's lsif.angle database
runIndexer :: Language -> FilePath -> IO Aeson.Value
runIndexer lang dir = do
  repoDir <- makeAbsolute dir -- save this before we switch to tmp
  withSystemTempDirectory "glean-lsif" $ \lsifDir -> do
    let lsifFile = lsifDir </> "index.lsif"
    runLSIFIndexer lang repoDir lsifFile
    processLSIF lsifFile

-- | Run the lsif-tsc indexer on a repository,
-- put lsif dump output into outputFile
runLSIFIndexer :: Language -> FilePath -> FilePath -> IO ()
runLSIFIndexer lang dir outputFile = withCurrentDirectory dir $ do
  logInfo $ printf "Indexing %s with %s" (takeBaseName dir) lsifBinary
  case lsifArgs lang outputFile of
    Right args -> callProcess lsifBinary args
    Left args -> callCommand $
      printf "%s %s > %s" lsifBinary (unwords args) outputFile
  where
    lsifBinary = lsifIndexer lang

-- | Convert an lsif json dump into Glean lsif.angle
processLSIF :: FilePath -> IO Aeson.Value
processLSIF lsifFile = do
  logInfo $ "Using LSIF from " <> lsifFile
  contents <- normalizeJson lsifFile
  r <- Foreign.CPP.Dynamic.parseJSON contents
  case r of
    Left err -> throwIO (ErrorCall (Text.unpack err))
    Right val -> case A.fromJSON val of
      A.Error err -> throwIO (ErrorCall err)
      A.Success lsif@(LSIF facts) -> do
        logInfo $ printf "Parsed LSIF ok. Found %d facts" (V.length facts)
        paths <- dropPrefixPaths
        return $ LSIF.toAngle paths lsif

-- Get some likely prefix paths to drop from indexers
-- E.g. typescript with a yarn install puts .config/yarn paths for libraries
dropPrefixPaths :: IO [Text]
dropPrefixPaths = do
  home <- Text.pack <$> getHomeDirectory
  return $ map ("file://" <>)
  -- typescript system paths
    [ home <> "/.config/yarn"
    , "/usr/local/share/.config/yarn"
   -- rust system paths
    , "/usr/lib"
    , home <> "/.cargo/registry"
    , home <> "/.rustup/toolchains/stable-x86_64-unknown-linux-gnu"
    , home <> "/.rustup/toolchains/stable-aarch64-unknown-linux-gnu"
    ]

-- | If we want to save to file instead
writeJSON :: FilePath -> Aeson.Value -> IO ()
writeJSON jsonFile json = do
  Lazy.writeFile jsonFile (encodingToLazyByteString $ Aeson.toEncoding json)
  logInfo $ printf "Wrote Angle/JSON to %s" jsonFile

-- lsif-tsc dump format is not valid json (each line is an object though)
-- We just turn the whole thing into a valid json array so it parses with the
-- folly parseJSON
normalizeJson :: FilePath -> IO B.ByteString
normalizeJson lsifFile = do
  raw <- Text.readFile lsifFile
  Text.writeFile outFile "["
  Text.appendFile outFile (Text.intercalate "," (Text.lines raw))
  Text.appendFile outFile "]"
  B.readFile outFile
  where
    outFile = (dropExtension lsifFile <> "-normal") `addExtension` "lsif"
