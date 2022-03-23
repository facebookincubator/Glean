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

module Glean.LSIF.Driver ( runIndexer, Language(..), processLSIF, writeJSON ) where

import Control.Exception ( throwIO, ErrorCall(ErrorCall) )
import System.Directory ( makeAbsolute, withCurrentDirectory )
import System.FilePath
    ( addExtension, (</>), dropExtension, takeBaseName )
import System.IO.Temp ( withSystemTempDirectory )
import System.Process ( callProcess )
import Text.Printf ( printf )

import Data.Aeson.Encoding (encodingToLazyByteString)
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
  | TypeScript

lsifIndexer :: Language -> String
lsifIndexer lang = case lang of
  Go -> "lsif-go"
  TypeScript -> "lsif-tsc"

lsifArgs :: Language -> FilePath -> [String]
lsifArgs TypeScript outFile = [ "-p", ".", "--out", outFile ]
lsifArgs Go outFile = [ "-o", outFile ]

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
  callProcess lsifBinary $ lsifArgs lang outputFile
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
        logInfo $ printf "Parsed to LSIF ok. Found %d facts" (V.length facts)
        return $ LSIF.toAngle lsif

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
