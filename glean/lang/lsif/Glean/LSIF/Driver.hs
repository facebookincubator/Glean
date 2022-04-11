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
    runIndexer,
    testArgs,

    -- writing
    writeJSON
 ) where

import Control.Exception ( throwIO, ErrorCall(ErrorCall) )
import Control.Monad.State.Strict
import Data.Text ( Text )
import Data.List 
import System.Directory ( getHomeDirectory, withCurrentDirectory, makeAbsolute )
import System.FilePath ( (</>), takeBaseName )
import System.IO.Temp ( withSystemTempDirectory )
import System.Process ( callProcess, callCommand )
import Text.Printf ( printf )
import Util.Log ( logInfo )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import qualified Foreign.CPP.Dynamic

import qualified Data.LSIF.Angle as LSIF

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
-- returning a single JSON value that can be sent to the Glean server
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

-- | Convert an lsif json dump into Glean lsif.angle JSON object
processLSIF :: FilePath -> IO Aeson.Value
processLSIF lsifFile = do
  logInfo $ "Using LSIF from " <> lsifFile
  toLsifAngle =<< Lazy.readFile lsifFile

-- | Write json to file
writeJSON :: FilePath -> Aeson.Value -> IO ()
writeJSON outFile json = do
  logInfo $ "Writing Angle facts to " <> outFile
  case json of
    Aeson.Array facts -> encodeChunks outFile facts
    _ -> Aeson.encodeFile outFile json

-- Uses less memory if we do this piece-wise
encodeChunks :: FilePath -> V.Vector Aeson.Value -> IO ()
encodeChunks file vs = bracketContents file $ mapM_ writeChunk $
    intersperse (Right ",") (map Left (V.toList vs))
  where
    writeChunk (Left c) = Lazy.appendFile file (Aeson.encode c)
    writeChunk (Right s) = Lazy.appendFile file (s <> "\n")

    bracketContents file act = do
      Lazy.writeFile file "["
      _ <- act
      Lazy.appendFile file "]"

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

toLsifAngle :: Lazy.ByteString -> IO Aeson.Value
toLsifAngle str = do
  paths <- dropPrefixPaths
  (facts, env) <- parseChunks paths str
  logInfo "Generating cross-references"
  let !xrefs = evalState LSIF.emitFileFactSets  env
  let result = LSIF.generateJSON (LSIF.insertPredicateMap facts xrefs)
  return (Aeson.Array $ V.fromList result)

-- | Lazily parse lsif as one object per line. File is consumed and can be
-- dropped at end of parsing We go to some lengths to avoid retaining things,
-- just the state needed to emit xrefs at the end of
-- the analysis.
parseChunks :: [Text] -> Lazy.ByteString -> IO (LSIF.PredicateMap, LSIF.Env)
parseChunks paths str =
  let contents = map (Strict.concat . Lazy.toChunks) (Lazy.lines str)
      initState = LSIF.emptyEnv { LSIF.root = paths }
  in runStateT (runToAngle contents) initState

-- strict left fold over each chunk, producing accumulating output facts
-- and final global state of the analysis
runToAngle :: [Strict.ByteString] -> StateT LSIF.Env IO LSIF.PredicateMap
runToAngle = go HashMap.empty -- a foldlM'
  where
    go !acc [] = return acc
    go !acc (line:lines) = do
      preds <- parseAsJSON line
      go (LSIF.insertPredicateMap acc preds) lines

parseAsJSON :: Strict.ByteString -> StateT LSIF.Env IO [LSIF.Predicate]
parseAsJSON line = do
  rawjson <- liftIO $ Foreign.CPP.Dynamic.parseJSON line
  case rawjson of
    Left bad -> liftIO $ throwIO (ErrorCall (Text.unpack bad))
    Right good -> case Aeson.fromJSON good of
      Aeson.Error err -> liftIO $ throwIO (ErrorCall err)
      Aeson.Success fact -> LSIF.factToAngle fact
