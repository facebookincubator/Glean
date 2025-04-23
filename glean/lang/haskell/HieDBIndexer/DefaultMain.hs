{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE TypeApplications #-}
module HieDBIndexer.DefaultMain (defaultMain, outputMain) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Extra (unlessM, when)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Foldable (toList)
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import HieDb.Run (optParser)
import HieDb (withHieFile)
import HieDb.Utils (makeNc)
import HieDb.Types (runDbM)
import Options.Applicative.Common (evalParser)
import System.Directory (copyFile, doesFileExist)
import System.IO.Extra (withTempFile)
import qualified Thrift.Protocol.Compact

import qualified Glean
import Glean.Impl.ConfigProvider ()

import HieDBIndexer.Builder (buildXrefMapFiles)
import HieDBIndexer.Glean (hieFactsBuilder, createGleanDB)
import HieDBIndexer.HieDB (mkHieDB, getHieFilesIn)
import HieDBIndexer.Index (indexHieFile)
import HieDBIndexer.Options
import HieDBIndexer.Trace (Tracer, traceMsg)

{- | Tests run concurrently, and they become flaky because the
 HieDB gets locked when running a test. This copies the DB to a temp dir
 before running a test.
-}
handleDontCreateDb ::
  HieDBIndexerOptions Sources -> (HieDBIndexerOptions Sources -> IO a) -> IO a
handleDontCreateDb opts k
  | HieDB p <- sources opts =
    withTempFile $ \fp -> do
      copyFile p fp
      k opts {sources = HieDB fp}
  | otherwise = k opts

defaultMain
  :: Glean.Backend b
  => Tracer Text
  -> HieDBIndexerOptions Sources
  -> Glean.Repo
  -> Bool
  -> b
  -> IO ()
defaultMain tracer cfg repo dontCreateDb backend =
  (if dontCreateDb then handleDontCreateDb cfg else ($ cfg)) $ \cfg' ->
  case sources cfg of
    HieDB{} -> error "TODO"
    HieFiles roots -> do
      paths <- getHieFilesIn (NonEmpty.toList roots)
      createGleanDB backend dontCreateDb repo $ \writer ->
        forM_ (HashSet.toList paths) $ \f -> do
          nc <- newIORef =<< makeNc
          runDbM nc $ do
            withHieFile f $ \h ->
              liftIO $ indexHieFile writer h

outputMain
  :: Glean.Backend b
  => Tracer Text
  -> HieDBIndexerOptions Sources
  -> FilePath
  -> Glean.SchemaId
  -> b
  -> IO ()
outputMain tracer cfg out schema_id backend = withHieDB cfg $ \hiedb -> do
  let finalCfg = cfg {sources = hiedb}
  (fileLinesMap, xrefMapData) <-
      traceMsg tracer "buildXrefMapFiles" $
        buildXrefMapFiles tracer finalCfg
  ((), batch) <-
    Glean.withBatchWriter backend schema_id Nothing def $ \writer ->
      Glean.writeFacts writer $
        hieFactsBuilder fileLinesMap xrefMapData
  BS.writeFile out (Thrift.Protocol.Compact.serializeCompact batch)


withHieDB :: HieDBIndexerOptions Sources -> (FilePath -> IO a) -> IO a
withHieDB cfg k = case sources cfg of
  HieDB p -> do
    unlessM (doesFileExist p) $
      error $ "Cannot find hiedb at: " <> p
    k p
  HieFiles paths -> withTempFile $ \hiedb -> do
    let Just opts = evalParser (optParser hiedb False)
    when (null paths) $ error "Empty set of paths for hiedb"
    mkHieDB (toList paths) opts
    k hiedb
