{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE OverloadedStrings #-}

module HieDBIndexer.DefaultMain (defaultMain) where

import Data.Text (Text)
import Glean (Backend)
import Glean.Impl.ConfigProvider ()
import HieDBIndexer.Builder (buildXrefMapFiles)
import HieDBIndexer.Glean (createGleanDB)
import HieDBIndexer.Options (
  HieDBIndexerEnv (..),
  HieDBIndexerOptions (..),
 )
import HieDBIndexer.Trace (Tracer, traceMsg)
import System.Directory (copyFile)
import System.IO.Temp (withTempFile)
import System.IO
import System.FilePath (takeDirectory)

{- | Tests run concurrently, and they become flaky because the
 HieDB gets locked when running a test. This copies the DB to a temp dir
 before running a test.
-}
handleDontCreateDb :: HieDBIndexerOptions -> (HieDBIndexerOptions -> IO a) -> IO a
handleDontCreateDb opts k
  | dontCreateDb opts = do
      withTempFile (takeDirectory $ hiedbPath opts) "hiedb_temp" $ \fp h -> do
        hClose h
        copyFile (hiedbPath opts) fp
        k opts {hiedbPath = fp}
  | otherwise = k opts

defaultMain :: Backend b => Tracer Text -> HieDBIndexerOptions -> b -> IO ()
defaultMain tracer cfg backend = handleDontCreateDb cfg $ \finalCfg -> do
  (fileLinesMap, xrefMapData) <-
    traceMsg tracer "buildXrefMapFiles" $
      buildXrefMapFiles tracer finalCfg
  putStrLn "Finished creating the data and saving to files."
  traceMsg tracer "createGleanDB" $
    createGleanDB
      (HieDBIndexerEnv backend finalCfg)
      fileLinesMap
      xrefMapData
