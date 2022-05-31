{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module HieDBIndexer.DefaultMain (defaultMain) where

import Data.Text (Text)
import Glean (Backend)
import Glean.Impl.ConfigProvider ()
import HieDBIndexer.Builder (buildXrefMapFiles)
import HieDBIndexer.Glean (createGleanDB)
import HieDBIndexer.HieDB (mkHieDB)
import HieDBIndexer.Options
import HieDBIndexer.Trace (Tracer, traceMsg)
import HieDb.Run (optParser)
import Options.Applicative.Common (evalParser)
import System.Directory (copyFile)
import System.IO.Extra (withTempFile)

{- | Tests run concurrently, and they become flaky because the
 HieDB gets locked when running a test. This copies the DB to a temp dir
 before running a test.
-}
handleDontCreateDb ::
  HieDBIndexerOptions Sources -> (HieDBIndexerOptions Sources -> IO a) -> IO a
handleDontCreateDb opts k
  | HieDB p <- sources opts
    , dontCreateDb opts = do
    withTempFile $ \fp -> do
      copyFile p fp
      k opts {sources = HieDB fp}
  | otherwise = k opts

defaultMain ::
  Backend b => Tracer Text -> HieDBIndexerOptions Sources -> b -> IO ()
defaultMain tracer cfg backend = handleDontCreateDb cfg $ \cfg' ->
  withHieDB cfg $ \hiedb -> do
    let finalCfg = cfg' {sources = hiedb}
    (fileLinesMap, xrefMapData) <-
      traceMsg tracer "buildXrefMapFiles" $
        buildXrefMapFiles tracer finalCfg
    putStrLn "Finished creating the data and saving to files."
    traceMsg tracer "createGleanDB" $
      createGleanDB
        (HieDBIndexerEnv backend finalCfg)
        fileLinesMap
        xrefMapData

withHieDB :: HieDBIndexerOptions Sources -> (FilePath -> IO a) -> IO a
withHieDB cfg k = case sources cfg of
  HieDB p -> k p
  HieFiles paths -> withTempFile $ \hiedb -> do
    let Just opts = evalParser (optParser hiedb False)
    mkHieDB paths opts
    k hiedb
