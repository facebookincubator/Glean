{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module HieDBIndexer.DefaultMain (defaultMain, outputMain) where

import Control.Monad.Extra (unlessM, when)
import qualified Data.ByteString as BS
import Data.Default (def)
import Data.Foldable (toList)
import qualified Data.Map as AMap
import Data.Text (Text)
import qualified Glean
import Glean.Impl.ConfigProvider ()
import Glean.Typed.Predicate (makePredicates)
import Glean.Types (SelectSchema(SelectSchema_schema_id), SchemaId)
import Glean.Write.Async as Glean
import qualified Glean.Types as Thrift
import HieDBIndexer.Builder (buildXrefMapFiles)
import HieDBIndexer.Glean (hieFactsBuilder, createGleanDB)
import HieDBIndexer.HieDB (mkHieDB)
import HieDBIndexer.Options
import HieDBIndexer.Trace (Tracer, traceMsg)
import HieDb.Run (optParser)
import Options.Applicative.Common (evalParser)
import System.Directory (copyFile, doesFileExist)
import System.IO.Extra (withTempFile)
import qualified Thrift.Protocol.Compact

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
  (if dontCreateDb then handleDontCreateDb cfg else ($cfg)) $ \cfg' ->
  withHieDB cfg $ \hiedb -> do
    let finalCfg = cfg' {sources = hiedb}
    (fileLinesMap, xrefMapData) <-
      traceMsg tracer "buildXrefMapFiles" $
        buildXrefMapFiles tracer finalCfg
    putStrLn "Finished creating the data and saving to files."
    traceMsg tracer "createGleanDB" $
      createGleanDB
        backend
        dontCreateDb
        repo
        fileLinesMap
        xrefMapData

outputMain
  :: Glean.Backend b
  => Tracer Text
  -> HieDBIndexerOptions Sources
  -> FilePath
  -> SchemaId
  -> b
  -> IO ()
outputMain tracer cfg out schema_id backend = withHieDB cfg $ \hiedb -> do
  schemaInfo <- Glean.getSchemaInfo backend Nothing $
    Thrift.GetSchemaInfo (SelectSchema_schema_id schema_id) True
  let finalCfg = cfg {sources = hiedb}
  (fileLinesMap, xrefMapData) <-
      traceMsg tracer "buildXrefMapFiles" $
        buildXrefMapFiles tracer finalCfg
  let predicates = makePredicates
        [AMap.elems (Thrift.schemaInfo_predicateIds schemaInfo)]
        schemaInfo
  ((), batch) <-
    withBatchWriter predicates Nothing def $ \writer ->
      writeFacts writer $
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
