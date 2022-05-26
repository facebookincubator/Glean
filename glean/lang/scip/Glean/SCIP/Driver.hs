{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Generic indexer for SCIP, "semantic code intelligence protocol":
a typed lsif-like indexer format from SourceGraph.

See https://github.com/sourcegraph/scip

-}

module Glean.SCIP.Driver (

    ScipIndexerParams(..),
    runIndexer,
    processSCIP,

  ) where

import System.Directory ( makeAbsolute, withCurrentDirectory )
import System.FilePath ( (</>), takeBaseName )
import System.IO.Temp ( withSystemTempDirectory )
import System.Process ( callProcess )
import Text.Printf ( printf )
import Util.Log ( logInfo )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as B

import Data.SCIP.Angle ( scipToAngle )

data ScipIndexerParams = ScipIndexerParams
  { scipBinary :: FilePath
  , scipArgs :: FilePath -> [String]
  , scipRoot :: FilePath
  }

-- | Run a SCIP indexer, and convert to a Glean's lsif.angle database
-- returning a single JSON value that can be sent to the Glean server
runIndexer :: ScipIndexerParams -> IO Aeson.Value
runIndexer params@ScipIndexerParams{..} = do
  repoDir <- makeAbsolute scipRoot
  withSystemTempDirectory "glean-scip" $ \scipDir -> do
    let scipFile = scipDir </> "index.scip"
    runSCIPIndexer params { scipRoot = repoDir } scipFile
    processSCIP scipFile

-- | Convert an scip protobufs encoded file into Glean lsif.angle JSON object
processSCIP :: FilePath -> IO Aeson.Value
processSCIP scipFile = do
  logInfo $ "Using SCIP from " <> scipFile
  scipToAngle <$> B.readFile scipFile

-- | Run a SCIP indexer on a repository, put scip dump output into outputFile
runSCIPIndexer :: ScipIndexerParams -> FilePath -> IO ()
runSCIPIndexer ScipIndexerParams{..} outputFile =
  withCurrentDirectory scipRoot $ do
    logInfo $ printf "Indexing %s with %s" (takeBaseName scipRoot) scipBinary
    let args = scipArgs outputFile
    callProcess scipBinary args
