{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Erlang ( indexer ) where

import Data.Maybe (fromMaybe)
import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External

data Erlang = Erlang
  { elpBinary :: FilePath
  , erlBinary :: FilePath
  , escriptBinary :: FilePath
  , elpProjectPath :: Maybe FilePath
  }

options :: Parser Erlang
options = do
  elpBinary <- strOption $
    long "elp" <>
    value "elp" <>
    help "path to the elp binary (default: 'elp' from PATH)"
  erlBinary <- strOption $
    long "erl" <>
    value "erl" <>
    help "path to the erl binary (default: 'erl' from PATH)"
  escriptBinary <- strOption $
    long "escript" <>
    value "escript" <>
    help "path to the escript binary (default: 'escript' from PATH)"
  elpProjectPath <- optional (strOption $
    long "project" <>
    help "path to the Erlang project root (optional, defaults to indexerRoot)")
  return Erlang{..}

-- | Erlang indexer using ELP (Erlang Language Platform)
-- ELP generates Glean facts directly in JSON format
--
-- Usage:
--   cli:    glean index erlang --elp /path/to/elp --repo name/hash
--   shell:  :index erlang --elp /path/to/elp
--
indexer :: Indexer Erlang
indexer = Indexer {
  indexerShortName = "erlang",
  indexerDescription = "Index Erlang code using ELP",
  indexerOptParser = options,
  indexerRun = \Erlang{..} -> do
    let ext = Ext {
          extRunScript = elpBinary,
          extFlavour = Json,
          extArgs =
            [ "glean"
            , "--v2"
            , "--multi"
            , "--project"
            , fromMaybe "${TEST_ROOT}" elpProjectPath
            , "--erl"
            , erlBinary
            , "--escript"
            , escriptBinary
            , "--to"
            , "${JSON_BATCH_DIR}"
            ],
          extDerivePredicates =
            [ "erlang.DeclarationUses"
            ],
          extAllowNonZeroExit = False
        }
    indexerRun externalIndexer ext
  }
