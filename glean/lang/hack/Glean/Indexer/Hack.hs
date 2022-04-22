{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Hack ( indexer, Hack(..) ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External

data Hack = Hack
  { hackBinary :: FilePath
  , hackWriteRoot :: String
  , hackOnly :: Maybe String
  }

options :: Parser Hack
options = do
  hackBinary <- strOption $
    long "hack" <>
    value "hh_server" <>
    help "path to the hh_server binary"
  hackWriteRoot <- strOption $
    long "symbol-write-root-path" <>
    value "www" <>
    help "prefix to add to source file paths in the DB"
  hackOnly <- optional $ strOption $
    long "only-index" <>
    help "comma-separated list of files to index (otherwise all)"
  return Hack{..}

indexer :: Indexer Hack
indexer = Indexer {
  indexerShortName = "hack",
  indexerDescription = "Index Hack code",
  indexerOptParser = options,
  indexerRun = \Hack{..} -> do
    let ext = Ext {
          extBinary = hackBinary,
          extFlavour = Json,
          extArgs =
            [ "${TEST_ROOT}"
            , "--write-symbol-info"
            , "${JSON_BATCH_DIR}"
            ] <> hhConfig,
          extDerivePredicates =
            [ "hack.NameLowerCase"
            , "hack.AttributeToDefinition"
            , "hack.AttributeHasParameter"
            , "hack.NamespaceMember"
            , "hack.ContainerParent"
            , "hack.ContainerChild"
            , "hack.MethodOverridden"
            , "hack.TargetUses"
            , "hack.DeclarationSource"
            ]
        }

        hhConfig = concatMap (\x -> ["--config", x]) $
          [ "symbol_write_include_hhi=false"
          , "symbol_write_root_path=" <> hackWriteRoot
          , "symbolindex_search_provider=NoIndex"
          , "use_mini_state=true"
          , "lazy_decl=true"
          , "lazy_parse=true"
          , "lazy_init2=true"
          , "enable_enum_classes=true"
          , "enable_enum_supertyping=true"
          ] <>
          (case hackOnly of
            Nothing -> []
            Just paths -> ["symbol_write_index_paths=" <> paths])

    indexerRun externalIndexer ext
  }
