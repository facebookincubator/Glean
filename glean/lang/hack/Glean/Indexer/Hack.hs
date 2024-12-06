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
          extRunScript = hackBinary,
          extFlavour = Json,
          extArgs =
            [ "${TEST_ROOT}"
            , "--write-symbol-info"
            , "${JSON_BATCH_DIR}"
            ] <> hhConfig,
          extDerivePredicates =
            [ "hack.NameLowerCase" -- retire me
            , "hack.AttributeToDefinition"
            , "hack.AttributeHasParameter"
            , "hack.NamespaceMember"
            , "hack.ContainerParent"
            , "hack.ContainerChild"
            , "hack.ModuleChild"
            , "hack.MethodOverridden"
            , "hack.TargetUses"
            , "hack.AttributeToDeclaration"
            , "hack.ThriftToHack"
            -- new search predicates replacing hack.NameLowerCase
            , "hack.SearchClassConstByName"
            , "hack.SearchClassConstByLowerCaseName"
            , "hack.SearchEnumeratorByName"
            , "hack.SearchEnumeratorByLowerCaseName"
            , "hack.SearchMethodByName"
            , "hack.SearchMethodByLowerCaseName"
            , "hack.SearchPropertyByName"
            , "hack.SearchPropertyByLowerCaseName"
            , "hack.SearchTypeConstByName"
            , "hack.SearchTypeConstByLowerCaseName"
            , "hack.SearchModuleByName"
            , "hack.SearchModuleByLowerCaseName"
            , "hack.SearchClassByName"
            , "hack.SearchClassByLowerCaseName"
            , "hack.SearchInterfaceByName"
            , "hack.SearchInterfaceByLowerCaseName"
            , "hack.SearchTraitByName"
            , "hack.SearchTraitByLowerCaseName"
            , "hack.SearchEnumByName"
            , "hack.SearchEnumByLowerCaseName"
            , "hack.SearchFunctionByName"
            , "hack.SearchFunctionByLowerCaseName"
            , "hack.SearchGlobalConstByName"
            , "hack.SearchGlobalConstByLowerCaseName"
            , "hack.SearchTypedefByName"
            , "hack.SearchTypedefByLowerCaseName"
            , "hack.SearchNamespaceByName"
            , "hack.SearchNamespaceByLowerCaseName"
            ]
        }

        hhConfig = concatMap (\x -> ["--config", x]) $
          [ "symbol_write_include_hhi=false"
          , "symbol_write_root_path=" <> hackWriteRoot
          , "symbolindex_search_provider=NoIndex"
          , "lazy_decl=true"
          , "lazy_parse=true"
          , "lazy_init2=true"
          , "symbol_write_sym_hash_out=true"
          ] <>
          (case hackOnly of
            Nothing -> []
            Just paths -> ["symbol_write_index_paths=" <> paths])

    indexerRun externalIndexer ext
  }
