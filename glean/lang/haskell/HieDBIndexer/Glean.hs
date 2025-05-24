{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module HieDBIndexer.Glean where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_)
import Data.Array.Unboxed (elems)
import Data.Default
import qualified Data.HashMap.Strict as Map
import qualified Data.Map as AMap
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import qualified Glean
import Glean.Derive (derivePredicate)
import qualified Glean.Schema.Hs as Hs
import qualified Glean.Schema.Hs.Types as Hs
import qualified Glean.Schema.Src as Src
import qualified Glean.Schema.Src.Types as Src
import HieDBIndexer.Types (
  Definition (..),
  FileLineMap,
  FileLocation (..),
  IndexerBatchOutput (..),
  LineLengthArray,
  NodeDefinition (..),
  XReferences (..),
  mkGleanByteSpan,
  mkGleanXReference,
 )
import Text.Printf
import Util.Log

data IndexerException = DatabaseAlreadyExistsException
  deriving (Show, Typeable)

instance Exception IndexerException

-- |  Throws a 'DatabaseAlreadyExistsException'
createGleanDB ::
  (Glean.Backend b) =>
  b ->
  Bool ->
  Glean.Repo ->
  (Glean.Writer -> IO ()) ->
  IO ()
createGleanDB backend dontCreateDb newRepo write = do
  let Glean.Repo{..} = newRepo

  logInfo $ printf "Creating Glean DB %s/%s" repo_name repo_hash

  let finalWriter = do
        Glean.withSender backend newRepo allPredicates def $ \sender -> do
          Glean.withWriter sender def $ \writer -> do
            write writer

        let mkPredRef predName =
              Glean.SourceRef {
                sourceRefName = predName,
                sourceRefVersion = Nothing
              }
            predsToDerive = [
              "hs.FileDefinition",
              "hs.TargetUses",
              "hs.DefinitionNameLowerCase"
          --    "hs.ModuleNameLowerCase",
          --    "hs.FunctionNameLowerCase",
          --    "hs.ClassNameLowerCase"
              ]

        -- Derive the predicates needed by Glass
        mapConcurrently_
          ( \s ->
              derivePredicate
                backend
                newRepo
                Nothing
                Nothing
                (mkPredRef s)
                Nothing
          )
          predsToDerive

  if dontCreateDb
    then finalWriter
    else
      Glean.fillDatabase
        backend
        newRepo
        Nothing
        (throwIO DatabaseAlreadyExistsException)
        finalWriter

  predicates <-
    Glean.schemaInfo_predicateIds
      <$> Glean.getSchemaInfo backend (Just newRepo)
            def { Glean.getSchemaInfo_omit_source = True }
  repoStats <- Glean.predicateStats backend newRepo Glean.ExcludeBase
  let readableStats =
        [ printf " - %s: count = %d, size = %d"
          (Text.unpack predicateRef_name)
          predicateStats_count
          predicateStats_size
        | (p, Glean.PredicateStats {..}) <- AMap.toList repoStats
        , Just Glean.PredicateRef {..} <- [AMap.lookup p predicates]
        ]

  logInfo $ unlines $ "Repo stats: " : readableStats

hieFactsBuilder ::
  FileLineMap ->
  [IndexerBatchOutput] ->
  Glean.FactBuilder
hieFactsBuilder fileLinesMap batchOutputs = do
  let
      fileLinesList = Map.toList fileLinesMap

      batchWriter IndexerBatchOutput {..} =
        gleanWriter nodeDefs xrefList

  -- Write FileLines facts first.
  writeFileLines fileLinesList

  -- Open each output file and write the definitions and xrefs
  mapM_ batchWriter batchOutputs

-- | Given a FileXRefMap, create and write Glean facts to a database.
gleanWriter ::
  [NodeDefinition] ->
  -- | FileXRefMap in list format.
  [(FilePath, [XReferences])] ->
  Glean.FactBuilder
gleanWriter nodeDefs xrefList = do
  -- Write the definition facts
  mapM_ mkNodeDefFact nodeDefs

  -- Write the FileXRefMap facts
  forM_ xrefList $ \(sourcePath, xrefs) -> do
    xrefsFacts <- mapM mkXRefsFacts xrefs
    fileFact <-
      Glean.makeFact @Src.File $
        Text.pack sourcePath
    Glean.makeFact_ @Hs.FileXRefMap $ Hs.FileXRefMap_key fileFact xrefsFacts
  where
    mkXRefsFacts XReferences {..} = do
      defNameFact <- Glean.makeFact @Hs.DefinitionName $ targetName
      return $ mkGleanXReference defNameFact spans

    mkNodeDefFact (Name definition) = mkDefFact definition
    mkNodeDefFact (Type definition) = mkDefFact definition
    mkNodeDefFact (Constructor definition) = mkDefFact definition

    mkDefFact Definition {..} = do
      defName <- Glean.makeFact @Hs.DefinitionName qualName
      fileFact <-
        Glean.makeFact @Src.File $
          Text.pack (fileName loc)
      let fileLocFact =
            Src.FileLocation fileFact (mkGleanByteSpan $ locSpan loc)

      Glean.makeFact_ @Hs.Definition $ Hs.Definition_key defName fileLocFact

-- | Given a FileXRefMap, create and write Glean facts to a database.
writeFileLines ::
  [(FilePath, LineLengthArray)] ->
  Glean.FactBuilder
writeFileLines fileLinesList = do
  forM_ fileLinesList $ \(srcFp, lineLens) -> do
    let fp = Text.pack srcFp
    fileFact <- Glean.makeFact @Src.File fp
    Glean.makeFact_ @Src.FileLines
      Src.FileLines_key
        { fileLines_key_file = fileFact
        , fileLines_key_lengths =
            map (Glean.Nat . fromIntegral) $ elems lineLens
        , fileLines_key_endsInNewline = True
        , fileLines_key_hasUnicodeOrTabs = False
        }

allPredicates :: [Glean.SchemaPredicates]
allPredicates = [Src.allPredicates, Hs.allPredicates]
