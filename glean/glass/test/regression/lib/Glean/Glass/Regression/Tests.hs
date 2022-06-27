{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Regression.Tests (
  testDocumentSymbolListX,
  testDescribeSymbolMatchesPath,
  testFindReferences,
  testDescribeSymbolComments,
  testDescribeSymbolHasAnnotations,
  testDescribeSymbolHasVisibility,
  testSearchRelated,
  testResolveSymbol,
  testSearchSymbolByName
) where

import Data.Default
import Data.List
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit hiding (Path)

import Glean
import Glean.Util.Some

import Glean.Glass.Handler
import Glean.Glass.Types as Glass

import Glean.Glass.Regression.Util

type Getter = IO (Some Backend, Repo)

-- | Test that documentSymbolListX returns non-empty definitions and
-- references for the given Path.
testDocumentSymbolListX :: Path -> Getter -> Test
testDocumentSymbolListX path get =
  TestLabel "testDocumentSymbolListX" $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      let req = DocumentSymbolsRequest
            { documentSymbolsRequest_repository = RepoName "test"
            , documentSymbolsRequest_filepath = path
            , documentSymbolsRequest_range = Nothing
            , documentSymbolsRequest_include_refs = True
            }
      res <- documentSymbolListX env req def
      assertBool "documentSymbolListX"
        (not (null (documentSymbolListXResult_references res)) &&
         not (null (documentSymbolListXResult_definitions res)))

testResolveSymbol :: SymbolId -> Path -> Getter -> Test
testResolveSymbol sym@(SymbolId name) path get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      LocationRange{..} <- resolveSymbolRange env sym def
      assertEqual "resolveSymbol Path matches" locationRange_filepath path

testSearchSymbolByName :: Text -> SymbolId -> Getter -> Test
testSearchSymbolByName searchStr sym@(SymbolId name) get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      syms <- searchByNameResult_symbols <$> searchByName env req def
      assertBool "searchByName" (elem sym syms)
  where
    req = SearchByNameRequest ctx searchStr False True
    ctx = SearchContext (Just $ RepoName "test") Nothing{-lang-} def{- kinds -}

-- | Test that both describeSymbol and resolveSymbol for a SymbolId
-- find the symbol in a given Path.
testDescribeSymbolMatchesPath
  :: SymbolId -> Path -> Getter -> Test
testDescribeSymbolMatchesPath sym@(SymbolId name) path get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      SymbolDescription{..} <- describeSymbol env sym def
      assertEqual "describeSymbol Path matches"
        (symbolPath_filepath symbolDescription_location)
        path
      Location{..} <- resolveSymbol env sym def
      assertEqual "resolveSymbol Path matches" location_filepath path

-- | Test that both describeSymbol has expected comment
testDescribeSymbolComments
  :: SymbolId -> (Int, Int) -> Getter -> Test
testDescribeSymbolComments sym@(SymbolId name) (line, col) get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      SymbolDescription{..} <- describeSymbol env sym def
      assertEqual "describeSymbol Comment start matches"
        [(fromIntegral line, fromIntegral col)] $
        zip
          (range_lineBegin . locationRange_range
            <$> symbolDescription_comments)
          (range_columnBegin . locationRange_range
            <$> symbolDescription_comments)

-- | Test that describeSymbol has specific annotations
testDescribeSymbolHasAnnotations
  :: SymbolId -> [(Text, Text)] -> Getter -> Test
testDescribeSymbolHasAnnotations sym@(SymbolId name) anns get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      SymbolDescription{..} <- describeSymbol env sym def
      assertEqual "describeSymbol Annotations equal"
        anns
        (maybe []
          (map $ \Annotation{..} -> (annotation_name, annotation_source))
          symbolDescription_annotations)

-- | Test that describeSymbol has a specific visibility
testDescribeSymbolHasVisibility
  :: SymbolId -> Visibility -> Getter -> Test
testDescribeSymbolHasVisibility sym@(SymbolId name) vis get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      SymbolDescription{..} <- describeSymbol env sym def
      assertEqual "describeSymbol Visibility equal"
        (Just vis)
        symbolDescription_visibility

-- | Test findReferences and findReferenceRanges: given a SymbolId check
-- that the number of references returned for each Path matches the input.
testFindReferences
  :: SymbolId -> [(Path,Int)] -> Getter -> Test
testFindReferences sym@(SymbolId name) paths get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      locs <- findReferences env sym def
      let
        assertLocsEq s results =
          assertEqual s
            (Map.fromList paths )
            (Map.fromList
              [ (head paths, length paths) | paths <- group (sort results) ])
      assertLocsEq "findReferences Path matches" $
        map location_filepath locs
      locs <- findReferenceRanges env sym def
      assertLocsEq "findReferences Path matches" $
        map locationRange_filepath locs

-- | Test search related contains an edge
testSearchRelated
  :: SymbolId
  -> Bool
  -> RelationDirection
  -> RelationType
  -> (SymbolId, SymbolId)
  -> Getter -> Test
testSearchRelated sym recurse dir rel (parent, child) get =
  TestLabel label $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      SearchRelatedResult{..} <-
        searchRelated env sym def SearchRelatedRequest {..}
      let
        actual = -- better error messages
          if
            any
              ((==) relatedSymbols_parent . Glass.relatedSymbols_parent)
              searchRelatedResult_edges &&
            any
              ((==) relatedSymbols_child . Glass.relatedSymbols_child)
              searchRelatedResult_edges
          then [(parent, arrow, child)]
          else
            map
              (\(RelatedSymbols p c) -> (p, arrow, c))
              searchRelatedResult_edges
      assertEqual
        "searchRelated contains edge" [(parent, arrow, child)] actual
  where
    label =
      showSymbolId sym <>
      ": " <>
      showSymbolId parent <>
      " " <>
      arrow <>
      " " <>
      showSymbolId child
    showSymbolId (SymbolId sym) = Text.unpack sym
    searchRelatedRequest_relatedBy = rel
    searchRelatedRequest_relation = dir
    searchRelatedRequest_recursive = recurse
    searchRelatedRequest_filter = Nothing
    searchRelatedRequest_detailedResults = False -- don't compute details
    relatedSymbols_parent = parent
    relatedSymbols_child = child
    arrowTail = if recurse then "-/ /-" else "--"
    arrow = case dir of
      RelationDirection_Parent -> arrowTail <> show rel <> "->"
      RelationDirection_Child -> "<-" <> show rel <> arrowTail
      RelationDirection__UNKNOWN{} -> "??" <> show rel <> "??"
