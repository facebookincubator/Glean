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
  testDescribeSymbolComments
) where

import Data.Default
import Data.List
import qualified Data.Map as Map
import qualified Data.Text as Text
import Test.HUnit hiding (Path)

import Glean
import Glean.Util.Some

import Glean.Glass.Handler
import Glean.Glass.Types

import Glean.Glass.Regression.Util


-- | Test that documentSymbolListX returns non-empty definitions and
-- references for the given Path.
testDocumentSymbolListX :: Path -> IO (Some Backend, Repo) -> Test
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
      print res
      assertBool "documentSymbolListX"
        (not (null (documentSymbolListXResult_references res)) &&
         not (null (documentSymbolListXResult_definitions res)))

-- | Test that both describeSymbol and resolveSymbol for a SymbolId
-- find the symbol in a given Path.
testDescribeSymbolMatchesPath
  :: SymbolId -> Path -> IO (Some Backend, Repo) -> Test
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
  :: SymbolId -> (Int, Int) -> IO (Some Backend, Repo) -> Test
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

-- | Test findReferences and findReferenceRanges: given a SymbolId check
-- that the number of references returned for each Path matches the input.
testFindReferences
  :: SymbolId -> [(Path,Int)] -> IO (Some Backend, Repo) -> Test
testFindReferences sym@(SymbolId name) paths get =
  TestLabel (Text.unpack name) $ TestCase $ do
    (backend, _repo) <- get
    withTestEnv backend $ \env -> do
      locs <- findReferences env sym def
      let cmpLocs results =
            Map.fromList paths ==
            Map.fromList
              [ (head paths, length paths) | paths <- group (sort results) ]
      assertBool "findReferences Path matches" $
        cmpLocs (map location_filepath locs)
      locs <- findReferenceRanges env sym def
      assertBool "findReferences Path matches" $
        cmpLocs (map locationRange_filepath locs)
