{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
module Glean.Glass.Tracing
  ( GlassTrace(..)
  , GlassTracer
  , traceSpan
  , glassTraceEvent
  ) where

import Data.Text (Text)

import Control.Trace (traceMsg, Tracer)

import Glean.Glass.GlassService.Service as Glass
import Glean.Glass.Types
import Data.Aeson ( pairs, fromEncoding, toEncoding, KeyValue((.=)) )
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Binary.Builder ( toLazyByteString )

type GlassTracer = Tracer GlassTrace

traceSpan :: GlassTracer -> Text -> IO a -> IO a
traceSpan tracer span = traceMsg tracer (TraceSpan span Nothing)

data GlassTrace where
  TraceCommand :: forall r . GlassServiceCommand r -> GlassTrace
  TraceSpan :: !Text -> Maybe Text -> GlassTrace

glassTraceEvent :: GlassTrace -> (Text, Maybe Text)
glassTraceEvent (TraceSpan event args) = (event, args)
glassTraceEvent (TraceCommand cmd) = case cmd of
  Glass.SuperFacebookService r -> ("SuperFacebookService", Nothing)
  Glass.DocumentSymbolListX DocumentSymbolsRequest{..} opts ->
    ( "DocumentSymbolListX"
    , json $ pairs $
       "filepath" .= documentSymbolsRequest_filepath <>
       "repository" .= documentSymbolsRequest_repository <>
       "revision" .= requestOptions_revision opts <>
       "exact" .= requestOptions_exact_revision opts
    )
  Glass.DocumentSymbolIndex DocumentSymbolsRequest{..} opts ->
    ("DocumentSymbolIndex"
    , json $ pairs $
       "filepath" .= documentSymbolsRequest_filepath <>
       "repository" .= documentSymbolsRequest_repository <>
       "revision" .= requestOptions_revision opts <>
       "exact" .= requestOptions_exact_revision opts
    )
  Glass.FindReferences r opts ->
    ( "FindReferences" , json $ toEncoding r)
  Glass.FindReferenceRanges r opts ->
    ("FindReferenceRanges", json $ toEncoding r)
  Glass.ResolveSymbolRange r opts ->
    ("ResolveSymbolRange", json $ toEncoding r)
  Glass.DescribeSymbol r opts ->
    ("DescribeSymbol", json $ toEncoding r)
  Glass.SearchSymbol r opts ->
    ("SearchSymbol", json $ toEncoding r)
  Glass.SearchRelated r opts req ->
    ("SearchRelated", json $ toEncoding r)
  Glass.SearchRelatedNeighborhood r opts req ->
    ("SearchRelatedNeighborhood", json $ toEncoding r)
  Glass.SearchBySymbolId r opts ->
    ("SearchBySymbolId", json $ toEncoding r)
  Glass.Index r ->
    ("Index", json $ toEncoding r)
  Glass.FileIncludeLocations r opts ->
    ("FileIncludeLocations", json $ toEncoding r)
  Glass.ClangUSRToDefinition r opts ->
    ("ClangUSRToDefinition", json $ toEncoding r)

  where
    json =
      Just . toStrict . decodeUtf8 . toLazyByteString . fromEncoding
