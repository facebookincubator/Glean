{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-unused-matches #-}
module Glean.Glass.Tracing
  ( GlassTrace(..)
  , GlassTraceWithId(..)
  , GlassTracer
  , TraceId
  , Tracer
  , traceSpan
  , glassTraceEvent
  ) where

import Data.Text (Text)
import TextShow

import Control.Trace (traceMsg, Tracer)

import Glean.Glass.GlassService.Service as Glass
import Glean.Glass.Types
import Data.Aeson ( pairs, fromEncoding, toEncoding, KeyValue((.=)) )
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Binary.Builder ( toLazyByteString )

type GlassTracer = Tracer GlassTrace

type TraceId = Int

data GlassTraceWithId = GlassTraceWithId
  { traceId :: !TraceId
  , traceEvent :: !GlassTrace
  }

instance TextShow GlassTraceWithId where
  showb t = case glassTraceEvent t of
    (id, _, _) -> showb id

data GlassTrace where
  TraceCommand :: forall r . GlassServiceCommand r -> GlassTrace
  TraceSpan :: !Text -> Maybe Text -> GlassTrace

instance TextShow GlassTrace where
  showb t = case glassTraceEvent (GlassTraceWithId 0 t) of
    (id, _, _) -> showb id

traceSpan :: GlassTracer -> Text -> IO a -> IO a
traceSpan tracer span = traceMsg tracer (TraceSpan span Nothing)

{-# INLINE glassTraceEvent #-}
glassTraceEvent :: GlassTraceWithId -> (Text, Int, Maybe Text)
glassTraceEvent (GlassTraceWithId tid (TraceSpan event args)) =
  (event, tid, args)
glassTraceEvent (GlassTraceWithId tid (TraceCommand cmd)) = case cmd of
  Glass.SuperFacebookService r -> ("SuperFacebookService", tid, Nothing)
  Glass.DocumentSymbolListX DocumentSymbolsRequest{..} opts ->
    ( "DocumentSymbolListX"
    , tid
    , json $ pairs $
       "filepath" .= documentSymbolsRequest_filepath <>
       "repository" .= documentSymbolsRequest_repository <>
       "revision" .= requestOptions_revision opts <>
       "exact" .= requestOptions_exact_revision opts <>
       "matching" .= requestOptions_matching_revision opts
    )
  Glass.DocumentSymbolIndex DocumentSymbolsRequest{..} opts ->
    ("DocumentSymbolIndex"
    , tid
    , json $ pairs $
       "filepath" .= documentSymbolsRequest_filepath <>
       "repository" .= documentSymbolsRequest_repository <>
       "revision" .= requestOptions_revision opts <>
       "exact" .= requestOptions_exact_revision opts <>
       "matching" .= requestOptions_matching_revision opts
    )
  Glass.FindReferences r opts ->
    ( "FindReferences", tid, json $ toEncoding r)
  Glass.FindReferenceRanges r opts ->
    ("FindReferenceRanges", tid, json $ toEncoding r)
  Glass.ResolveSymbolRange r opts ->
    ("ResolveSymbolRange", tid, json $ toEncoding r)
  Glass.DescribeSymbol r opts ->
    ("DescribeSymbol", tid, json $ toEncoding r)
  Glass.SearchSymbol r opts ->
    ("SearchSymbol", tid, json $ toEncoding r)
  Glass.SearchRelated r opts req ->
    ("SearchRelated", tid, json $ toEncoding r)
  Glass.SearchRelatedNeighborhood r opts req ->
    ("SearchRelatedNeighborhood", tid, json $ toEncoding r)
  Glass.FileIncludeLocations r opts ->
    ("FileIncludeLocations", tid, json $ toEncoding r)
  Glass.ClangUSRToDefinition r opts ->
    ("ClangUSRToDefinition", tid, json $ toEncoding r)

  where
    json =
      Just . toStrict . decodeUtf8 . toLazyByteString . fromEncoding
