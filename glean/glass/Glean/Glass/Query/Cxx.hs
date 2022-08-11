{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ApplicativeDo #-}

module Glean.Glass.Query.Cxx
  (
    documentSymbolsForCxx,
    fileIncludeLocationsForCxx,
  ) where

import Data.Maybe ( catMaybes )
import Data.Map.Strict ( Map )
import qualified Data.Map as Map
import Data.Text (Text)
import Util.List ( uniq )

import qualified Glean
import Glean.Angle
import Glean.Haxl.Repos as Glean

import qualified Glean.Schema.CodemarkupCxx.Types as Code
import qualified Glean.Schema.CodemarkupPp.Types as Code
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.CodeCxx.Types as Cxx
import qualified Glean.Schema.CodePp.Types as Pp
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Src.Types as Src
import Glean.Util.ToAngle ( ToAngle(toAngle) )
import qualified Glean.Util.Range as Range

import Glean.Glass.Utils

--
-- The cxx1 schema is complicated for xrefs and decls, as files have multiple
-- "traces" that instantiate them.
--
-- These are represented by cxx1.Trace values, and cxx1.FileXRef facts
--
-- We pick one trace and filexref set to start with, and use that to
-- consistently index.
--
-- Definitions in file:
-- * codemarkup.{cxx,pp}.{Cxx,Pp}ResolveTraceLocations
--
-- Computing xrefs:
-- * fixed xrefs
-- * variable xrefs
-- * uses of decls to their defs
-- * decls to def xrefs
-- * def to decl xrefs
--
-- A frequent source of problems is enumerating too many traces and filexrefs
--
-- We also have a set of different kind of symbols
-- * declarations, definitions, enumerators
-- Compounding this, xrefs come in many forms
-- * direct, indirect, unknown ,..
--
-- Finally, fixed and external xrefs are logically just xrefs, but
-- one requires lookup and zip.
--
-- Finally, finally, we have to do c preprocessor symbols at the same time, via
-- PPTrace tables
--
documentSymbolsForCxx
  :: Maybe Int
  -> Bool  -- ^ include references?
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w
      ([(Code.XRefLocation,Code.Entity)] , [(Code.Location,Code.Entity)])
documentSymbolsForCxx mlimit includeRefs fileId = do
  mTraceId <- getFirstFileTrace fileId
  case mTraceId of
    Nothing -> return ([], [])
    Just traceId -> do
      -- these can run concurrently
      defns <- fileEntityLocations mlimit traceId
      xrefs <- if includeRefs
        then fileEntityXRefLocations mlimit fileId traceId
        else return []
      return (xrefs, defns)

-- | Get first (arbitrary) file trace
-- TODO: we can generalize this to return more traces
getFirstFileTrace
  :: Glean.IdOf Src.File
  -> Glean.RepoHaxl u w (Maybe (Glean.IdOf Cxx.Trace))
getFirstFileTrace = fetchFactIdOnly . cxxFileTrace

-- | Get first file xrefs set, and keep the fact id around
-- > cxx1.FileXRefs { xmap = { file = "fbcode/admarket/lib/stringdb/StringDB.h"
--
getFirstFileXRefs
  :: Glean.IdOf Src.File
  -> Glean.RepoHaxl u w (Maybe (Glean.IdOf Cxx.FileXRefs))
getFirstFileXRefs = fetchFactIdOnly . cxxFileXRefs

--
-- | Find the cxx and pp entities associated with a file, using
-- a specific trace.
--
fileEntityLocations
  :: Maybe Int
  -> Glean.IdOf Cxx.Trace
  -> Glean.RepoHaxl u w [(Code.Location, Code.Entity)]
fileEntityLocations mlimit traceId = searchRecursiveWithLimit mlimit $
  cxxPpResolveTraceLocations traceId

-- | Find xrefs from this file, and their associated entities,
-- using a specific FileXRefMap annd cxx1.Trace
--
-- Share the Cxx.Trace fact between Cpp decls and xrefs for consistency
--
fileEntityXRefLocations
  :: Maybe Int
  -> Glean.IdOf Src.File
  -> Glean.IdOf Cxx.Trace
  -> Glean.RepoHaxl u w [(Code.XRefLocation,Code.Entity)]
fileEntityXRefLocations mlimit fileId traceId = do
  mresult <- getFirstFileXRefs fileId
  case mresult of
    Nothing -> return []
    Just xrefId -> do
      fixedXRefs <- fixedXRefs mlimit xrefId
      variableXRefs <- externalXRefs mlimit xrefId
      ppxrefs <- ppXRefs mlimit traceId
      defXRefs <- declToDefXRefs mlimit traceId
      return (fixedXRefs ++ variableXRefs ++ ppxrefs ++ defXRefs)

-- fixed (easily discoverable) xrefs
fixedXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w [(Code.XRefLocation, Code.Entity)]
fixedXRefs mlimit xmapId = searchRecursiveWithLimit mlimit $
  cxxFileEntityXMapFixedXRefLocations xmapId

-- and the external xrefs (zip)
externalXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w [(Code.XRefLocation, Code.Entity)]
externalXRefs mlimit xrefId = do
  -- process concurrently
  maybeRawXRefs <- variableXRefs xrefId
  declToDefMap <- externalDeclToDefXRefs mlimit xrefId
  declLocMap <- externalDeclToLocations xrefId
  case maybeRawXRefs of
    Nothing -> return []
    Just (sources, targets) -> do
      locations <- mapM (cxxXRefTargetToLocation declLocMap) targets
      let ranges = relativeByteSpansToRanges sources
      let declXRefs = zipXRefSourceAndTargets ranges locations
      let defnXRefs = zipXRefSourcesAndDefinitions declToDefMap ranges locations
      return $ defnXRefs ++ declXRefs

-- N.B. Src.ByteSpans are _relative_ bytespans ([RelByteSpan]), it's misnamed
relativeByteSpansToRanges :: [Src.ByteSpans] -> [[Src.ByteSpan]]
relativeByteSpansToRanges =
  map (map Range.rangeToByteSpan . Range.relByteSpansToRanges)

-- Laboriously stitch the unzipped xref source and target into
-- XRefLocation and Entities again.
zipXRefSourceAndTargets
  :: [[Src.ByteSpan]]
  -> [Maybe (Code.Entity, Code.Location)]
  -> [(Code.XRefLocation, Code.Entity)]
zipXRefSourceAndTargets sources targets =
  [ (xrefFromLocationAndSpan targetLocation span, entity)
  | (spans, Just (entity, targetLocation) ) <- zip sources targets
  , span <- spans
  ]

-- Laboriously stitch the unzipped xref source to target _definition_
-- by looking it up in the decl to def map
zipXRefSourcesAndDefinitions
  :: DeclToDefMap
  -> [[Src.ByteSpan]]
  -> [Maybe (Code.Entity, Code.Location)]
  -> [(Code.XRefLocation, Code.Entity)]
zipXRefSourcesAndDefinitions declToDefMap sources targets = catMaybes
  [ case Map.lookup decl declToDefMap of
      Nothing -> Nothing
      Just (entity, targetLocation) -> Just
        (xrefFromLocationAndSpan targetLocation span, entity)
  | (spans, Just (Code.Entity_cxx (Cxx.Entity_decl decl), _) ) <-
      zip sources targets -- find just the decls
  , span <- spans
  ]

xrefFromLocationAndSpan :: Code.Location -> Src.ByteSpan -> Code.XRefLocation
xrefFromLocationAndSpan location span =
  Code.XRefLocation {
    Code.xRefLocation_target = location,
    Code.xRefLocation_source = Code.RangeSpan_span span
  }

-- | Map of decl to their def entity
type DeclToDefMap = Map Cxx.Declaration (Code.Entity, Code.Location)

-- The standard variable xrefs. Takes the first cxx.FileXRefs result for the
-- FileXRefMap fact. There may be additional facts (more xrefs in other traces)
variableXRefs
  :: Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w (Maybe ([Src.ByteSpans], [Cxx.XRefTarget]))
variableXRefs = fetchDataRecursive . cxxFileEntityXMapVariableXRefLocations

-- | Build lookup table of definitions found from declToDef calls
-- on the exteranl xrefs in the file
externalDeclToDefXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w DeclToDefMap
externalDeclToDefXRefs mlimit xrefId = do
  defRefs <- searchRecursiveWithLimit mlimit $
    cxxFileEntityXMapVariableXRefDeclToDefs xrefId
  return $ Map.fromList $
    map (\(decl, entity, loc) -> (decl, (entity, loc))) defRefs

-- | Map of external xref decl to location
type DeclLocationMap = Map Cxx.Declaration Code.Location

-- | Build lookup table of external xref target decls to their location
-- We need all results
externalDeclToLocations
  :: Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w DeclLocationMap
externalDeclToLocations xrefId = do
  Map.fromList <$> searchRecursiveWithLimit Nothing
    (cxxFileEntityXMapVariableXRefDeclLocations xrefId)

-- and the pp #define and #include occurences
ppXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.Trace
  -> Glean.RepoHaxl u w [(Code.XRefLocation, Code.Entity)]
ppXRefs mlimit traceId = searchRecursiveWithLimit mlimit $
  ppEntityTraceXRefLocations traceId

-- and the underlying definitions of any decls that have them
declToDefXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.Trace
  -> Glean.RepoHaxl u w [(Code.XRefLocation, Code.Entity)]
declToDefXRefs mlimit traceId = searchRecursiveWithLimit mlimit $
  cxxFileEntityTraceDeclToDefXRefLocations traceId

-- Basically the body of CxxFileEntityTraceFixedXRefLocations
-- But split between client and server calls.
--
-- TODO: this could be replaced with a single bulk fetch and Decl
-- index like we do for DeclToDef externals , for the declarations and enums
--
cxxXRefTargetToLocation
  :: DeclLocationMap
  -> Cxx.XRefTarget
  -> Glean.RepoHaxl u w (Maybe (Code.Entity, Code.Location))
cxxXRefTargetToLocation declLocMap (Cxx.XRefTarget_declaration decl) = do
  -- try the cache first
  mloc <- case Map.lookup decl declLocMap of
    Just location -> return $ Just location -- done
    Nothing -> do -- maybe it was indirect, fetch it
       mlocation <- fetchDataRecursive $ cxxDeclarationLocation (toAngle decl)
       case mlocation of
          Nothing -> return Nothing
          Just (range, name, span) -> do
            let location = Code.Location {
                Code.location_name = name,
                Code.location_file = Src.range_file range,
                Code.location_location = Code.RangeSpan_range range,
                Code.location_span = Just span
              }
            return $ Just location

  case mloc of
    Nothing -> return Nothing
    Just location -> do
      let entity = Code.Entity_cxx (Cxx.Entity_decl decl)
      return $ Just (entity, location)

cxxXRefTargetToLocation declLocMap (Cxx.XRefTarget_indirect indirect) = do
  Cxx.XRefIndirectTarget_key _via target <- Glean.keyOf indirect
  cxxXRefTargetToLocation declLocMap target  -- n.b recurse

cxxXRefTargetToLocation _ (Cxx.XRefTarget_enumerator enumerator) = do
  Cxx.Enumerator_key name _decl range <- Glean.keyOf enumerator
  nameStr <- Glean.keyOf name
  let entity = Code.Entity_cxx (Cxx.Entity_enumerator enumerator)
  let location = Code.Location {
      Code.location_name = nameStr,
      Code.location_file = Src.range_file range,
      Code.location_location = Code.RangeSpan_range range,
      Code.location_span = Nothing
    }
  return $ Just (entity, location)

{-
XRefTarget_objcSelector ObjcSelector
XRefTarget_unknown Loc
-}
cxxXRefTargetToLocation _ _ = return Nothing

--
-- Take the ids of the declaration and pptrace facts of the file
--
-- > fbsource> cxx1.Trace { file = "fbcode/folly/Synchronized.h" }
-- > { "id": 431259564, "key": { "tuplefield0": { "id":...
-- > 1 results, 1 facts, 0.43ms, 240632 bytes, 1253 compiled bytes
--
-- N.B. this is a predicate query, not a data query
--
cxxFileTrace :: Glean.IdOf Src.File -> Angle Cxx.Trace
cxxFileTrace fileId =
  predicate @Cxx.Trace (
    rec $
      field @"file" (asPredicate (factId fileId))
    end
  )

--
-- Same idea as for cxx1.Trace, for xrefs. There are multiple "traces"
-- of xref maps, so pick the first one, then key off it.
--
cxxFileXRefs :: Glean.IdOf Src.File -> Angle Cxx.FileXRefs
cxxFileXRefs fileId =
  predicate @Cxx.FileXRefs (
    rec $
      field @"xmap" (
        rec $ field @"file" (asPredicate (factId fileId)) end
      )
    end
  )

--
-- Find file entities for a specific trace associated with a file
--
cxxPpResolveTraceLocations
  :: Glean.IdOf Cxx.Trace
  -> Angle (Code.Location, Code.Entity)
cxxPpResolveTraceLocations traceId =
  vars $ \(location :: Angle Code.Location)
          (entity :: Angle Code.Entity)
          (cxx_entity :: Angle Cxx.Entity)
          (pp_entity :: Angle Pp.Entity) ->
    tuple (location,entity) `where_` [
      wild .=
        [
          wild .= predicate @Code.CxxResolveTraceLocation (
            rec $
              field @"trace" (asPredicate (factId traceId)) $
              field @"location" location $
              field @"entity" cxx_entity
            end),
          entity .= sig (alt @"cxx" cxx_entity)
        ] `or_` [
          wild .= predicate @Code.PpResolveTraceLocation (
            rec $
              field @"trace" (asPredicate (factId traceId)) $
              field @"location" location $
              field @"entity" pp_entity
            end),
          entity .= sig (alt @"pp" pp_entity)
        ]
    ]

-- Fixed XRefs associated with a file and xmap /xref set
cxxFileEntityXMapFixedXRefLocations
  :: Glean.IdOf Cxx.FileXRefs
  -> Angle (Code.XRefLocation, Code.Entity)
cxxFileEntityXMapFixedXRefLocations xrefId =
  vars $ \(xref :: Angle Code.XRefLocation) (entity :: Angle Cxx.Entity) ->
    tuple (xref, sig (alt @"cxx" entity) :: Angle Code.Entity) `where_` [
      wild .= predicate @Code.CxxFileEntityXMapFixedXRefLocations (
        rec $
          field @"trace" (asPredicate (factId xrefId)) $
          field @"xref" xref $
          field @"entity" entity
        end)
      ]

--
-- "Variable" xrefs associated with a file and an xref map
-- These get processed on the client, here, into Entity and XRefLocations
--
cxxFileEntityXMapVariableXRefLocations
  :: Glean.IdOf Cxx.FileXRefs
  -> Angle ([Src.ByteSpans], [Cxx.XRefTarget])
cxxFileEntityXMapVariableXRefLocations xrefId =
  vars $ \(spans :: Angle [Src.ByteSpans])
      (targets :: Angle [Cxx.XRefTarget]) ->
    tuple (spans,targets) `where_` [
      factId xrefId .= predicate @Cxx.FileXRefs (
        rec $
          field @"xmap" (rec $ field @"variable" spans end) $
          field @"externals" targets
        end
      )
    ]

--
-- Defn xref targets computed from file Decls
-- N.B. won't contain indirect decls. Just the first level Decls
--
cxxFileEntityTraceDeclToDefXRefLocations
  :: Glean.IdOf Cxx.Trace
  -> Angle (Code.XRefLocation, Code.Entity)
cxxFileEntityTraceDeclToDefXRefLocations traceId =
  vars $ \(xref :: Angle Code.XRefLocation) (entity :: Angle Cxx.Entity) ->
    tuple (xref, sig (alt @"cxx" entity) :: Angle Code.Entity) `where_` [
      wild .= predicate @Code.CxxFileEntityTraceDeclToDefXRefLocations (
        rec $
          field @"trace" (asPredicate (factId traceId)) $
          field @"xref" xref $
          field @"entity" entity
        end)
      ]

--
-- Get location information for a Cxx Declaration
--
cxxDeclarationLocation :: Angle Cxx.Declaration
  -> Angle (Src.Range, Text, Src.ByteSpan)
cxxDeclarationLocation decl =
  vars $ \(source :: Angle Src.Range)
          (name :: Angle Text)
          (span :: Angle Src.ByteSpan) ->
  tuple (source, name, span) `where_` [
    wild .= predicate @Cxx.DeclarationLocationNameSpan (
      rec $
        field @"decl" decl $
        field @"source" source $
        field @"name" name $
        field @"span" span
      end)
    ]

--
-- decl xref target locations computed from external xrefs, bulk fetch
--
cxxFileEntityXMapVariableXRefDeclLocations
  :: Glean.IdOf Cxx.FileXRefs
  -> Angle (Cxx.Declaration, Code.Location)
cxxFileEntityXMapVariableXRefDeclLocations xrefId =
  vars $ \(decl :: Angle Cxx.Declaration)
      (location :: Angle Code.Location) ->
    tuple (decl, location) `where_` [
      wild .= predicate @Code.CxxFileEntityXMapVariableXRefDeclLocations (
        rec $
          field @"trace" (asPredicate (factId xrefId)) $
          field @"source" decl $
          field @"location" location
        end)
      ]

--
-- Defn xref targets computed from external decls
-- Returns pairs of each decl fact and the corresponding xref defn target
--
cxxFileEntityXMapVariableXRefDeclToDefs
  :: Glean.IdOf Cxx.FileXRefs
  -> Angle (Cxx.Declaration, Code.Entity, Code.Location)
cxxFileEntityXMapVariableXRefDeclToDefs xrefId =
  vars $ \(decl :: Angle Cxx.Declaration) (entity :: Angle Cxx.Entity)
      (location :: Angle Code.Location) ->
    tuple (decl, sig (alt @"cxx" entity) :: Angle Code.Entity, location)
    `where_` [
      wild .= predicate @Code.CxxFileEntityXMapVariableXRefDeclToDefs (
        rec $
          field @"trace" (asPredicate (factId xrefId)) $
          field @"source" decl $
          field @"entity" entity $ -- todo this needs to be a location
          field @"location" location
        end)
      ]

-- C preprocessor #define and #include uses associated with a cxx1.Trace
ppEntityTraceXRefLocations
  :: Glean.IdOf Cxx.Trace
  -> Angle (Code.XRefLocation, Code.Entity)
ppEntityTraceXRefLocations traceId =
  vars $ \(xref :: Angle Code.XRefLocation) (entity :: Angle Pp.Entity) ->
    tuple (xref, sig (alt @"pp" entity) :: Angle Code.Entity) `where_` [
      wild .= predicate @Code.PpEntityTraceXRefLocations (
        rec $
          field @"trace" (asPredicate (factId traceId)) $
          field @"xref" xref $
          field @"entity" entity
        end)
      ]

--
-- | Breadth first, find at most @n@ #include src.file targets for #include
-- xrefs in this file. Recursively visiting each #include file to gather more
--
fileIncludeLocationsForCxx
  :: Int
  -> Maybe Int
  -> Src.File
  -> Glean.RepoHaxl u w (Map Src.File [(Src.File, Src.Range)])
fileIncludeLocationsForCxx depth mlimit file = go depth Map.empty [file]
  where
    go 0 acc _ = pure acc
    go _ acc [] = pure acc -- no more #includes to process
    go n !acc files = do
      res <- mapM fetch files -- ensure haxl can do this concurrently
      let seen = Map.fromList res `Map.union` acc
          candidates = uniq (map fst (concatMap snd res))
          new = filter (`Map.notMember` seen) candidates
      go (n-1) seen new

    fetch f = do -- named for haxl magic concurrency
      rs <- fileIncludeLocations mlimit (Glean.getId f)
      return (f,rs)

fileIncludeLocations
  :: Maybe Int
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w [(Src.File, Src.Range)]
fileIncludeLocations mlimit fileId = do
  traces <- getPPTraces traceMaxLimit fileId
  let mquery = case traces of
        [] -> Nothing -- no pp trace events for this file
        [fact] -> Just (asPredicate (factId fact))
        facts -> Just $ foldr1 (.|) (map (asPredicate . factId) facts)
  case mquery of
    Nothing -> return []
    Just traceQ -> do
      searchRecursiveWithLimit mlimit (ppXRefFileLocations traceQ)
  where
    -- arbitrary number of fact traces to be reasonable. More than 1. But
    -- don't overwhelm things. c.f PPTrace { file = folly/Optional.h }
    -- also the query gets quite large
    traceMaxLimit = 10

--
-- #include resolution acceleration. We can quickly return target filepaths
-- for all or most #includes in a file.
--
-- N.B. we want more than 1, but not all, as some are large (>50k facts)
--
getPPTraces
  :: Int
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w [Glean.IdOf Cxx.PPTrace]
getPPTraces n fileId = fmap (Glean.getId . fst) <$>
  searchWithLimit (Just n) (factIdQuery (ppFileTrace fileId))

--
-- find the PPTrace facts when we only care about C pre-processor events
--
ppFileTrace :: Glean.IdOf Src.File -> Angle Cxx.PPTrace
ppFileTrace fileId =
  predicate @Cxx.PPTrace (
    rec $
      field @"file" (asPredicate (factId fileId))
    end
  )

-- | Given a PPTrace expression, find C preprocessor #include filepath targets
ppXRefFileLocations :: Angle Cxx.PPTrace_key -> Angle (Src.File, Src.Range)
ppXRefFileLocations ppTraceQ =
  vars $ \(filepath :: Angle Src.File) (range :: Angle Src.Range) ->
    tuple (filepath, range) `where_` [
      wild .= predicate @Code.PpIncludeXRefLocations (
        rec $
          field @"trace" ppTraceQ $
          field @"range" range $
          field @"target" (asPredicate filepath)
        end)
      ]

--
-- "On the economy of bandwidth through content elision
--     in the Glean database system"
--
-- This is a little trick to non-recursivley pull something that might
-- have a large payload, but we know we only need the fact id
--
fetchFactIdOnly
  :: (Glean.Predicate p, QueryType p)
  => Angle p -> Glean.RepoHaxl u w (Maybe (Glean.IdOf p))
fetchFactIdOnly p = fmap (Glean.getId . fst) <$> fetchData (factIdQuery p)

-- | Used to avoid recursive expansion when we just need a fact id
factIdQuery :: Angle t -> Angle (t, ())
factIdQuery p = var $ \r ->
  tuple (r, sig unit) `where_` [ r .= p ]
