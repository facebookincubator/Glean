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
    usrHashToDeclaration,
    usrHashToXRefs,
  ) where

import Data.Maybe ( catMaybes )
import Data.Map.Strict ( Map )
import qualified Data.Map as Map
import Data.Text (Text)
import Safe (atMay)
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

import Glean.Glass.XRefs ( XRef )
import Glean.Glass.Utils
import qualified Glean.Schema.Codemarkup.Types as Code

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
      ([XRef], [(Code.Location,Code.Entity)], Bool)
documentSymbolsForCxx mlimit includeRefs fileId = do
  mTraceId <- getFirstFileTrace fileId
  case mTraceId of
    Nothing -> return ([], [], False)
    Just traceId -> do
      -- these can run concurrently
      (defns, trunc1) <- fileEntityLocations mlimit traceId
      (xrefs, trunc2{- one of the sub queries truncated-}) <- if includeRefs
        then fileEntityXRefLocations mlimit fileId traceId
        else return ([], False)
      let (xrefs', trunc3) = maybeTake mlimit xrefs
      return (xrefs', defns, trunc1 || trunc2 || trunc3)

maybeTake :: Maybe Int -> [a] -> ([a], Bool)
maybeTake Nothing xs = (xs, False)
maybeTake (Just n) xs = (take n xs, m > n)
  where
    m = length xs

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
  -> Glean.RepoHaxl u w ([(Code.Location, Code.Entity)], Bool)
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
  -> Glean.RepoHaxl u w ([XRef], Bool)
fileEntityXRefLocations mlimit fileId traceId = do
  mresult <- getFirstFileXRefs fileId
  fixedAndVariable <- case mresult of -- C++ xrefs rely on a cxxFileXRefs fact
    Nothing -> return []
    Just xrefId -> do
      fixedXRefs <- fixedXRefs mlimit xrefId
      variableXRefs <- externalXRefs mlimit xrefId fileId
      return [fixedXRefs, variableXRefs]
  -- these do not depend on a cxx1.FileXRefs fact, run independently
  ppxrefs <- ppXRefs mlimit traceId
  defXRefs <- declToDefXRefs mlimit traceId
  spellingXRefs <- spellingXRefs mlimit fileId
  let result = fixedAndVariable ++ [ppxrefs, defXRefs, spellingXRefs]
  return (concatMap fst result, any snd result)

-- spelling (easily discoverable) xrefs
spellingXRefs
  :: Maybe Int
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w ([XRef], Bool)
spellingXRefs mlimit fileId = searchRecursiveWithLimit mlimit $
  cxxFileEntitySpellingXRefLocations fileId

-- fixed (easily discoverable) xrefs
fixedXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w ([XRef], Bool)
fixedXRefs mlimit xmapId = searchRecursiveWithLimit mlimit $
  cxxFileEntityXMapFixedXRefLocations xmapId

-- and the external xrefs (zip)
externalXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.IdOf Src.File
  -> Glean.RepoHaxl u w ([XRef], Bool)
externalXRefs mlimit xrefId fileId = do
  -- process concurrently
  maybeRawXRefs <- variableXRefs xrefId
  declToDefMap  <- externalDeclToDefXRefs mlimit xrefId
  declLocMap <- externalDeclToLocations mlimit xrefId
  case maybeRawXRefs of
    Nothing -> return ([], False)
    Just (sources, targets) -> do
      locations <- mapM (mapM (cxxXRefTargetToLocation (fst declLocMap)))
        [ xrefs | Cxx.XRefTargets{xRefTargets_key = Just xrefs} <- targets ]
      let ranges = fromToRanges sources
          declXRefs = zipXRefSourceAndTargets ranges locations
          defnXRefs = zipXRefSourcesAndDefinitions (fst declToDefMap)
                        ranges locations
      indirectXRefs <- catMaybes <$> mapM (mkIndirectXRef fileId)
        [ indirect
        | Cxx.XRefTargets { xRefTargets_key = Just xrefs} <- targets
        , Cxx.XRefTarget_indirect indirect <- xrefs -- initial indirect xrefs
        ]
      return (defnXRefs ++ declXRefs ++ indirectXRefs,
        snd declLocMap || snd declToDefMap)
  where
    fromToRanges :: [Cxx.From] -> [[Src.ByteSpan]]
    fromToRanges =
      map (map Range.rangeToByteSpan . Range.fromToSpansAndSpellings)

-- | Extract depth=1 "indirect" xref "via" targets, and make them into
-- first-class xrefs results. Build xref source span from `via` emit target
-- entity/location from target of "via"
mkIndirectXRef
  :: Glean.IdOf Src.File -- ^ we only want indirect xrefs whose use is here
  -> Cxx.XRefIndirectTarget
  -> Glean.RepoHaxl u w (Maybe XRef)
mkIndirectXRef fileId viaFact = do
  (Cxx.XRefIndirectTarget_key via target) <- Glean.keyOf viaFact
  mXRefTarget <- cxxXRefTargetToLocation mempty target
  case mXRefTarget of
    Nothing -> return Nothing -- filter unknowns
    Just (targetEntity, targetLocation) -> do -- now get the source from XRefVia
      mSourceRange <- getXRefViaRange via
      case mSourceRange of
        Just sourceRange -- validate the via use is in this file only
          | Glean.getId (Src.range_file sourceRange) == fileId -> do
            let xrefLocation = Code.XRefLocation {
                  Code.xRefLocation_target = targetLocation,
                  Code.xRefLocation_source = Code.RangeSpan_range sourceRange
                }
            return $ Just (xrefLocation, targetEntity)
        _ -> return Nothing

-- Find the use span of an indirect xref: the `via` xref source range
getXRefViaRange :: Cxx.XRefVia -> Glean.RepoHaxl u w (Maybe Src.Range)
getXRefViaRange via = case via of
  Cxx.XRefVia_usingDeclaration decl -> do
    Cxx.UsingDeclaration_key _name range <- Glean.keyOf decl
    return (Just range)
{-
-- TODO: until we verify how these behave, disable them:
-- e.g "using namespace" is potentially a UPND source due to the import of
-- many unrelated xrefs
  Cxx.XRefVia_usingDirective decl -> do
    Cxx.UsingDirective_key _name range <- Glean.keyOf decl
    return (Just range)
  Cxx.XRefVia_macro use -> do
    Pp.Use_key _m _d _e source _name <- Glean.keyOf use
    return (Just source) -- entire term including args is xref use site?
-}
  _ -> pure Nothing

-- Laboriously stitch the unzipped xref source and target into
-- XRefLocation and Entities again.
zipXRefSourceAndTargets
  :: [[Src.ByteSpan]]
  -> [[Maybe (Code.Entity, Code.Location)]]
  -> [XRef]
zipXRefSourceAndTargets sources targets =
  [ (xrefFromLocationAndSpan targetLocation span, entity)
  | (spans, xrefs) <- zip sources targets
  , span <- spans
  , Just (entity, targetLocation) <- xrefs -- filter unknown xref targets
  ]

-- Laboriously stitch the unzipped xref source to target _definition_
-- by looking it up in the decl to def map
zipXRefSourcesAndDefinitions
  :: DeclToDefMap
  -> [[Src.ByteSpan]]
  -> [[Maybe (Code.Entity, Code.Location)]]
  -> [XRef]
zipXRefSourcesAndDefinitions declToDefMap sources targets = catMaybes
  [ case entity of
      Cxx.Entity_decl decl -> do
        (entity, targetLocation) <- Map.lookup decl declToDefMap
        return (xrefFromLocationAndSpan targetLocation span, entity)
      Cxx.Entity_objcSelectorSlot (Cxx.ObjcSelectorSlotEntity
          (Cxx.ObjcMethodEntity_decl decl) idx) -> do
        (Code.Entity_cxx (Cxx.Entity_defn (Cxx.Definition_objcMethod defn)), _)
          <- Map.lookup (Cxx.Declaration_objcMethod decl) declToDefMap
        (entity, location) <- objcSelectorSlotLocation $
          Cxx.ObjcSelectorSlotEntity (Cxx.ObjcMethodEntity_defn defn) idx
        return (xrefFromLocationAndSpan location span, entity)
      _ -> Nothing
  | (spans, xrefs) <- zip sources targets -- find just the decls
  , span <- spans
  , Just (Code.Entity_cxx entity, _) <- xrefs
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
  -> Glean.RepoHaxl u w (Maybe ([Cxx.From], [Cxx.XRefTargets]))
variableXRefs = fetchDataRecursive . cxxFileEntityXMapVariableXRefLocations

-- | Build lookup table of definitions found from declToDef calls
-- on the exteranl xrefs in the file
externalDeclToDefXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w (DeclToDefMap, Bool)
externalDeclToDefXRefs mlimit xrefId = do
  (defRefs, truncated) <- searchRecursiveWithLimit mlimit $
    cxxFileEntityXMapVariableXRefDeclToDefs xrefId
  return $ (,truncated) (Map.fromList $
    map (\(decl, entity, loc) -> (decl, (entity, loc))) defRefs)

-- | Map of external xref decl to location
type DeclLocationMap = Map Cxx.Declaration Code.Location

-- | Build lookup table of external xref target decls to their location
-- We need all results, so just assume we don't want to exceed the limit.
-- The value here should actually be the length of the trace `targets*spans`?
externalDeclToLocations
  :: Maybe Int
  -> Glean.IdOf Cxx.FileXRefs
  -> Glean.RepoHaxl u w (DeclLocationMap, Bool)
externalDeclToLocations mlimit xrefId = do
  (rows, truncated) <- searchRecursiveWithLimit mlimit
    (cxxFileEntityXMapVariableXRefDeclLocations xrefId)
  return (Map.fromList rows, truncated)

-- and the pp #define and #include occurences
ppXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.Trace
  -> Glean.RepoHaxl u w ([XRef], Bool)
ppXRefs mlimit traceId = searchRecursiveWithLimit mlimit $
  ppEntityTraceXRefLocations traceId

-- and the underlying definitions of any decls that have them
declToDefXRefs
  :: Maybe Int
  -> Glean.IdOf Cxx.Trace
  -> Glean.RepoHaxl u w ([XRef], Bool)
declToDefXRefs mlimit traceId = searchRecursiveWithLimit mlimit $
  cxxFileEntityTraceDeclToDefXRefLocations traceId

objcSelectorSlotLocation
  :: Cxx.ObjcSelectorSlotEntity -> Maybe (Code.Entity, Code.Location)
objcSelectorSlotLocation slot@(Cxx.ObjcSelectorSlotEntity method idx) = do
  decl <- case method of
    Cxx.ObjcMethodEntity_decl decl -> Just decl
    Cxx.ObjcMethodEntity_defn Cxx.ObjcMethodDefinition{
      objcMethodDefinition_key = Just decl} -> Just decl
    _ -> Nothing
  Cxx.ObjcMethodDeclaration_key{
    objcMethodDeclaration_key_selector = Cxx.ObjcSelector{
      objcSelector_key = Just selector
    },
    objcMethodDeclaration_key_locations = locations
  } <- Glean.getFactKey decl
  let index = fromIntegral $ Glean.fromNat idx
  name <- atMay selector index
  Src.FileLocation{..} <- atMay locations index
  return (
    Code.Entity_cxx (Cxx.Entity_objcSelectorSlot slot),
    Code.Location {
      Code.location_name = name,
      Code.location_file = fileLocation_file,
      Code.location_location = Code.RangeSpan_span fileLocation_span,
      Code.location_destination = Nothing
    })

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
          Just (range, name, file, span) -> do
            let location = Code.Location {
                Code.location_name = name,
                Code.location_file = Src.range_file range,
                Code.location_location = Code.RangeSpan_range range,
                Code.location_destination = Just Src.FileLocation {
                  Src.fileLocation_file = file,
                  Src.fileLocation_span = span
                }
              }
            return $ Just location

  case mloc of
    Nothing -> return Nothing
    Just location -> do
      let entity = Code.Entity_cxx (Cxx.Entity_decl decl)
      return $ Just (entity, location)

-- Indirect xrefs: we resolve it fully to its target
cxxXRefTargetToLocation declLocMap (Cxx.XRefTarget_indirect indirect) = do
  Cxx.XRefIndirectTarget_key _via target <- Glean.keyOf indirect
  cxxXRefTargetToLocation declLocMap target  -- n.b recurse to find target

cxxXRefTargetToLocation _ (Cxx.XRefTarget_enumerator enumerator) = do
  Cxx.Enumerator_key name _decl range <- Glean.keyOf enumerator
  nameStr <- Glean.keyOf name
  let entity = Code.Entity_cxx (Cxx.Entity_enumerator enumerator)
  let location = Code.Location {
      Code.location_name = nameStr,
      Code.location_file = Src.range_file range,
      Code.location_location = Code.RangeSpan_range range,
      Code.location_destination = Nothing
    }
  return $ Just (entity, location)

cxxXRefTargetToLocation _ (Cxx.XRefTarget_objcSelectorSlot slot) =
  return $ objcSelectorSlotLocation $
    Cxx.ObjcSelectorSlotEntity (Cxx.ObjcMethodEntity_decl method) idx
  where
    (Cxx.ObjcSelectorSlot method idx) = slot

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

-- Spelling XRefs associated with a file
cxxFileEntitySpellingXRefLocations
  :: Glean.IdOf Src.File
  -> Angle XRef
cxxFileEntitySpellingXRefLocations fileId =
  vars $ \(xref :: Angle Code.XRefLocation) (entity :: Angle Cxx.Entity) ->
    tuple (xref, sig (alt @"cxx" entity) :: Angle Code.Entity) `where_` [
      wild .= predicate @Code.CxxFileEntitySpellingXRefLocations (
        rec $
          field @"file" (asPredicate (factId fileId)) $
          field @"xref" xref $
          field @"entity" entity
        end)
      ]

-- Fixed XRefs associated with a file and xmap /xref set
cxxFileEntityXMapFixedXRefLocations
  :: Glean.IdOf Cxx.FileXRefs
  -> Angle XRef
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
  -> Angle ([Cxx.From], [Cxx.XRefTargets])
cxxFileEntityXMapVariableXRefLocations xrefId =
  vars $ \(froms :: Angle [Cxx.From]) (targets :: Angle [Cxx.XRefTargets]) ->
    tuple (froms, targets) `where_` [
      factId xrefId .= predicate @Cxx.FileXRefs (
        rec $
          field @"xmap" (rec $ field @"froms" froms end) $
          field @"targets" targets
        end
      )
    ]

--
-- Defn xref targets computed from file Decls
-- N.B. won't contain indirect decls. Just the first level Decls
--
cxxFileEntityTraceDeclToDefXRefLocations
  :: Glean.IdOf Cxx.Trace
  -> Angle XRef
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
  -> Angle (Src.Range, Text, Src.File, Src.ByteSpan)
cxxDeclarationLocation decl =
  vars $ \(source :: Angle Src.Range)
          (name :: Angle Text)
          (file :: Angle Src.File)
          (span :: Angle Src.ByteSpan) ->
  tuple (source, name, file, span) `where_` [
    wild .= predicate @Cxx.DeclarationLocationNameSpan (
      rec $
        field @"decl" decl $
        field @"source" source $
        field @"name" name $
        field @"file" (asPredicate file) $
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
  -> Angle XRef
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
    Just traceQ ->
      fst <$> searchRecursiveWithLimit mlimit (ppXRefFileLocations traceQ)
  where
    -- arbitrary number of fact traces to be reasonable. More than 1. But
    -- don't overwhelm things. c.f PPTrace { file = folly/Optional.h }
    -- also the query gets quite large
    traceMaxLimit = 10

usrHashToDeclaration
  :: Text -> Glean.RepoHaxl u w (Maybe (Code.Location, Code.Entity))
usrHashToDeclaration usrhash = fetchDataRecursive (usrToDeclaration usrhash)

-- | Resolve USR hash to its definition site
usrToDeclaration :: Text -> Angle (Code.Location, Code.Entity)
usrToDeclaration usrhash =
  vars $ \(decl :: Angle Cxx.Declaration) (loc :: Angle Code.Location)
      (defn :: Angle Cxx.Definition) ->
    tuple (loc, sig (alt @"cxx" (alt @"defn" defn)) :: Angle Code.Entity)
      `where_` [
        wild .= predicate @Cxx.USRToDeclaration (
          rec $
            field @"hash" (string usrhash) $
            field @"declaration" decl
          end),
        wild .= predicate @Cxx.DeclToDef (
          rec $
            field @"decl" decl $
            field @"defn" defn
          end),
        wild .= predicate @Code.CxxEntityLocation (
          rec $
            field @"entity"  (alt @"defn" defn) $
            field @"location" loc
          end)
      ]

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
factIdQuery :: Type t => Angle t -> Angle (t, ())
factIdQuery p = var $ \r ->
  tuple (r, sig unit) `where_` [ r .= p ]

usrHashToXRefs
  :: Maybe Int
    -> Text
    -> Glean.RepoHaxl u w [(Src.File, Code.RangeSpan) ]
usrHashToXRefs n usrhash = searchWithLimit n (usrToXref usrhash)

-- | Return all reference locations of the symbol that corresponds to USR hash.
--
-- Equivalent to a findReferenceRanges but accepts a USR instead of a glean
-- symbol id
usrToXref :: Text -> Angle ( Src.File, Code.RangeSpan)
usrToXref usrhash =
  vars $ \(reffile :: Angle Src.File) (rangespan :: Angle Code.RangeSpan)
          (decl :: Angle Cxx.Declaration) ->
    tuple (reffile, rangespan) `where_` [
      wild .= predicate @Cxx.USRToDeclaration (
        rec $
          field @"hash" (string usrhash) $
          field @"declaration" decl
        end),
      wild .= predicate @Code.EntityReferences (
      rec $
          field @"target" (alt @"cxx" (alt @"decl" decl)) $
          field @"file" (asPredicate reffile) $
          field @"range" rangespan
        end
      )
    ]
