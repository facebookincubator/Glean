{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}

module Glean.Glass.Query
  (
  -- * Files
    srcFile
  , fileInfo
  , fileDigests

  -- * Working with XRefs
  , fileEntityLocations
  , fileEntityXRefsGenEntities

  -- * Finding refernces to declarations
  , findReferenceRangeSpan

  -- * Finding references for call hierarchy
  , findReferenceEntities
  , findReferenceEntitiesFast

  -- * Finding source definitions for generated entities
  , generatedEntityToIdlEntity

  -- * offsets and conversions to lines
  , fileLines

  -- * Entity annotations
  , symbolKind

  -- * Query helpers
  , entityLocation

  ) where

import Data.Text (Text)

import qualified Glean
import Glean.Angle as Angle

import Glean.Glass.Base (GleanPath(..))

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.Glass.Types as Glass
import qualified Glean.Schema.Digest.Types as Digest

--
-- Find the id in this db of a source file. We mostly operate on these ids once
-- we have them, to avoid passing strings around.
--
-- > src.File "www/flib/intern/glean/Glean.php"
--
srcFile :: GleanPath -> Angle Src.File
srcFile (GleanPath path) =
  predicate @Src.File $
    string path

-- For a potential filepath, fetch all file metadata
fileInfo :: GleanPath -> Angle Glass.FileInfo
fileInfo (GleanPath path) = predicate @Glass.FileInfo $
  rec $
    field @"file" (string path)
  end

-- | Given a file id, look up the index of line endings
--
-- Line ending tables are needed to do line:col conversions, but we want to
-- minimize when we have to do this conversion. The Range vs Location types in
-- the .thrift API capture when this is needed.
--
fileLines :: Glean.IdOf Src.File -> Angle Src.FileLines
fileLines fileid =
  predicate @Src.FileLines (
    rec $ field @"file"
      (asPredicate (factId fileid))
    end)

--
-- Bulk fetch all digests for a set of filepath facts. Non-recursive, as the
-- Digest type will expand. Extremely cheap
--
-- > digest.FileDigest { file = [$371971 : src.File, $61059][..] }
--
fileDigests :: [Glean.IdOf Src.File] -> Angle Digest.FileDigest
fileDigests fileIds =
  predicate @Digest.FileDigest (
    rec $ field @"file"
      (asPredicate (elementsOf (factIdsArray fileIds)))
    end)

-- | Get the definition entities defined in this file, and their declaration
-- sites.
--
-- > www> src.File "www/flib/intern/glean/Glean.php"
-- { "id": 123968960, "key": "www/flib/intern/glean/Glean.php" }
--
-- > {L , E} where
-- >  codemarkup.FileEntityLocations {$123968960, L, E}
--
fileEntityLocations :: Glean.IdOf Src.File -> Angle (Code.Location, Code.Entity)
fileEntityLocations fileid =
  vars $ \(location :: Angle Code.Location) (entity :: Angle Code.Entity) ->
    tuple (location,entity) `where_` [
      wild .= predicate @Code.FileEntityLocations (
        rec $
          field @"file" (asPredicate (factId fileid)) $
          field @"location" location $
          field @"entity" entity
        end)
      ]

-- | Find all "generic" xrefs from this file, regular and xlang
fileEntityXRefsGenEntities
 :: Glean.IdOf Src.File
 -> Bool
 -> Angle Code.GenericEntity
fileEntityXRefsGenEntities fileid includeXRefs =
   vars $ \(genEntity :: Angle Code.GenericEntity) ->
    genEntity `where_` (
      stmt (predicate @Code.FileXRefsGenericEntities (
        rec $
          field @"file" (asPredicate (factId fileid)) $
          field @"genEntity" genEntity
        end)) : [genEntity .= alt @"plainEntity" wild | not includeXRefs])

-- | Entity-based find-references returning native range or bytespan
findReferenceRangeSpan
  :: Angle Code.Entity
  -> Angle (Src.File, Code.RangeSpan)
findReferenceRangeSpan ent =
  vars $ \(reffile :: Angle Src.File) (rangespan :: Angle Code.RangeSpan) ->
    Angle.tuple (reffile, rangespan) `where_` [
      wild .= predicate @Code.EntityReferences (
      rec $
          field @"target" ent $
          field @"file" (asPredicate reffile) $
          field @"range" rangespan
        end
      )
    ]

generatedEntityToIdlEntity
  :: Angle Code.Entity
  -> Angle (Code.Entity, Src.File)
generatedEntityToIdlEntity entity =
  vars $ \(idlEntity :: Angle Code.Entity) (file :: Angle Src.File) ->
    tuple (idlEntity, file) `where_` [
      wild .=
        predicate @Code.GeneratedEntityToIdlEntity (
          rec $
            field @"entity" entity $
            field @"idlEntity" (rec $
                field @"entity" (just idlEntity) $
                field @"file" (asPredicate file)
              end)
          end)
    ]

-- | Entity-based find-references for call hierarchy.
--   Returns referencing entities with their location, and the call site
--   Beware: O(NM) on the number of referencing entites
--           and the xrefs in their source files
findReferenceEntities
  :: Angle Code.Entity
  -> Angle (Src.File, Code.Entity, Code.Location, Code.RangeSpan)
findReferenceEntities ent =
  vars $ \(reffile :: Angle Src.File)
          (caller :: Angle Code.Entity)
          (referenceRangespan :: Angle Code.RangeSpan)
          (callerLocation :: Angle Code.Location)
          ->
    Angle.tuple (reffile, caller, callerLocation, referenceRangespan) `where_`
    [ wild .= predicate @Code.ReferencingEntity (
      rec $
          field @"target" ent $
          field @"referrer" caller $
          field @"reference_file" (asPredicate reffile) $
          field @"reference_range" referenceRangespan $
          field @"referrer_location" callerLocation
        end
      )
    , wild .= predicate @Code.FileEntityKinds (
        rec $
          field @"entity" caller $
          field @"file" (asPredicate reffile) $
          field @"kind"
            (enum Code.SymbolKind_Constructor .|
             enum Code.SymbolKind_Function .|
             enum Code.SymbolKind_Macro .|
             enum Code.SymbolKind_Method
            )
        end
    )
    ]

-- | Like 'findReferenceEntities' but faster
--   Available only for fbsource and www.hack
--   Does not return call site locations
findReferenceEntitiesFast
  :: Angle Code.Entity -> Angle (Code.Entity, Code.Location)
findReferenceEntitiesFast target =
  vars $ \(caller :: Angle Code.Entity) (location :: Angle Code.Location) ->
    Angle.tuple (caller, location) `where_`
      [ wild .= predicate @Code.EntitySource (rec $
          field @"target" target $
          field @"source" caller
          end)
      , wild .= predicate @Code.EntityLocation (rec $
          field @"entity" caller $
          field @"location" location
          end)
      ]

-- | Given an entity find the kind associated with it
symbolKind :: Angle Code.Entity -> Angle Code.SymbolKind
symbolKind ent = var $ \kind -> kind `where_`
  [ wild .= predicate @Code.EntityKind (
      rec $
          field @"entity" ent $
          field @"kind" kind
      end)
  ]

-- | Helper to generate entity location footers on entity search queries
-- Takes free file and rangespan variables to bind
entityLocation
  :: Angle Code.Entity
  -> (Angle Src.File -> Angle Code.RangeSpan -> Angle Text -> AngleStatement)
entityLocation entity file rangespan name =
  wild .= predicate @Code.EntityLocation (
    rec $
      field @"entity" entity $
      field @"location" (
        rec $
          field @"name" name $
          field @"file" (asPredicate file) $
          field @"location" rangespan
        end)
    end)
