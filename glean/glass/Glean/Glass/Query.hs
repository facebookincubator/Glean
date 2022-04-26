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

  -- * Working with XRefs
  , fileEntityLocations
  , fileEntityXRefLocations

  -- * Finding refernces to declarations
  , findReferences
  , findReferenceRangeSpan

  -- * offsets and conversions to lines
  , fileLines

  -- * Search
  , searchByLocalName
  , searchByLocalNamePrefix
  , SearchCase(..)

  -- * Entity annotations
  , symbolKind

  -- * Query helpers
  , entityLocation

  ) where

import Data.Text (Text, toLower)

import qualified Glean
import Glean.Angle as Angle

import Glean.Glass.Base (GleanPath(..))

import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Codemarkup.Types as Code
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.SearchCode.Types as Code

import Thrift.CodegenTypesOnly ( allThriftEnumValues )

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

-- | Find xrefs from this file, and their associated entities.
--
-- > www> src.File "www/flib/intern/glean/Glean.php"
-- > { "id": 885230, "key": "www/flib/intern/glean/Glean.php" }
--
-- > www> {X,E} where
-- >   codemarkup.FileEntityXRefLocations {file = $885230, xref = X, entity = E}
--
fileEntityXRefLocations
  :: Glean.IdOf Src.File -> Angle (Code.XRefLocation, Code.Entity)
fileEntityXRefLocations fileid =
  vars $ \(xref :: Angle Code.XRefLocation) (entity :: Angle Code.Entity) ->
    tuple (xref,entity) `where_` [
      wild .= predicate @Code.FileEntityXRefLocations (
        rec $
          field @"file" (asPredicate (factId fileid)) $
          field @"xref" xref $
          field @"entity" entity
        end)
      ]

-- | Entity-based find-references
findReferences
  :: Angle Code.Entity
  -> Angle (Src.File, Src.ByteSpan)
findReferences ent =
  vars $ \(reffile :: Angle Src.File) (refspan :: Angle Src.ByteSpan) ->
    Angle.tuple (reffile, refspan) `where_` [
      wild .= predicate @Code.EntityUses (
      rec $
          field @"target" ent $
          field @"file" (asPredicate reffile) $
          field @"span" refspan
        end
      )
    ]

-- | Entity-based find-references returning native range or bytespan
-- (Unused, replace findReferences in Handler.fetchSymbolReference*)
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

data SearchType = Exact | Prefix
data SearchCase = Sensitive | Insensitive

searchByLocalName
  :: SearchCase
  -> [Code.SymbolKind]
  -> Text
  -> Angle (Code.Entity, Code.Location, Maybe Code.SymbolKind)
searchByLocalName = searchByLocalName_ Exact

searchByLocalNamePrefix
  :: SearchCase
  -> [Code.SymbolKind]
  -> Text
  -> Angle (Code.Entity, Code.Location, Maybe Code.SymbolKind)
searchByLocalNamePrefix = searchByLocalName_ Prefix

searchByLocalName_
  :: SearchType
  -> SearchCase
  -> [Code.SymbolKind]
  -> Text
  -> Angle (Code.Entity, Code.Location, Maybe Code.SymbolKind)
searchByLocalName_ ty sCase kinds name =
  vars $ \entity loc mKind kind -> tuple (entity, loc, mKind) `where_`
    ([ searchPredicate (stringOrPrefix name') entity
    , wild .= predicate @Code.EntityLocation (
        rec $
            field @"entity" entity $
            field @"location" loc
        end)
    , sig @() wild .= (
      [ not_ [wild .= entityKind entity allKinds]
      , mKind .= sig @(Maybe Code.SymbolKind) nothing
      ]
      `or_` [wild .= entityKind entity kind, mKind .= just kind]
    )
    ] ++ kindFilters entity kinds)
  where
    searchPredicate name entity = case sCase of
      Sensitive -> wild .= predicate @Code.SearchByName (
        -- not sure how to avoid code duplication here
        rec $
            field @"name" name $
            field @"entity" entity
        end
        )
      Insensitive -> wild .= predicate @Code.SearchByLowerCaseName (
        rec $
            field @"name" name $
            field @"entity" entity
        end
        )
    stringOrPrefix = case ty of
      Exact -> string
      Prefix -> stringPrefix

    name' = case sCase of
      Sensitive -> name
      Insensitive -> toLower name

    kindFilters :: Angle Code.Entity -> [Code.SymbolKind] -> [AngleStatement]
    kindFilters _ [] = []
    kindFilters entity (h:t) = [
      wild .= entityKind entity (disjunction h t) ]

    disjunction :: Code.SymbolKind -> [Code.SymbolKind] -> Angle Code.SymbolKind
    disjunction kind [] = enum kind
    disjunction kind (h:t) = enum kind .| disjunction h t

    entityKind
      :: Angle Code.Entity
      -> Angle Code.SymbolKind
      -> Angle Code.EntityKind
    entityKind entity kind = predicate @Code.EntityKind (
      rec $
        field @"entity" entity $
        field @"kind" kind
      end)

    -- HACK: because Angle can't handle wildcards in negation yet (T100361464),
    -- we enumerate all possible values of Code.SymbolKind. This increases the
    -- query size considerably.
    allKinds = let (h:t) = allThriftEnumValues in disjunction h t

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
  -> (Angle Src.File -> Angle Code.RangeSpan -> AngleStatement)
entityLocation entity file rangespan =
  wild .= predicate @Code.EntityLocation (
    rec $
      field @"entity" entity $
      field @"location" (
        rec $
          field @"file" (asPredicate file) $
          field @"location" rangespan
        end)
    end)
