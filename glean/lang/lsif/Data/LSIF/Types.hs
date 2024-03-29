{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- Types for LSIF
-- https://github.com/microsoft/language-server-protocol/blob/main/indexFormat/specification.md
--
-- These represent VS Code LSP-like bulk requests for symbol information
--

module Data.LSIF.Types (
  Fact(..),
  ToolInfo(..),
  Scope(..),
  Marker(..),
  Tag(..),
  LSIF(..),
  KeyFact(..),
  Diagnostic(..),
  HoverContents(..),
  Property(..),
  Label(..),

) where

-- types only
import Data.Text ( Text )
import Data.Vector ( Vector )
import GHC.Generics ( Generic )

import Data.LSIF.Gen ( Id, MonikerKind, Range, SymbolKind, LanguageId )

-- | LSIF document facts. Fact with id N is at vector index N+1
newtype LSIF = LSIF (Vector KeyFact)
  deriving (Generic)

-- | A single  "fact" in an LSIF dump, keyed by id
data KeyFact
  = KeyFact {
      id_ :: {-# UNPACK #-}!Id,
      fact :: !Fact
  }

-- | LSIF records of various sorts. Constructors correspond to labels
-- We flatten edges and vertices here, as they will all be serialized back to
-- Glean facts
data Fact
  --
  -- Verticies
  --
  = MetaData {
      version :: !Text, -- ^ LSIF format version
      projectRoot :: !Text, -- ^ URI of project root
      positionEncoding :: !Text, -- ^ encoding, always utf16
      toolInfo :: Maybe ToolInfo -- ^ info about tool that made the index
    }
  | Project {
      kind :: !LanguageId
  }
  -- begin and end events for documents and project sections
  | Event {
      eventKind :: !Marker,
      scope :: !Scope,
      data_ :: {-# UNPACK #-}!Id
  }
  | Document {
      uri :: !Text,
      language :: !LanguageId
  }
  | HoverResult {
      contents :: Vector HoverContents
  }
  -- Project-level symbol identifiers, usually for imports and exports
  -- These are glass-like symbol ids, identified by opaque text and a scheme
  | Moniker {
      monikerKind :: !MonikerKind,
      scheme :: !Text,
      identifier :: !Text
  }
  | PackageInformation {
      name :: !Text,
      manager :: !Text,
      version :: !Text
  }
  | SymbolRange {
      range :: {-# UNPACK #-}!Range,
      tag :: Maybe Tag
  }
  | ResultSet -- indirection nodes
  | DefinitionResult
  | DeclarationResult
  | ReferenceResult

  | LsifDiagnosticResult {
      diagnostics :: Vector Diagnostic
  }
  | LsifDocumentSymbolResult {
      documentSymbols :: !(Vector Id)
  }
  | LsifUnknown
  --
  -- Edge constructors
  -- Edges are labels + an in and out vertex id
  --
  | Edge {
      label :: !Label,
      outV :: {-# UNPACK #-}!Id,
      inV :: {-# UNPACK #-}!Id
    }
  | Contains {
    outV :: {-# UNPACK #-}!Id,
    inVs :: !(Vector Id)
  }
  | Item {
    outV :: {-# UNPACK #-}!Id,
    inVs :: !(Vector Id),
    document :: {-# UNPACK #-}!Id,
    property :: Maybe Property
  }
  deriving (Generic)

data HoverContents
  = HoverSignature {
      hoverLanguage :: !LanguageId,
      value :: !Text
  }
  | HoverText !Text

data Diagnostic
  = Diagnostic {
      severity :: !Int,
      code :: !Int,
      message :: !Text,
      diagnosticRange :: {-# UNPACK #-}!Range
  }


data Marker = Begin | End

data Scope = DocumentScope | ProjectScope



-- | LSIF tags, very close to LSP method call results
data Tag
  = Definition {
    -- The text covered by the range
      tagText :: !Text,
    -- The kind of the declaration.
      tagKind :: !SymbolKind,
      -- The full range of the declaration not including leading/trailing
      -- whitespace but everything else, e.g comments and code.  The range must
      -- be included in fullRange.
      fullRange :: {-# UNPACK #-}!Range,
      -- Optional detail information for the declaration.
      tagDetail :: Maybe Text
  }
  | Declaration {
    -- The text covered by the range
      tagText :: !Text,
    -- The kind of the declaration.
      tagKind :: !SymbolKind,
      -- The full range of the declaration not including leading/trailing
      -- whitespace but everything else, e.g comments and code.  The range must
      -- be included in fullRange.
      fullRange :: {-# UNPACK #-}!Range,
      -- Optional detail information for the declaration.
      tagDetail :: Maybe Text
  }
  | Reference {
      tagText :: !Text
  }
  | UnknownSymbol {
      tagText :: !Text
  }
  deriving (Generic)

-- | Information about the tool that created the dump
data ToolInfo =
  ToolInfo {
    toolName :: !Text,
    toolArgs :: [Text],
    toolVersion :: Maybe Text
  }
  deriving (Generic)

data Property = Definitions | References | ReferenceResults

data Label
  = EdgeContains
  | EdgeMoniker
  | EdgeNext
  | EdgeNextMoniker
  | EdgePackageInformation
  | EdgeTextDocumentDefinition
  | EdgeTextDocumentDiagnostic
  | EdgeTextDocumentDocumentSymbol
  | EdgeTextDocumentHover
  | EdgeTextDocumentReferences
  | EdgeTextDocumentFoldingRange
  -- added in lsif-go 1.7.x
  | EdgeSourceGraphDocString
  | EdgeSourceGraphDocChildren
