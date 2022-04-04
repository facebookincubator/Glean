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

{-# LANGUAGE DeriveGeneric #-}

module Data.LSIF.Types where

-- types only
import Data.Int ( Int64 )
import Data.Text ( Text )
import Data.Vector ( Vector )
import GHC.Generics ( Generic )

-- | LSIF document facts. Fact with id N is at vector index N+1
newtype LSIF = LSIF (Vector KeyFact)
  deriving (Generic, Show)

-- | A single  "fact" in an LSIF dump, keyed by id
data KeyFact
  = KeyFact {
      id_ :: {-# UNPACK #-}!Id,
      fact :: !Fact
  }
  deriving Show

-- | An Id to identify a vertex or an edge.
newtype Id = Id Int64
  deriving (Generic, Show, Eq)

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
  | LsifMoniker {
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
      documentSymbols :: Vector Id
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
  deriving (Generic, Show, Eq)

data HoverContents
  = HoverSignature {
      hoverLanguage :: !LanguageId,
      value :: !Text
  }
  | HoverText !Text
  deriving (Eq, Show)

data Diagnostic
  = Diagnostic {
      severity :: !Int,
      code :: !Int,
      message :: !Text,
      diagnosticRange :: {-# UNPACK #-}!Range
  }
  deriving (Eq, Show)

data MonikerKind = Export | Local | Import | Implementation
  deriving (Show, Eq)

data Marker = Begin | End
  deriving (Show, Eq)

data Scope = DocumentScope | ProjectScope
  deriving (Show, Eq)

-- | A 0-indexed line/character-offset point in a document
data Position = Position {
      line :: {-# UNPACK #-} !Int64,
      character :: {-# UNPACK #-} !Int64
    }
  deriving (Generic, Eq, Show)

data Range = Range {
      start :: {-# UNPACK #-} !Position,
      end :: {-# UNPACK #-} !Position
    }
  deriving (Generic, Eq, Show)

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
  deriving (Generic, Eq, Show)

-- | Information about the tool that created the dump
data ToolInfo =
  ToolInfo {
    toolName :: !Text,
    toolArgs :: [Text],
    toolVersion :: Maybe Text
  }
  deriving (Generic, Eq, Ord, Show)

data Property = Definitions | References | ReferenceResults
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | LSP symbolKind type (c.f. LSP.Types for similar examples)
-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind
--
-- Should match lsif.SymbolKind
--
data SymbolKind
    = SkFile
    | SkModule
    | SkNamespace
    | SkPackage
    | SkClass
    | SkMethod
    | SkProperty
    | SkField
    | SkConstructor
    | SkEnum
    | SkInterface
    | SkFunction
    | SkVariable
    | SkConstant
    | SkString
    | SkNumber
    | SkBoolean
    | SkArray
    | SkObject
    | SkKey
    | SkNull
    | SkEnumMember
    | SkStruct
    | SkEvent
    | SkOperator
    | SkTypeParameter
    | SkUnknown
    deriving (Generic,Show,Eq,Enum)

-- From https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentItem
-- Text documents have a language identifier to identify a document on the
-- server side when it handles more than one language to avoid re-interpreting
-- the file extension.
--
-- We add a couple of extra languages here
--
data LanguageId
  = ABAP  -- "abap"
  | WindowsBat -- "bat"
  | BibTeX -- "bibtex"
  | Clojure -- "clojure"
  | Coffeescript -- "coffeescript"
  | C -- "c"
  | Cpp -- "cpp"
  | CSharp -- "csharp"
  | CSS -- "css"
  | Diff -- "diff"
  | Dart -- "dart"
  | Dockerfile -- "dockerfile"
  | Elixir -- "elixir"
  | Erlang -- "erlang"
  | FSharp -- "fsharp"
  | Git -- "git-commit" and "git-rebase"
  | Go -- "go"
  | Groovy -- "groovy"
  | Handlebars -- "handlebars"
  | Haskell -- "haskell"
  | HTML -- "html"
  | Ini -- "ini"
  | Java -- "java"
  | JavaScript -- "javascript"
  | JavaScriptReact -- "javascriptreact"
  | JSON -- "json"
  | LaTeX -- "latex"
  | Less -- "less"
  | Lua -- "lua"
  | Makefile -- "makefile"
  | Markdown -- "markdown"
  | ObjectiveC -- "objective-c"
  | ObjectiveCpp -- "objective-cpp"
  | Perl -- "perl"
  | Perl6 -- "perl6"
  | PHP -- "php"
  | Powershell -- "powershell"
  | Pug -- "jade"
  | Python -- "python"
  | R -- "r"
  | Razor -- (cshtml) "razor"
  | Ruby -- "ruby"
  | Rust -- "rust"
  | SCSS -- "scss" (syntax using curly brackets), sass (indented syntax)
  | Scala -- "scala"
  | ShaderLab -- "shaderlab"
  | Shell -- (Bash) "shellscript"
  | SQL -- "sql"
  | Swift -- "swift"
  | TypeScript -- "typescript"
  | TypeScriptReact -- "typescriptreact"
  | TeX -- "tex"
  | VisualBasic -- "vb"
  | XML -- "xml"
  | XSL -- "xsl"
  | YAML -- "yaml"
  | UnknownLanguage
  deriving (Show, Eq, Enum)
