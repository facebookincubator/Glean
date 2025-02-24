{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-

Helpers for generating lsif angle json. Shared between the scip and lsif
converters

-}

module Data.LSIF.Gen (

    Predicate(..),
    PredicateMap,
    predicate,
    predicateId,
    key,
    text,
    string,
    generateJSON,
    generateSCIPJSON,
    insertPredicateMap,
    factId,
    srcFile,

    -- * more shared types
    LanguageId(..),
    MonikerKind(..),
    Id(..),
    Position(..),
    Range(..),
    SymbolKind(..),
    toRange,

    -- * parsing LanguageIds
    parseLanguage,

    -- * processing sourcegraph symbols
    Suffix,
    kindFromSuffix,
    parseSuffix

  ) where

import Data.Aeson
import Data.Maybe ( mapMaybe )
import Data.Int ( Int64 )
import Data.Aeson.Types ( Pair )
import GHC.Generics ( Generic )
import Data.HashMap.Strict ( HashMap )
import Data.List ( foldl', sortOn )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as V
import Data.List.Split ( chunksOf )

-- A predicate "block", containing multiple facts
data Predicate = Predicate !Text [Value]
  deriving Show

-- | Tracking output by predicate
type PredicateMap = HashMap Text [Value]

-- | An Id to identify a vertex or an edge.
newtype Id = Id Int64
  deriving (Generic, Eq)

-- keep instances handy
instance FromJSON Id
instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions

predicate :: Applicative f => Text -> [Pair] -> f [Predicate]
predicate name facts = pure [Predicate name [object [key facts]]]

predicateId :: Applicative f => Text -> Id -> [Pair] -> f [Predicate]
predicateId name id_ facts =
  pure [Predicate name [object [factId id_, key facts ]]]

key :: KeyValue kv => [Pair] -> kv
key xs = "key" .= object xs

string :: Text -> Value
string s = object [ "key" .= s ]

-- | Helper for src.File facts
srcFile :: Id -> Text -> Predicate
srcFile id_ path = Predicate "src.File" [
    object [ factId id_, "key" .= path ]
  ]

factId :: KeyValue kv => Id -> kv
factId (Id id_) = "id" .= id_

-- | Accumulate predicates
insertPredicateMap :: PredicateMap -> [Predicate] -> PredicateMap
insertPredicateMap = foldl' ins
  where
    ins hm (Predicate name vs) = HashMap.insertWith (++) name vs hm

-- | Given a hashmap keyed by lsif predicate names, emit an array of json
-- pred/facts with one entry per predicate. In case we have very large
-- predicates, we chunk them into smaller top level groups, which makes memory
-- mgmt a bit easier
generateJSON :: PredicateMap -> [Value]
generateJSON = mkGenerateJSON lsifSchemaVersion lsifDependencyOrder

-- | Given a hashmap keyed by scip predicate names, emit an array of json
-- pred/facts with one entry per predicate. In case we have very large
-- predicates, we chunk them into smaller top level groups, which makes memory
-- mgmt a bit easier
generateSCIPJSON :: PredicateMap -> [Value]
generateSCIPJSON = mkGenerateJSON scipSchemaVersion scipDependencyOrder

mkGenerateJSON :: Int -> (Text -> Int) -> PredicateMap -> [Value]
mkGenerateJSON version ordering hm = concat $
    mapMaybe (\k -> gen k <$> HashMap.lookup k hm) keys
  where
    gen k = emitPredicate version . Predicate k
    keys = sortOn ordering (HashMap.keys hm)

-- | Try to be slightly robust about which version of the lsif facts we generate
lsifSchemaVersion :: Int
lsifSchemaVersion = 2

-- | Try to be slightly robust about which version of the lsif facts we generate
scipSchemaVersion :: Int
scipSchemaVersion = 1

-- | We do these in chunks to make parsing simpler
emitPredicate :: Int -> Predicate -> [Value]
emitPredicate version (Predicate name facts) =
  [ object
    [ "predicate" .= text (name <> "." <> Text.pack (show version))
    , "facts" .= Array (V.fromList chunk)
    ]
  | chunk <- chunksOf 10000 facts
  ]

text :: Text -> Value
text = String

--
-- Dependency ordering where we have sharing between facts
--
lsifDependencyOrder :: Text -> Int
lsifDependencyOrder p = case p of
  "lsif.Document" -> 0
  "lsif.Project" -> 0
  "lsif.Range" -> 1
  "lsif.HoverContent" -> 2
  "lsif.Moniker" -> 3
  -- these refer to Range
  "lsif.Definition" -> 10
  "lsif.Declaration" -> 11
  -- these refer to Declaration, Range
  "lsif.Reference" -> 12
  "lsif.DefinitionHover" ->  13
  "lsif.DefinitionUse" -> 14
  "lsif.ProjectDocument" -> 15
  "lsif.DefinitionMoniker" ->  16
  "lsif.DefinitionKind" ->  17
  -- everything else, in the middle
  _ -> 5

--
-- Topological ordering of predicates based on sharing in the indexer json
-- We have to write fact for A before a reference to the factId for A
--
scipDependencyOrder :: Text -> Int
scipDependencyOrder p = case p of
  "src.File" -> 01
  "scip.Range" -> 02
  "scip.Symbol" -> 03
  "scip.LocalName" -> 04
  "scip.Documentation" -> 05
  "scip.FileLanguage" -> 11 -- refers to src.File
  "scip.FileRange" -> 21 -- refers to src.File and scip.Range
  "scip.Definition" -> 31
  "scip.Reference" -> 32
  "scip.SymbolDocumentation" -> 33
  "scip.SymbolName" -> 34
  "scip.SymbolKind" -> 41
  "scip.Metadata" -> 42
  _ -> 100

data MonikerKind = Export | Local | Import | Implementation
  deriving (Enum)

instance FromJSON MonikerKind where
  parseJSON (String str) = case str of
    "export" -> pure Export
    "local" -> pure Local
    "import" -> pure Import
    "implementation" -> pure Implementation
    s -> fail ("FromJSON.MonikerKind: unknown kind: " <> show s)
  parseJSON s = fail ("FromJSON.MonikerKind: unknown kind: " <> show s)

-- | A 0-indexed line/character-offset point in a document
data Position = Position {
      line :: {-# UNPACK #-} !Int64,
      character :: {-# UNPACK #-} !Int64
    }
  deriving (Generic)

instance FromJSON  Position where

data Range = Range {
      start :: {-# UNPACK #-} !Position,
      end :: {-# UNPACK #-} !Position
    }
  deriving (Generic)

instance FromJSON Range where

-- LSIF ranges are 0-indexed, exclusive of end col.
-- We want to store as Glean ranges, 1-indexed, inclusive of end col.
toRange :: Range -> Value
toRange Range{..} =
  let colBegin = character start + 1
       -- n.b. end col should be _inclusive_ of end, and >= col start
      colEnd = max colBegin ((character end + 1) - 1)
  in
    object [
      "lineBegin" .= (line start + 1),
      "columnBegin" .= colBegin,
      "lineEnd" .= (line end + 1),
      "columnEnd" .= colEnd
    ]

--
-- Rudimentary handling of semanticdb encoded symbol data
--
data Suffix
  = SymUnspecifiedSuffix
  | SymPackage
  | SymType
  | SymTerm
  | SymMethod
  | SymTypeParameter
  | SymParameter
  | SymMeta -- anything
  deriving Enum

-- These are all a bit wonky
kindFromSuffix :: Suffix -> SymbolKind
kindFromSuffix s = case s of
  SymUnspecifiedSuffix -> SkUnknown
  SymPackage -> SkPackage
  SymType -> SkClass
  SymTerm -> SkVariable
  SymMethod -> SkMethod
  SymTypeParameter -> SkTypeParameter
  SymParameter -> SkField
  SymMeta  -> SkUnknown

parseSuffix :: Text -> (Text, Suffix)
parseSuffix "" = ("", SymUnspecifiedSuffix)
parseSuffix str = case Text.last str of
  '/' -> (sym, SymPackage)
  '#' -> (sym, SymType)
  '.' -> case Text.last sym of
    ')' -> (sym, SymMethod)
    _'  -> (sym, SymTerm)
  ':' -> (sym, SymMeta)
  ']' -> (sym, SymTypeParameter)
  ')' -> (sym, SymParameter)
  _ -> (str, SymUnspecifiedSuffix)
  where
    sym = Text.init str

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
    deriving (Generic,Enum)

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
  -- extensions from Meta
  | Kotlin
  | OCaml
  deriving (Enum)

-- | Parse language strings from LSIF or SCIP to spec
parseLanguage :: Text -> LanguageId
parseLanguage s = case s of
  "abap" -> ABAP
  "bat" -> WindowsBat
  "bibtex" -> BibTeX
  "clojure" -> Clojure
  "coffeescript" -> Coffeescript
  "c" -> C
  "cpp" -> Cpp
  "csharp" -> CSharp
  "css" -> CSS
  "diff" -> Diff
  "dart" -> Dart
  "dockerfile" -> Dockerfile
  "elixir" -> Elixir
  "erlang" -> Erlang
  "fsharp" -> FSharp
  "git-commit" -> Git
  "git-rebase" -> Git
  "go" -> Go
  "groovy" -> Groovy
  "handlebars" -> Handlebars
  "haskell" -> Haskell
  "html" -> HTML
  "ini" -> Ini
  "java" -> Java
  "javascript" -> JavaScript
  "javascriptreact" -> JavaScriptReact
  "json" -> JSON
  "kotlin" -> Kotlin
  "latex" -> LaTeX
  "less" -> Less
  "lua" -> Lua
  "makefile" -> Makefile
  "markdown" -> Markdown
  "objective-c" -> ObjectiveC
  "objective-cpp" -> ObjectiveCpp
  "ocaml" -> OCaml
  "perl" -> Perl
  "perl6" -> Perl6
  "php" -> PHP
  "powershell" -> Powershell
  "jade" -> Pug
  "python" -> Python
  "r" -> R
  "razor" -> Razor
  "ruby" -> Ruby
  "rust" -> Rust
  "scss" -> SCSS
  "scala" -> Scala
  "shaderlab" -> ShaderLab
  "shellscript" -> Shell
  "sql" -> SQL
  "swift" -> Swift
  "typescript" -> TypeScript
  "typescriptreact" -> TypeScriptReact
  "tex" -> TeX
  "vb" -> VisualBasic
  "xml" -> XML
  "xsl" -> XSL
  "yaml" -> YAML
  _ -> UnknownLanguage
