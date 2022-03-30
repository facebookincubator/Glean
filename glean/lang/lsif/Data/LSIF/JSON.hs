{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- Barebones parser for LSIF/JSON dumps to LSIF.Types
-- Examples given in
-- https://github.com/microsoft/language-server-protocol/blob/main/indexFormat/specification.md
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.LSIF.JSON ({-instances -}) where

import Data.Aeson.Types
import Data.Text ( Text )
import qualified Data.Vector as V

import Data.LSIF.Types

instance FromJSON LSIF where
  parseJSON = withArray "LSIF" $ fmap LSIF . V.mapM parseJSON

instance FromJSON KeyFact where
  parseJSON = withObject "LSIF.KeyFact" $ \o -> do
    id_ <- o .: "id"
    type_ <- o .: "type"
    fact <- case (type_::Text) of
      "vertex" -> parseVertex o
      "edge" -> parseEdge o
      _ -> fail $ "FromJSON.KeyFact: unknown object type: " <> show type_
    return KeyFact{..}

instance FromJSON Id
instance ToJSON Id where
  toEncoding = genericToEncoding defaultOptions

parseDiagnostic :: Value -> Parser Diagnostic
parseDiagnostic (Object o) = Diagnostic
  <$> o .: "severity"
  <*> o .: "code"
  <*> o .: "message"
  <*> o .: "range"
parseDiagnostic v = fail ("Unrecognized value in diagnostic results" <> show v)

parseHoverContents :: Value -> Parser HoverContents
parseHoverContents (Object o) = HoverSignature
  <$> o .: "language"
  <*> o .: "value"
parseHoverContents (String s) = pure (HoverText s)
parseHoverContents v = fail ("Unrecognized value in hover contents: " <> show v)

instance FromJSON MonikerKind where
  parseJSON (String str) = case str of
    "export" -> pure Export
    "local" -> pure Local
    "import" -> pure Import
    s -> fail ("FromJSON.MonikerKind: unknown kind: " <> show s)
  parseJSON s = fail ("FromJSON.MonikerKind: unknown kind: " <> show s)

instance FromJSON Marker where
  parseJSON (String str) = case str of
    "begin" -> pure Begin
    "end" -> pure End
    s -> fail ("FromJSON.Marker: unknown marker: " <> show s)
  parseJSON s = fail ("FromJSON.Marker. unknown marker: " <> show s)

instance FromJSON Scope where
  parseJSON (String str) = case str of
    "project" -> pure ProjectScope
    "document" -> pure DocumentScope
    s -> fail ("FromJSON.Scope: unknown scope: " <> show s)
  parseJSON s = fail ("FromJSON.Scope: unknown scope: " <> show s)

-- | lsif dumps are rows of flattened objects, indexed by tags
parseVertex :: Object -> Parser Fact
parseVertex o = do
  label::Text <- o .: "label"
  case label of
    "metaData" -> MetaData
      <$> o .: "version"
      <*> o .: "projectRoot"
      <*> o .: "positionEncoding"
      <*> o .: "toolInfo"
    "project" -> Project
      <$> o .: "kind"
    "$event" -> Event
      <$> o .: "kind"
      <*> o .: "scope"
      <*> o .: "data"
    "range" -> SymbolRange
      <$> (Range <$> o .: "start" <*> o .: "end")
      <*> o .:? "tag"
    "document" -> Document
      <$> o .: "uri"
      <*> o .: "languageId"
    "moniker" -> LsifMoniker
      <$> o .: "kind"
      <*> o .: "scheme"
      <*> o .: "identifier"
    "packageInformation" -> PackageInformation
      <$> o .: "name"
      <*> o .: "manager"
      <*> o .: "version"
    "hoverResult" -> do
      result <- o .: "result"
      cs <- result .: "contents"
      contents <- withArray "hoverResult" (V.mapM parseHoverContents) cs
      return HoverResult{..}

    "resultSet" -> pure ResultSet
    "definitionResult" -> pure DefinitionResult
    "declarationResult" -> pure DeclarationResult
    "referenceResult" -> pure ReferenceResult

    "diagnosticResult" -> do
      rs <- o .: "result"
      LsifDiagnosticResult <$> withArray "diagnosticResult"
        (V.mapM parseDiagnostic) rs

    "documentSymbolResult" -> do
      rs <- o .: "result"
      LsifDocumentSymbolResult <$> withArray "documentSymbols"
        (V.mapM (withObject "documentSymbolId" (.: "id"))) rs

    _ -> pure LsifUnknown

instance FromJSON  Position where

instance FromJSON Range where

instance FromJSON Tag where
  parseJSON (Object o) = do
    lsifType <- o .: "type"
    case lsifType::Text of
      "definition" -> Definition
        <$> o .: "text"
        <*> o .: "kind"
        <*> o .: "fullRange"
        <*> o .:? "tagDetail"
      "declaration" -> Definition
        <$> o .: "text"
        <*> o .: "kind"
        <*> o .: "fullRange"
        <*> o .:? "tagDetail"
      "reference" -> Reference
        <$> o .: "text"
      _ -> UnknownSymbol
        <$> o .: "text"
  parseJSON s = fail ("FromJSON.Tag: unknown tag type: " <> show s)

instance FromJSON ToolInfo where
  parseJSON = withObject "ToolInfo" $ \o -> ToolInfo
    <$> o .: "name"
    <*> o .: "args"
    <*> o .: "version"

parseProperty :: Text -> Parser Property
parseProperty "definitions" = pure Definitions
parseProperty "references" = pure References
parseProperty "referenceResults" = pure ReferenceResults
parseProperty s = fail ("Unknown property: parsePropery: " <> show s)

parseEdgeLabel :: Text -> Parser Label
parseEdgeLabel "moniker" = pure EdgeMoniker
parseEdgeLabel "packageInformation" = pure EdgePackageInformation
parseEdgeLabel "nextMoniker" = pure EdgeNextMoniker
parseEdgeLabel "next" = pure EdgeNext
parseEdgeLabel "textDocument/hover" = pure EdgeTextDocumentHover
parseEdgeLabel "textDocument/definition" = pure EdgeTextDocumentDefinition
parseEdgeLabel "textDocument/references" = pure EdgeTextDocumentReferences
parseEdgeLabel "textDocument/diagnostic" = pure EdgeTextDocumentDiagnostic
parseEdgeLabel "textDocument/documentSymbol" =
  pure EdgeTextDocumentDocumentSymbol
parseEdgeLabel s = fail ("Unknown edge label: parseEdgeLabel: " <> show s)

-- | lsif edges
parseEdge :: Object -> Parser Fact
parseEdge o = do
  label <- o .: "label"
  case label of
    "contains" -> Contains
      <$> o .: "outV"
      <*> o .: "inVs"
    "item" -> Item
      <$> o .: "outV"
      <*> o .: "inVs"
      <*> o .: "document"
      <*> explicitParseFieldMaybe
            (withText "property" parseProperty) o "property"
    s -> Edge -- singleton edge
      <$> parseEdgeLabel s
      <*> o .: "outV"
      <*> o .: "inV"

-- https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind
instance FromJSON SymbolKind where
  parseJSON (Number n) = pure $ case n of
    1 -> SkFile
    2 -> SkModule
    3 -> SkNamespace
    4 -> SkPackage
    5 -> SkClass
    6 -> SkMethod
    7 -> SkProperty
    8 -> SkField
    9 -> SkConstructor
    10 -> SkEnum
    11 -> SkInterface
    12 -> SkFunction
    13 -> SkVariable
    14 -> SkConstant
    15 -> SkString
    16 -> SkNumber
    17 -> SkBoolean
    18 -> SkArray
    19 -> SkObject
    20 -> SkKey
    21 -> SkNull
    22 -> SkEnumMember
    23 -> SkStruct
    24 -> SkEvent
    25 -> SkOperator
    26 -> SkTypeParameter
    _ -> SkUnknown
  parseJSON s = fail ("FromJSON.SymbolKind: " <> show s)

-- From https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocumentItem
instance FromJSON LanguageId where
  parseJSON (String s) = pure $ case s of
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
    "latex" -> LaTeX
    "less" -> Less
    "lua" -> Lua
    "makefile" -> Makefile
    "markdown" -> Markdown
    "objective-c" -> ObjectiveC
    "objective-cpp" -> ObjectiveCpp
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
  parseJSON s = fail ("FromJSON.LanguageId: " <> show s)
