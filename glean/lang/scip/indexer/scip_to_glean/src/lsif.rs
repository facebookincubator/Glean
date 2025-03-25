/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(clippy::upper_case_acronyms)]

use crate::Suffix;

#[allow(unused)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SymbolKind {
    SkFile,
    SkModule,
    SkNamespace,
    SkPackage,
    SkClass,
    SkMethod,
    SkProperty,
    SkField,
    SkConstructor,
    SkEnum,
    SkInterface,
    SkFunction,
    SkVariable,
    SkConstant,
    SkString,
    SkNumber,
    SkBoolean,
    SkArray,
    SkObject,
    SkKey,
    SkNull,
    SkEnumMember,
    SkStruct,
    SkEvent,
    SkOperator,
    SkTypeParameter,
    SkUnknown,
}
impl SymbolKind {
    pub fn new(value: Suffix) -> Self {
        use Suffix::*;
        use SymbolKind::*;
        match value {
            SymUnspecifiedSuffix => SkUnknown,
            SymPackage => SkPackage,
            SymType => SkClass,
            SymTerm => SkVariable,
            SymMethod => SkMethod,
            SymTypeParameter => SkTypeParameter,
            SymParameter => SkField,
            SymMeta => SkUnknown,
        }
    }
}

// https://www.internalfb.com/code/fbsource/[8d8905791b99]/fbcode/glean/lang/lsif/Data/LSIF/Gen.hs?lines=318
#[derive(Copy, Clone, Debug, Default)]
pub enum LanguageId {
    ABAP,            // "abap"
    WindowsBat,      // "bat"
    BibTeX,          // "bibtex"
    Clojure,         // "clojure"
    Coffeescript,    // "coffeescript"
    C,               // "c"
    Cpp,             // "cpp"
    CSharp,          // "csharp"
    CSS,             // "css"
    Diff,            // "diff"
    Dart,            // "dart"
    Dockerfile,      // "dockerfile"
    Elixir,          // "elixir"
    Erlang,          // "erlang"
    FSharp,          // "fsharp"
    Git,             // "git-commit" and "git-rebase"
    Go,              // "go"
    Groovy,          // "groovy"
    Handlebars,      // "handlebars"
    Haskell,         // "haskell"
    HTML,            // "html"
    Ini,             // "ini"
    Java,            // "java"
    JavaScript,      // "javascript"
    JavaScriptReact, // "javascriptreact"
    JSON,            // "json"
    LaTeX,           // "latex"
    Less,            // "less"
    Lua,             // "lua"
    Makefile,        // "makefile"
    Markdown,        // "markdown"
    ObjectiveC,      // "objective-c"
    ObjectiveCpp,    // "objective-cpp"
    Perl,            // "perl"
    Perl6,           // "perl6"
    PHP,             // "php"
    Powershell,      // "powershell"
    Pug,             // "jade"
    Python,          // "python"
    R,               // "r"
    Razor,           // (cshtml) "razor"
    Ruby,            // "ruby"
    Rust,            // "rust"
    SCSS,            // "scss" (syntax using curly brackets), sass (indented syntax)
    Scala,           // "scala"
    ShaderLab,       // "shaderlab"
    Shell,           // (Bash) "shellscript"
    SQL,             // "sql"
    Swift,           // "swift"
    TypeScript,      // "typescript"
    TypeScriptReact, // "typescriptreact"
    TeX,             // "tex"
    VisualBasic,     // "vb"
    XML,             // "xml"
    XSL,             // "xsl"
    YAML,            // "yaml"
    #[default]
    UnknownLanguage,
    Kotlin,
    OCaml,
}
impl LanguageId {
    pub fn new(s: &str) -> LanguageId {
        use LanguageId::*;
        match s {
            "abap" => ABAP,
            "bat" => WindowsBat,
            "bibtex" => BibTeX,
            "clojure" => Clojure,
            "coffeescript" => Coffeescript,
            "c" => C,
            "cpp" => Cpp,
            "csharp" => CSharp,
            "css" => CSS,
            "diff" => Diff,
            "dart" => Dart,
            "dockerfile" => Dockerfile,
            "elixir" => Elixir,
            "erlang" => Erlang,
            "fsharp" => FSharp,
            "git-commit" => Git,
            "git-rebase" => Git,
            "go" => Go,
            "groovy" => Groovy,
            "handlebars" => Handlebars,
            "haskell" => Haskell,
            "html" => HTML,
            "ini" => Ini,
            "java" => Java,
            "javascript" => JavaScript,
            "javascriptreact" => JavaScriptReact,
            "json" => JSON,
            "kotlin" => Kotlin,
            "latex" => LaTeX,
            "less" => Less,
            "lua" => Lua,
            "makefile" => Makefile,
            "markdown" => Markdown,
            "objective-c" => ObjectiveC,
            "objective-cpp" => ObjectiveCpp,
            "ocaml" => OCaml,
            "perl" => Perl,
            "perl6" => Perl6,
            "php" => PHP,
            "powershell" => Powershell,
            "jade" => Pug,
            "python" => Python,
            "r" => R,
            "razor" => Razor,
            "ruby" => Ruby,
            "rust" => Rust,
            "scss" => SCSS,
            "scala" => Scala,
            "shaderlab" => ShaderLab,
            "shellscript" => Shell,
            "sql" => SQL,
            "swift" => Swift,
            "typescript" => TypeScript,
            "typescriptreact" => TypeScriptReact,
            "tex" => TeX,
            "vb" => VisualBasic,
            "xml" => XML,
            "xsl" => XSL,
            "yaml" => YAML,
            _ => UnknownLanguage,
        }
    }

    pub fn known(self) -> Option<Self> {
        match self {
            LanguageId::UnknownLanguage => None,
            _ => Some(self),
        }
    }
}
