/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "glean/if/glean.thrift"
include "glean/schema/thrift/cxx1.thrift"
include "glean/schema/thrift/hs.thrift"
include "glean/schema/thrift/pp1.thrift"
include "glean/schema/thrift/java.thrift"
include "glean/schema/thrift/src.thrift"

namespace hs Glean
namespace cpp2 facebook.glean.thrift
namespace php glean
namespace py glean.search

struct FileXRef {
  1: string file_name;
  2: optional list<i32> line_nos;
}

union Decl {
  1: cxx1.FunctionDefinition fun_def;
  2: cxx1.FunctionDeclaration fun_decl;
  // Function declarations that are not definitions.
  3: cxx1.RecordDefinition rec_def;
  4: cxx1.RecordDeclaration rec_decl;
  // Record declarations that are not definitions.
  5: hs.FunctionDefinition hs_fun_def;
  // Haskell definitions
  6: cxx1.VariableDeclaration var_decl;
  7: cxx1.EnumDefinition enum_def;
  8: cxx1.EnumDeclaration enum_decl;
  9: cxx1.Enumerator enumerator_def;
  10: pp1.Define macro_decl;
  11: java.ClassDeclaration java_class_decl;
} (hs.nonempty)

struct DeclRefs {
  1: Decl decl;
  2: list<FileXRef> xrefs;
}

struct FindDeclsResult {
  1: list<DeclRefs> results;
}

struct FindDeclsQuery {
  1: string user_id;
  // Unique identifier for the client searching.
  2: string query;
  3: bool refs;
// Fetch cross references.
}

struct FunctionCall {
  1: cxx1.FunctionDeclaration from;
  2: cxx1.FunctionDeclaration via;
  3: cxx1.FunctionDeclaration to;
}

struct Override {
  1: cxx1.FunctionDeclaration base;
  2: cxx1.FunctionDeclaration via;
  3: cxx1.FunctionDeclaration derived;
}

struct SourceVertex {
  1: cxx1.FunctionDeclaration declaration;
  2: list<FunctionCall> called_functions;
  3: list<Override> overriding_methods;
  4: i32 dropped_edges;
}

struct TargetVertex {
  1: cxx1.FunctionDeclaration declaration;
  2: list<FunctionCall> calling_functions;
  3: optional Override overridden_method;
  4: i32 dropped_edges;
}

struct FindLocalGraphResult {
  1: list<SourceVertex> source_vertices;
  2: list<TargetVertex> target_vertices;
}

struct FindLocalGraphQuery {
  1: string user_id;
  // Unique identifier for the client searching.
  2: i64 fun_id;
  3: i32 max_funs;
// Maximal number of declarations in the returned graph.
}

struct SearchQuery {
  1: string query;
  // The query string. This is interpreted in a language-sensitive
  // way, e.g. C++ will interpret foo::bar as a search for symbol
  // foo in namespace bar.
  2: bool case_sensitive = true;
  3: optional list<src.Language> languages;
// if set, limit the search results to these languages
} (hs.prefix = "")
