/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

namespace hs Glean
namespace hack GleanGlass
namespace py3 glean
namespace cpp2 glean

include "thrift/annotation/hack.thrift"

typedef string RepoName (hs.newtype)

typedef string Path (hs.newtype)

typedef string Revision (hs.newtype)

struct LineRange {
  1: i64 lineBegin (hs.strict);

  2: i64 lineEnd (hs.strict);
}

struct Range {
  1: i64 lineBegin (hs.strict);
  2: i64 columnBegin (hs.strict);
  3: i64 lineEnd (hs.strict);
  4: i64 columnEnd (hs.strict);
}

struct ByteSpan {
  1: i64 start (hs.strict);
  2: i64 length (hs.strict);
}

struct Location {
  1: RepoName repository;

  2: Path filepath;

  3: ByteSpan span (hs.strict);
}

struct LocationRange {
  1: RepoName repository;

  2: Path filepath;

  3: Range range (hs.strict);
}

struct RequestOptions {
  1: optional Revision revision;

  2: optional i32 limit;
}

struct DocumentSymbolsRequest {
  1: RepoName repository;

  2: Path filepath;

  3: optional list<LineRange> range;
}

typedef string SymbolId (hs.newtype)

union Attribute {
  1: bool aBool;
  2: i64 aInteger;
  3: double aDouble;
  4: string aString;
  5: list<string> aList;
}

typedef map<string, Attribute> Attributes (hs.newtype)

struct KeyedAttribute {
  1: string key;
  2: Attribute attribute;
}

typedef list<KeyedAttribute> AttributeList (hs.newtype)

struct ReferenceRangeSymbolX {
  1: SymbolId sym;

  2: Range range (hs.strict);

  3: LocationRange target;

  4: AttributeList attributes;
}

struct DefinitionSymbolX {
  1: SymbolId sym;

  2: Range range (hs.strict);

  3: AttributeList attributes;
}

struct SymbolX {
  1: SymbolId sym;

  2: Range range (hs.strict);

  3: optional LocationRange target;

  4: Attributes attributes;
}

struct SymbolPath {
  1: RepoName repository;

  2: Path filepath;

  3: Range range (hs.strict);
}

struct DocumentSymbolListXResult {
  1: list<ReferenceRangeSymbolX> references;

  2: list<DefinitionSymbolX> definitions;

  3: Revision revision;
}

struct DocumentSymbolIndex {
  1: map<i64, list<SymbolX>> symbols;

  2: Revision revision;

  3: i64 size (hs.strict);
}

exception ServerException {
  1: string message;
}

typedef string Name (hs.newtype)

struct QualifiedName {
  1: Name localName;
  2: Name container;
}

struct SymbolDescription {
  1: SymbolId sym;
  2: SymbolPath location;
  3: QualifiedName name;
  4: optional SymbolKind kind;
}

struct SearchContext {
  1: RepoName repo_name;
  2: Language language;
  4: set<SymbolKind> kinds;
}

@hack.Attributes{
  attributes = [
    "\GraphQLEnum('GlassSymbolKind')",
    "\RelayFlowEnum",
    "\SelfDescriptive",
    "\Oncalls('code_indexing')",
  ],
}
enum SymbolKind {
  Package = 1,
  Type = 2,
  Value = 3,
  File = 4,
  Module = 5,
  Namespace = 6,
  Class_ = 7,
  Method = 8,
  Property = 9,
  Field = 10,
  Constructor = 11,
  Enum = 12,
  Interface = 13,
  Function = 14,
  Variable = 15,
  Constant = 16,
  String = 17,
  Number = 18,
  Boolean = 19,
  Array = 20,
  Object = 21,
  Key = 22,
  Null = 23,
  Enumerator = 24,
  Struct = 25,
  Event = 26,
  Operator = 27,
  TypeParameter = 28,
  Union = 29,
  Macro = 30,
}

@hack.Attributes{
  attributes = [
    "\GraphQLEnum('GlassLanguage')",
    "\RelayFlowEnum",
    "\SelfDescriptive",
    "\Oncalls('code_indexing')",
  ],
}
enum Language {
  Cpp = 1,
  JavaScript = 2,
  Hack = 3,
  Haskell = 4,
  Java = 5,
  ObjectiveC = 6,
  Python = 7,
  PreProcessor = 8,
  Thrift = 9,
  Rust = 10,
  Buck = 11,
}

@hack.Attributes{
  attributes = [
    "\GraphQLEnum('GlassDefinitionKind')",
    "\SelfDescriptive",
    "\Oncalls('code_indexing')",
  ],
}
enum DefinitionKind {
  Definition = 1,
  Declaration = 2,
}

struct SearchByNameRequest {
  1: SearchContext context;
  2: string name;
}

struct SearchByNameResult {
  1: list<SymbolId> symbols;
  2: list<SymbolDescription> symbolDetails;
}

struct SearchBySymbolIdResult {
  1: list<SymbolId> symbols;
}

service ZeroService {
}

service BaseService extends ZeroService {
}

service GlassService extends BaseService {
  DocumentSymbolListXResult documentSymbolListX(
    1: DocumentSymbolsRequest request,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  DocumentSymbolIndex documentSymbolIndex(
    1: DocumentSymbolsRequest request,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  Range jumpTo(1: Location reference, 2: RequestOptions options) throws (
    1: ServerException e,
  );

  list<Location> findReferences(
    1: SymbolId symbol,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  list<LocationRange> findReferenceRanges(
    1: SymbolId symbol,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  Location resolveSymbol(1: SymbolId symbol, 2: RequestOptions options) throws (
    1: ServerException e,
  );

  SymbolDescription describeSymbol(
    1: SymbolId symbol,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  SearchByNameResult searchByName(
    1: SearchByNameRequest request,
    3: RequestOptions options,
  ) throws (1: ServerException e);

  SearchByNameResult searchByNamePrefix(
    1: SearchByNameRequest request,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  SearchBySymbolIdResult searchBySymbolId(
    1: SymbolId symbol_prefix,
    2: RequestOptions options,
  ) throws (1: ServerException e);
}
