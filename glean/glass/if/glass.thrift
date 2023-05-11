/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

include "glean/github/if/fb303.thrift"
include "glean/if/index.thrift"

namespace hs Glean
namespace hack GleanGlass
namespace py3 glean
namespace cpp2 glean

// Default ceiling on total items on any individual Glean query
const i32 MAXIMUM_SYMBOLS_QUERY_LIMIT = 10000;

// Hard time ceiling on any individual glean query in ms
const i32 MAXIMUM_QUERY_TIME_LIMIT = 15000;

// request types

// Repositories are referred to by their SCS repo names
typedef string RepoName (hs.newtype)

// The UTF-8 path of a file relative to the source control root
typedef string Path (hs.newtype)

// Unique revision identifier (repo-wide unique id)
typedef string Revision (hs.newtype)

// USR hash (Symbol string from ClangD hashed)
typedef string USR (hs.newtype)

// A line range in the file to restrict the query. start should be <= end, and
// range is inclusive of end.
struct LineRange {
  // 1-based line index
  1: i64 lineBegin (hs.strict);

  // end line index, range is inclusive
  2: i64 lineEnd (hs.strict);
}

// Resolved symbol range in a file, using line/column locators.
// lines and columns are 1-indexed.
struct Range {
  1: i64 lineBegin (hs.strict);
  2: i64 columnBegin (hs.strict);
  3: i64 lineEnd (hs.strict);
  4: i64 columnEnd (hs.strict);
}

// Accurate byte ranges of symbols (can be resolved to Ranges)
struct ByteSpan {
  1: i64 start (hs.strict);
  2: i64 length (hs.strict);
}

// An universal, unresolved symbol location.
struct Location {
  // The repository it is defined in
  1: RepoName repository;

  // the filepath in that repository
  2: Path filepath;

  // unresolved bytespan location in file
  3: ByteSpan span (hs.strict);
}

// An universal, resolved symbol location.
struct LocationRange {
  // The repository it is defined in
  1: RepoName repository;

  // the filepath in that repository
  2: Path filepath;

  // resolved line/column ranges in file
  3: Range range (hs.strict);
}

// Generic request options, supported by most calls
struct RequestOptions {
  // repo-global revision identifier, otherwise latest index
  1: optional Revision revision;

  // maximum results to return.
  2: optional i32 limit;

  // feature flags for internal use
  3: optional FeatureFlags feature_flags;
}

struct FeatureFlags {
  1: optional bool snapshots;
}

// List symbols in a file. Symbols are spans of one or more tokens Glean has
// information on (e.g. references, declarations, ..)
struct DocumentSymbolsRequest {
  // SCS repo name (n.b not old style arcanist)
  1: RepoName repository;

  // UTF-8 path to file in repo relative to source control repo root
  2: Path filepath;

  // Limit results to this line range in file
  3: optional list<LineRange> range;

  // include references?
  4: bool include_refs = true;
}

// response types

// Human-readable opaque, stable, globally unique symbol identifier
typedef string SymbolId (hs.newtype)

// Type of attributes associated with a symbol.
union Attribute {
  1: bool aBool;
  2: i64 aInteger;
  3: double aDouble;
  4: string aString;
  5: list<string> aList;
}

// Symbol attributes, keyed by attribute name
typedef map<string, Attribute> Attributes (hs.newtype)

// For clients that can't process maps, use an assoc list for attributes
struct KeyedAttribute {
  1: string key;
  2: Attribute attribute;
}

// For clients that can't process maps, use an assoc list for attributes
typedef list<KeyedAttribute> AttributeList (hs.newtype)

// Reference symbols. These are use sites that point to their definition
struct ReferenceRangeSymbolX {
  // a symbol id to its definition
  1: SymbolId sym;

  // local line:col spans in this file
  2: Range range (hs.strict);

  // this points to the (unresolved) definition site
  3: LocationRange target;

  // attributes of this reference
  4: AttributeList attributes;

  // this points to an optional name range of the `target` rather than
  // the full definition. the range is assumed to be in the same
  // `repository` and `filepath` as the `target` field.
  5: optional Range targetName;
}

// a definition symbol
struct DefinitionSymbolX {
  // a stable name for the definition
  1: SymbolId sym;

  // the resolved local line:col spans in this file
  2: Range range (hs.strict);

  // attributes of this definition
  3: AttributeList attributes;

  // the resolved local line:col spans of the symbol name in this file
  4: optional Range nameRange (hs.strict);
}

// sometimes we prefer to combine all symbols in a file, for use later
struct SymbolX {
  // A stable name for the definition of this symbol
  1: SymbolId sym;

  // the resolved local line:col spans in this file
  2: Range range (hs.strict);

  // if this is a reference, it will point to its definition
  3: optional LocationRange target;

  // additional metadata associated with the symbol
  4: Attributes attributes;

  // the resolved local line:col spans of the symbol name in this file
  5: optional Range nameRange (hs.strict);

  // if this is a reference, it will point to the name of its definition
  6: optional Range targetName;
}

// Path-based symbol identifer. This is less stable than an SymbolId, and is
// stable only for a given revision. However, it is precise and efficient, in
// that it will uniquely map to an entity in the underlying database, without
// requiring search.
//
// (deprecated)
//
struct SymbolPath {
  // The repository it is defined in
  1: RepoName repository;

  // the filepath in that repository
  2: Path filepath;

  // the resolved local line:col spans in this file
  3: Range range (hs.strict);
}

// A list of known symbols in the file, their locations, and their keys
// with all locations resolved to line/column ranges, and attributes
struct DocumentSymbolListXResult {
  // references that appear in this file
  1: list<ReferenceRangeSymbolX> references;

  // definitions in this file
  2: list<DefinitionSymbolX> definitions;

  // actual revision used for results
  3: Revision revision;

  // was the result truncated either by glean or glass?
  4: bool truncated;
}

// For cursor navigation in a file, it is useful to have a line indexed
// map of symbols (to quickly find token under cursor)
struct DocumentSymbolIndex {
  // all symbols present in this file, 1-indexed by line.
  1: map<i64, list<SymbolX>> symbols;

  // actual revision used for results
  2: Revision revision;

  // count of unique symbols in the map
  3: i64 size (hs.strict);

  // was the result truncated either by glean or glass?
  4: bool truncated;
}

// Generic server exception
exception ServerException {
  1: string message;
}

// Type of abstract identifiers
typedef string Name (hs.newtype)

// A pair of names, usually a scope or qualified name and local identifier
struct QualifiedName {
  1: Name localName;
  2: Name container;
}

// Annotations/Attributes/Decorators/Directives attach metadata to definitions
// in source code. They can be optionally cross-referenced with symbols
// and symbolic names (e.g. if the directive is a class name)
//
struct Annotation {
  1: string source; // the annotation as it appears in the source code
  2: optional SymbolId symbol; // the symbol of the annotation
  3: string name;
}

// Visibility attributes
enum Visibility {
  Public = 20,
  Protected = 30,
  Private = 40,
  Internal = 50,
} (
  hack.attributes = "\GraphQLEnum('GlassVisibility'), \SelfDescriptive, \Oncalls('code_indexing')",
)

// Symbol modifiers (see codmearkup.types.Modifiers for those in Glean)
// nb. upper case for thrift to graphql happiness
enum Modifier {
  ABSTRACT = 1,
  FINAL = 2,
  ASYNC = 3,
  STATIC = 4,
  READONLY = 5,
  CONST = 6,
  MUTABLE = 7,
  VOLATILE = 8,
  VIRTUAL = 9,
  INLINE = 10,
} (
  hack.attributes = "\GraphQLEnum('GlassModifiers'), \SelfDescriptive, \Oncalls('code_indexing')",
)

// A symbol occuring in a type, together with its
// span relative to the signature field
struct TypeSymSpan {
  1: SymbolId type;
  2: ByteSpan span;
}

// A more concise symbol description for common hover/link scenarios
struct SymbolBasicDescription {
  1: SymbolId sym;
  2: QualifiedName name;
  3: optional SymbolKind kind;
  4: Language language;
  5: optional string signature;
}

// A symbol description extends the symbol id with additional attributes
struct SymbolDescription {
  1: SymbolId sym;
  2: SymbolPath location; // deprecated, use sym_location(s)
  3: QualifiedName name;
  4: optional SymbolKind kind;
  5: optional list<Annotation> annotations;
  6: list<LocationRange> comments; // deprecated, use pretty_comments
  7: optional Visibility visibility;
  8: Revision repo_hash;
  9: Language language;
  10: optional string signature;
  11: LocationRange sym_location; // symbol have at least one defining location
  12: list<LocationRange> sym_other_locations; // and optionally extra locations
  13: RelationDescription extends_relation;
  14: RelationDescription contains_relation;
  15: set<Modifier> modifiers;
  16: list<TypeSymSpan> type_xrefs;
  17: list<SymbolComment> pretty_comments; // comment text in markdown format
}

// Processed comment and original span
struct SymbolComment {
  1: LocationRange location;
  2: optional string comment; // comment in doxygen/docblock or markdown format
}

// summary of search related results
struct RelationDescription {
  1: optional SymbolId firstParent;
  2: bool hasMoreParents;
  3: optional SymbolId firstChild;
  4: bool hasMoreChildren;
  5: optional QualifiedName firstParentName;
  6: optional QualifiedName firstChildName;
}

struct SearchContext {
  1: optional RepoName repo_name;
  2: optional Language language;
  4: set<SymbolKind> kinds;
}

// tags for symbol kinds, so clients can distinguish them
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
  Trait = 31,
} (
  hack.attributes = "\GraphQLEnum('GlassSymbolKind'), \RelayFlowEnum, \SelfDescriptive, \Oncalls('code_indexing')",
)

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
  Erlang = 12,
  TypeScript = 13,
  Go = 14,
} (
  hack.attributes = "\GraphQLEnum('GlassLanguage'), \RelayFlowEnum, \SelfDescriptive, \Oncalls('code_indexing')",
)

// Kinds of definitions. E.g. for jump-to-declaration or jump-to-definition
enum DefinitionKind {
  Definition = 1,
  Declaration = 2,
} (
  hack.attributes = "\GraphQLEnum('GlassDefinitionKind'), \SelfDescriptive, \Oncalls('code_indexing')",
)

// What kind of search to conduct
struct SymbolSearchOptions {
  1: bool detailedResults; // fill out detailed metadata for symbol results
  2: bool exactMatch = false; // whole work exact match, or prefix match
  3: bool ignoreCase = false;
  4: bool namespaceSearch = false; // treat query as namespace-delimited search
  5: bool sortResults = false; // attempt to select results evenly across langs
  6: bool feelingLucky = false; // return a unique result or nothing
}

// Search for symbols by string
struct SymbolSearchRequest {
  1: string name;
  2: optional RepoName repo_name; // optional scm repo ("fbsource", "www")
  3: set<Language> language; // optional set of language filters
  4: set<SymbolKind> kinds; // optional set of symbol kind filters
  5: SymbolSearchOptions options; // flavor of search
}

// Core symbol result data. All search results have these
struct SymbolResult {
  1: SymbolId symbol;
  2: LocationRange location; // assumes a single location for this entity
  3: Language language;
  4: optional SymbolKind kind;
  5: string name; // local name of identifier
  6: map<string, double> score; // extensible ranking scores
  7: QualifiedName qname; // full name and parent
  8: optional SymbolContext context; // sym from which this symbol is viewed
}

// Search context for a symbol. Usually the containing parent, or nothing if it
// is a global symbol. For inherited searches this will be a sub-class from
// which the symbol is viewed. I.e. viewed in this context
struct SymbolContext {
  1: SymbolId symbol;
  2: QualifiedName qname;
  3: optional SymbolKind kind;
}

// String search, either core symbol data or with full metadata per symbol
struct SymbolSearchResult {
  1: list<SymbolResult> symbols;
  2: list<SymbolDescription> symbolDetails;
}

// deprecated
struct SearchByNameRequest {
  1: SearchContext context;
  2: string name;
  3: bool detailedResults; // fill out symbol_details in the response
  4: bool ignoreCase = false;
}

// deprecated
struct SearchByNameResult {
  1: list<SymbolId> symbols;
  2: list<SymbolDescription> symbolDetails;
}

struct SearchBySymbolIdResult {
  1: list<SymbolId> symbols;
}

enum RelationType {
  Extends = 1, // OOP inheritance
  Contains = 2, // Syntactically nested (usually)
  Calls = 3, // Callers(Parent) or Callees (Child)
}

enum RelationDirection {
  Parent = 1,
  Child = 2,
}

struct SearchRelatedRequest {
  1: RelationType relatedBy;
  2: RelationDirection relation;
  3: bool recursive; // Not just directly related entities
  4: optional set<SymbolKind> filter; //return only these symbols of these kinds
  5: bool detailedResults; // fill out symbol_details in the response
}

// Some limits to cap how large the neighborhood can get
struct RelatedNeighborhoodRequest {
  1: i32 children_limit = 5000; // max direct children
  2: i32 inherited_limit = 500; // max inherited, per parent
  3: i32 parent_depth = 500; // max count of extend or containing parents
  4: bool hide_uninteresting = false; // whether to drop 'uninteresting' nodes
}

// Consider capping the number of symbols in a single angle query before
// increasing this number
const i32 RELATED_SYMBOLS_MAX_LIMIT = 1000;

// A directed edge in the related symbols graph
struct RelatedSymbols {
  1: SymbolId parent;
  2: SymbolId child;
  // ranges at which this relationship appears, e.g. call sites
  3: optional list<LocationRange> ranges;
}

// Pairs of edges from "parent" to "child" according to relationship
// and an optional table of details for each symbol
struct SearchRelatedResult {
  1: list<RelatedSymbols> edges;
  2: map<string, SymbolDescription> symbolDetails;
}

// Inheritance sets: a parent by `extends` provides a set of things it contains
struct InheritedSymbols {
  1: SymbolId base;
  2: list<SymbolId> provides;
}

// Report of neighborhoood of a symbol in all directions (for API discovery)
// Even if there are no parents or children we guarantee to return details
// of the base symbol. the contains and extends 1st level children redundantly
// track the parent symbol id (even though its known from context)
struct RelatedNeighborhoodResult {
  7: list<SymbolId> childrenContained; // 1st level children, contained
  8: list<SymbolId> childrenExtended; // 1st level children, extends
  9: list<SymbolId> parentsExtended; // 1st level of parents, extends

  3: list<RelatedSymbols> containsParents; // N level path of containing parents
  5: list<InheritedSymbols> inheritedSymbols; // "inherited" children, in scope
  6: map<string, SymbolDescription> symbolDetails; // details for members
  10: map<string, SymbolBasicDescription> symbolBasicDetails; // details of rest
}

# request xref locations (currently just #includes for C++ only)
struct FileIncludeLocationRequest {
  // SCS repo name (e.g. "fbsource")
  1: RepoName repository;
  // UTF-8 path to file in repo relative to source control repo root
  2: Path filepath;
  // depth to resolve xrefs recursively
  3: i32 depth = 2;
}

# simplified ReferenceRangeSymbolX when we just need to know the file of the
# xref and the origin span. Useful for caching/pre-fetching file contents
struct FileXRefTarget {
  1: Path target; // target file only
  2: Range range (hs.strict); // local line:col of use
}

# list of struct rather than map to help out GraphQL
struct FileIncludeXRef {
  1: Path source;
  2: list<FileXRefTarget> includes;
}

# map of source file, to local spans and their target files only
typedef list<FileIncludeXRef> XRefFileList (hs.newtype)

struct FileIncludeLocationResults {
  2: Revision revision; // actual revision used for results
  3: XRefFileList references;
}

# Response to ClangD for what we know about a USR and its target definition
struct USRSymbolDefinition {
  // location of the definition (or fallback to decl)
  2: LocationRange location;
  // just the name target of the definition in same file as location
  4: optional Range nameRange;
  // actual revision used for results
  5: Revision revision;
}

# Response to ClangD for the references we know about a USR. Based off of the `Ref` protobuf in clangd's remote index:
# https://github.com/llvm/llvm-project/blob/93cf9640fa3890aa3a4af8c4bd7c07322548b5e8/clang-tools-extra/clangd/index/remote/Index.proto#L80
# n.b. we remove kind because in glean all references are the same entity with the same kind.
# We will keep it a separate struct instead returning LocationRanges in case of
# future additions to API.
struct USRSymbolReference {
  1: LocationRange location;
}

// Glass symbol service
service GlassService extends fb303.FacebookService {
  // Return a list of symbols in the given file, with attributes
  DocumentSymbolListXResult documentSymbolListX(
    1: DocumentSymbolsRequest request,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Return a line-index map of resolved symbols, useful for cursor lookup
  DocumentSymbolIndex documentSymbolIndex(
    1: DocumentSymbolsRequest request,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Resolve a location span to a concrete line:col range in a file
  Range jumpTo(1: Location reference, 2: RequestOptions options) throws (
    1: ServerException e,
  );

  // Find any uses of a definition, generically
  list<Location> findReferences(
    1: SymbolId symbol,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Find any uses of a definition, resolving all locations to line/col ranges
  list<LocationRange> findReferenceRanges(
    1: SymbolId symbol,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Resolve a symbol id to its definition location range
  LocationRange resolveSymbolRange(
    1: SymbolId symbol,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Return details about a symbol, such as its location or type signature
  SymbolDescription describeSymbol(
    1: SymbolId symbol,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Generic symbol search by string query
  SymbolSearchResult searchSymbol(
    1: SymbolSearchRequest request,
    3: RequestOptions options,
  ) throws (1: ServerException e);

  // Find symbol ids based on the prefix of a full symbol id
  // (e.g. www/php/Gl).
  // (deprecated)
  SearchBySymbolIdResult searchBySymbolId(
    1: SymbolId symbol_prefix,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Search for symbols by a specific relationship (child/parent, inheritance)
  SearchRelatedResult searchRelated(
    1: SymbolId symbol,
    2: RequestOptions options,
    3: SearchRelatedRequest request,
  ) throws (1: ServerException e);

  // Search neighborhood of symbols by all relationships
  RelatedNeighborhoodResult searchRelatedNeighborhood(
    1: SymbolId symbol,
    2: RequestOptions options,
    3: RelatedNeighborhoodRequest request,
  ) throws (1: ServerException e);

  // Special purpose queries

  // Trigger the creation of an incremental database based on file changes
  // relative to an indexed revision
  index.IndexResponse index(1: index.IndexRequest request) throws (
    1: ServerException e,
  );

  // Resolve #include file paths to depth N
  FileIncludeLocationResults fileIncludeLocations(
    1: FileIncludeLocationRequest request,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Resolve declaration USR from ClangD to definition sites
  USRSymbolDefinition clangUSRToDefinition(
    1: USR hash,
    2: RequestOptions options,
  ) throws (1: ServerException e);

  // Resolve declaration USR from ClangD to xref sites
  list<USRSymbolReference> clangUSRToReferenceRanges(
    1: USR hash,
    2: RequestOptions options,
  ) throws (1: ServerException e);
}
