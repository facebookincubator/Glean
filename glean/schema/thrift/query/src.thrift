// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"

namespace cpp2 facebook.glean.schema.query.src
namespace hs Glean.Schema.Query
namespace php glean_schema_query_src
namespace py glean.schema.query.src
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.src
namespace rust glean_schema_query_src

hs_include "glean/schema/thrift/query/src_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "ByteSpanContains": 1,
  "FileLanguage": 1,
  "File": 1,
  "FileLines": 1,
}


typedef glean.Id FileLines_id

@glean.PredicateAnnotation{
  name="src.FileLines";
  version=1;
}
union FileLines {
  1: FileLines_id id (hs.strict);
  2: FileLines_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileLines_with_")

typedef glean.Id FileLanguage_id

@glean.PredicateAnnotation{
  name="src.FileLanguage";
  version=1;
}
union FileLanguage {
  1: FileLanguage_id id (hs.strict);
  2: FileLanguage_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileLanguage_with_")

typedef glean.Id File_id

@glean.PredicateAnnotation{
  name="src.File";
  version=1;
}
union File {
  1: File_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "File_with_")

typedef glean.Id ByteSpanContains_id

@glean.PredicateAnnotation{
  name="src.ByteSpanContains";
  version=1;
}
union ByteSpanContains {
  1: ByteSpanContains_id id (hs.strict);
  2: ByteSpanContains_key key;
  3: builtin.Unit get;
} (hs.prefix = "ByteSpanContains_with_")

struct RelByteSpan {
  1: optional glean.Nat offset;
  2: optional glean.Nat length;
}

struct Range {
  1: optional File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional glean.Nat lineBegin;
  3: optional glean.Nat columnBegin;
  4: optional glean.Nat lineEnd;
  5: optional glean.Nat columnEnd;
}

struct Loc {
  1: optional File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional glean.Nat line;
  3: optional glean.Nat column;
}

enum Language {
  Buck = 0,
  C = 1,
  Cpp = 2,
  Hack = 3,
  Haskell = 4,
  ObjC = 5,
  ObjCpp = 6,
  Python = 7,
  Thrift = 8,
  Java = 9,
  GraphQL = 10
} (hs.nounknown)

union FileLines_lengths_array {
  1: glean.Nat every;
  2: list<glean.Nat> exact;
}

struct FileLines_key {
  1: optional File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional FileLines_lengths_array lengths;
  3: optional bool endsInNewline;
  4: optional bool hasUnicodeOrTabs;
}

struct FileLanguage_key {
  1: optional File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Language language;
}

union ByteSpans {
  1: RelByteSpan every;
  2: list<RelByteSpan> exact;
}

struct ByteSpan {
  1: optional glean.Nat start;
  2: optional glean.Nat length;
}

struct ByteSpanContains_key {
  1: optional ByteSpan byteSpan (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ByteSpan contains (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileLocation {
  1: optional File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ByteRange {
  1: optional glean.Nat begin;
  2: optional glean.Nat end;
}
