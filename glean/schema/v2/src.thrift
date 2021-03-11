// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/builtin.thrift"

namespace cpp2 facebook.glean.schema.src
namespace hs Glean.Schema
namespace php glean_schema_src
namespace py glean.schema.src
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.src
namespace rust glean_schema_src

hs_include "glean/schema/v2/src_include.hs"
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
struct FileLines {
  1: FileLines_id id (hs.strict);
  2: optional FileLines_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileLanguage_id

@glean.PredicateAnnotation{
  name="src.FileLanguage";
  version=1;
}
struct FileLanguage {
  1: FileLanguage_id id (hs.strict);
  2: optional FileLanguage_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id File_id

@glean.PredicateAnnotation{
  name="src.File";
  version=1;
}
struct File {
  1: File_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ByteSpanContains_id

@glean.PredicateAnnotation{
  name="src.ByteSpanContains";
  version=1;
}
struct ByteSpanContains {
  1: ByteSpanContains_id id (hs.strict);
  2: optional ByteSpanContains_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct RelByteSpan {
  1: glean.Nat offset;
  2: glean.Nat length;
}

struct Range {
  1: File file;
  2: glean.Nat lineBegin;
  3: glean.Nat columnBegin;
  4: glean.Nat lineEnd;
  5: glean.Nat columnEnd;
}

struct Loc {
  1: File file;
  2: glean.Nat line;
  3: glean.Nat column;
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

struct FileLines_key {
  1: File file;
  2: list<glean.Nat> lengths;
  3: bool endsInNewline;
  4: bool hasUnicodeOrTabs;
}

struct FileLanguage_key {
  1: File file;
  2: Language language;
}

typedef list<RelByteSpan> ByteSpans

struct ByteSpan {
  1: glean.Nat start;
  2: glean.Nat length;
}

struct ByteSpanContains_key {
  1: ByteSpan byteSpan;
  2: ByteSpan contains;
}

struct FileLocation {
  1: File file;
  2: ByteSpan span;
}

struct ByteRange {
  1: glean.Nat begin;
  2: glean.Nat end;
}
