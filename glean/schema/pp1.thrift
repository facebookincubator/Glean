// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/builtin.thrift"
include "glean/schema/src.thrift"

namespace cpp2 facebook.glean.schema.pp1
namespace hs Glean.Schema
namespace php glean_schema_pp1
namespace py glean.schema.pp1
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.pp1
namespace rust glean_schema_pp1

hs_include "glean/schema/pp1_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "Include": 1,
  "Macro": 1,
  "Define": 1,
  "Use": 1,
  "Undef": 1,
}


typedef glean.Id Use_id

@glean.PredicateAnnotation{
  name="pp1.Use";
  version=1;
}
struct Use {
  1: Use_id id (hs.strict);
  2: optional Use_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Undef_id

@glean.PredicateAnnotation{
  name="pp1.Undef";
  version=1;
}
struct Undef {
  1: Undef_id id (hs.strict);
  2: optional Undef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Macro_id

@glean.PredicateAnnotation{
  name="pp1.Macro";
  version=1;
}
struct Macro {
  1: Macro_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Include_id

@glean.PredicateAnnotation{
  name="pp1.Include";
  version=1;
}
struct Include {
  1: Include_id id (hs.strict);
  2: optional Include_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Define_id

@glean.PredicateAnnotation{
  name="pp1.Define";
  version=1;
}
struct Define {
  1: Define_id id (hs.strict);
  2: optional Define_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Use_key {
  1: Macro macro;
  2: src.ByteRange name;
  3: optional src.Loc definition;
  4: bool expand;
  5: src.Range source;
}

struct Undef_key {
  1: Macro macro;
  2: src.Range source;
}

struct Include_key {
  1: src.File file;
  2: src.ByteRange path;
  3: src.Range source;
}

struct Define_key {
  1: Macro macro;
  2: src.Range source;
}
