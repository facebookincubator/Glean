// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.pp1
namespace hs Glean.Schema.Query
namespace php glean_schema_query_pp1
namespace py glean.schema.query.pp1
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.pp1
namespace rust glean_schema_query_pp1

hs_include "glean/schema/thrift/query/pp1_include.hs"
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
union Use {
  1: Use_id id (hs.strict);
  2: Use_key key;
  3: builtin.Unit get;
} (hs.prefix = "Use_with_")

typedef glean.Id Undef_id

@glean.PredicateAnnotation{
  name="pp1.Undef";
  version=1;
}
union Undef {
  1: Undef_id id (hs.strict);
  2: Undef_key key;
  3: builtin.Unit get;
} (hs.prefix = "Undef_with_")

typedef glean.Id Macro_id

@glean.PredicateAnnotation{
  name="pp1.Macro";
  version=1;
}
union Macro {
  1: Macro_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Macro_with_")

typedef glean.Id Include_id

@glean.PredicateAnnotation{
  name="pp1.Include";
  version=1;
}
union Include {
  1: Include_id id (hs.strict);
  2: Include_key key;
  3: builtin.Unit get;
} (hs.prefix = "Include_with_")

typedef glean.Id Define_id

@glean.PredicateAnnotation{
  name="pp1.Define";
  version=1;
}
union Define {
  1: Define_id id (hs.strict);
  2: Define_key key;
  3: builtin.Unit get;
} (hs.prefix = "Define_with_")

struct Use_definition {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Loc just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Use_key {
  1: optional Macro macro (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteRange name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Use_definition definition (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional bool expand;
  5: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Undef_key {
  1: optional Macro macro (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Include_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteRange path (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Define_key {
  1: optional Macro macro (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Range source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
