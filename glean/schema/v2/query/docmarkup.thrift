// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/query/builtin.thrift"
include "glean/schema/v2/query/code.thrift"
include "glean/schema/v2/query/hack.thrift"
include "glean/schema/v2/query/java.thrift"
include "glean/schema/v2/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.docmarkup
namespace hs Glean.Schema.Query
namespace php glean_schema_query_docmarkup
namespace py glean.schema.query.docmarkup
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.docmarkup
namespace rust glean_schema_query_docmarkup

hs_include "glean/schema/v2/query/docmarkup_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "EntityAnnotations": 1,
  "EntityComments": 1,
  "DocAttr": 1,
  "DocAttrKey": 1,
  "EntityDocAttr": 1,
  "EntityByDocAttrKey": 1,
}


typedef glean.Id EntityComments_id

@glean.PredicateAnnotation{
  name="docmarkup.EntityComments";
  version=1;
}
union EntityComments {
  1: EntityComments_id id (hs.strict);
  2: EntityComments_key key;
  3: builtin.Unit get;
} (hs.prefix = "EntityComments_with_")

typedef glean.Id EntityByDocAttrKey_id

@glean.PredicateAnnotation{
  name="docmarkup.EntityByDocAttrKey";
  version=1;
}
union EntityByDocAttrKey {
  1: EntityByDocAttrKey_id id (hs.strict);
  2: EntityByDocAttrKey_key key;
  3: builtin.Unit get;
} (hs.prefix = "EntityByDocAttrKey_with_")

typedef glean.Id EntityDocAttr_id

@glean.PredicateAnnotation{
  name="docmarkup.EntityDocAttr";
  version=1;
}
union EntityDocAttr {
  1: EntityDocAttr_id id (hs.strict);
  2: code.Entity key;
  3: builtin.Unit get;
} (hs.prefix = "EntityDocAttr_with_")

typedef glean.Id EntityAnnotations_id

@glean.PredicateAnnotation{
  name="docmarkup.EntityAnnotations";
  version=1;
}
union EntityAnnotations {
  1: EntityAnnotations_id id (hs.strict);
  2: EntityAnnotations_key key;
  3: builtin.Unit get;
} (hs.prefix = "EntityAnnotations_with_")

typedef glean.Id DocAttrKey_id

@glean.PredicateAnnotation{
  name="docmarkup.DocAttrKey";
  version=1;
}
union DocAttrKey {
  1: DocAttrKey_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "DocAttrKey_with_")

typedef glean.Id DocAttr_id

@glean.PredicateAnnotation{
  name="docmarkup.DocAttr";
  version=1;
}
union DocAttr {
  1: DocAttr_id id (hs.strict);
  2: DocAttr_key key;
  3: builtin.Unit get;
} (hs.prefix = "DocAttr_with_")

struct EntityComments_key {
  1: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EntityByDocAttrKey_key {
  1: optional DocAttrKey key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union DocAttrs {
  1: DocAttr every;
  2: list<DocAttr> exact;
}

typedef DocAttrs EntityDocAttr_value

union GeneralAnnotations_hack__array {
  1: hack.UserAttribute every;
  2: list<hack.UserAttribute> exact;
}

union GeneralAnnotations_java__array {
  1: java.Annotation every;
  2: list<java.Annotation> exact;
}

struct GeneralAnnotations {
  1: optional DocAttrs doc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional GeneralAnnotations_hack__array hack (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional GeneralAnnotations_java__array java (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct EntityAnnotations_key {
  1: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional GeneralAnnotations annotations (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef string DocAttrValue

struct DocAttr_key {
  1: optional DocAttrKey key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DocAttrValue value;
}
