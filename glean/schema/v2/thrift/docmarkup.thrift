// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/builtin.thrift"
include "glean/schema/v2/thrift/code.thrift"
include "glean/schema/v2/thrift/hack.thrift"
include "glean/schema/v2/thrift/java.thrift"
include "glean/schema/v2/thrift/src.thrift"

namespace cpp2 facebook.glean.schema.docmarkup
namespace hs Glean.Schema
namespace php glean_schema_docmarkup
namespace py glean.schema.docmarkup
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.docmarkup
namespace rust glean_schema_docmarkup

hs_include "glean/schema/v2/thrift/docmarkup_include.hs"
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
struct EntityComments {
  1: EntityComments_id id (hs.strict);
  2: optional EntityComments_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EntityByDocAttrKey_id

@glean.PredicateAnnotation{
  name="docmarkup.EntityByDocAttrKey";
  version=1;
}
struct EntityByDocAttrKey {
  1: EntityByDocAttrKey_id id (hs.strict);
  2: optional EntityByDocAttrKey_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EntityDocAttr_id

@glean.PredicateAnnotation{
  name="docmarkup.EntityDocAttr";
  version=1;
}
struct EntityDocAttr {
  1: EntityDocAttr_id id (hs.strict);
  2: optional code.Entity key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional EntityDocAttr_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EntityAnnotations_id

@glean.PredicateAnnotation{
  name="docmarkup.EntityAnnotations";
  version=1;
}
struct EntityAnnotations {
  1: EntityAnnotations_id id (hs.strict);
  2: optional EntityAnnotations_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DocAttrKey_id

@glean.PredicateAnnotation{
  name="docmarkup.DocAttrKey";
  version=1;
}
struct DocAttrKey {
  1: DocAttrKey_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id DocAttr_id

@glean.PredicateAnnotation{
  name="docmarkup.DocAttr";
  version=1;
}
struct DocAttr {
  1: DocAttr_id id (hs.strict);
  2: optional DocAttr_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EntityComments_key {
  1: code.Entity entity;
  2: src.File file;
  3: src.ByteSpan span;
}

struct EntityByDocAttrKey_key {
  1: DocAttrKey key;
  2: code.Entity entity;
}

typedef list<DocAttr> DocAttrs

typedef DocAttrs EntityDocAttr_value

union GeneralAnnotations {
  1: DocAttrs doc;
  2: list<hack.UserAttribute> hack;
  3: list<java.Annotation> java;
} (hs.nonempty)

struct EntityAnnotations_key {
  1: code.Entity entity;
  2: GeneralAnnotations annotations;
}

typedef string DocAttrValue

struct DocAttr_key {
  1: DocAttrKey key;
  2: DocAttrValue value;
}
