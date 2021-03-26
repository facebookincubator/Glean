// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/query/builtin.thrift"
include "glean/schema/thrift/query/code_cxx.thrift"
include "glean/schema/thrift/query/pp1.thrift"
include "glean/schema/thrift/query/sys.thrift"

namespace cpp2 facebook.glean.schema.query.glean.test
namespace hs Glean.Schema.Query
namespace php glean_schema_query_glean_test
namespace py glean.schema.query.glean_test
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.glean_test
namespace rust glean_schema_query_glean_test

hs_include "glean/schema/thrift/query/glean_test_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "RevStringPair": 1,
  "LeftOr": 1,
  "Tree": 4,
  "StoredRevStringPairWithA": 1,
  "Ref": 4,
  "Bar": 4,
  "DerivedKeyValue": 1,
  "ViaStringPair": 1,
  "LeftOr2": 1,
  "StringPair": 1,
  "Name": 1,
  "StringPairBox": 1,
  "ReflStringPair": 1,
  "StoredRevStringPair": 1,
  "RevStringPairs": 1,
  "RefRef": 4,
  "DerivedKeyValue2": 1,
  "nothingTest": 4,
  "FooToFoo": 4,
  "Edge": 4,
  "IsGlean": 1,
  "Foo": 4,
  "MatchOneAlt": 1,
  "Predicate": 4,
  "Predicate_1": 1,
  "Unbound": 1,
  "IsThree": 1,
  "Qux": 4,
  "KeyValue": 1,
  "SameString": 1,
  "Expr": 1,
  "RevRevStringPair": 1,
  "RevStringPairRec": 1,
  "Node": 4,
  "Unbound2": 1,
  "TreeToTree": 4,
  "DualStringPair": 1,
}


typedef glean.Id nothingTest_id

@glean.PredicateAnnotation{
  name="glean.test.nothingTest";
  version=4;
}
union nothingTest {
  1: nothingTest_id id (hs.strict);
  2: nothingTest_key key;
  3: builtin.Unit get;
} (hs.prefix = "nothingTest_with_")

typedef glean.Id ViaStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.ViaStringPair";
  version=1;
}
union ViaStringPair {
  1: ViaStringPair_id id (hs.strict);
  2: ViaStringPair_key key;
  3: builtin.Unit get;
} (hs.prefix = "ViaStringPair_with_")

typedef glean.Id Unbound2_id

@glean.PredicateAnnotation{
  name="glean.test.Unbound2";
  version=1;
}
union Unbound2 {
  1: Unbound2_id id (hs.strict);
  2: Unbound2_key key;
  3: builtin.Unit get;
} (hs.prefix = "Unbound2_with_")

typedef glean.Id Unbound_id

@glean.PredicateAnnotation{
  name="glean.test.Unbound";
  version=1;
}
union Unbound {
  1: Unbound_id id (hs.strict);
  2: Unbound_key key;
  3: builtin.Unit get;
} (hs.prefix = "Unbound_with_")

typedef glean.Id TreeToTree_id

@glean.PredicateAnnotation{
  name="glean.test.TreeToTree";
  version=4;
}
union TreeToTree {
  1: TreeToTree_id id (hs.strict);
  2: Tree key;
  3: builtin.Unit get;
} (hs.prefix = "TreeToTree_with_")

typedef glean.Id Tree_id

@glean.PredicateAnnotation{
  name="glean.test.Tree";
  version=4;
}
union Tree {
  1: Tree_id id (hs.strict);
  2: Tree_key key;
  3: builtin.Unit get;
} (hs.prefix = "Tree_with_")

typedef glean.Id StringPairBox_id

@glean.PredicateAnnotation{
  name="glean.test.StringPairBox";
  version=1;
}
union StringPairBox {
  1: StringPairBox_id id (hs.strict);
  2: StringPairBox_key key;
  3: builtin.Unit get;
} (hs.prefix = "StringPairBox_with_")

typedef glean.Id StringPair_id

@glean.PredicateAnnotation{
  name="glean.test.StringPair";
  version=1;
}
union StringPair {
  1: StringPair_id id (hs.strict);
  2: StringPair_key key;
  3: builtin.Unit get;
} (hs.prefix = "StringPair_with_")

typedef glean.Id StoredRevStringPairWithA_id

@glean.PredicateAnnotation{
  name="glean.test.StoredRevStringPairWithA";
  version=1;
}
union StoredRevStringPairWithA {
  1: StoredRevStringPairWithA_id id (hs.strict);
  2: StoredRevStringPairWithA_key key;
  3: builtin.Unit get;
} (hs.prefix = "StoredRevStringPairWithA_with_")

typedef glean.Id StoredRevStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.StoredRevStringPair";
  version=1;
}
union StoredRevStringPair {
  1: StoredRevStringPair_id id (hs.strict);
  2: StoredRevStringPair_key key;
  3: builtin.Unit get;
} (hs.prefix = "StoredRevStringPair_with_")

typedef glean.Id SameString_id

@glean.PredicateAnnotation{
  name="glean.test.SameString";
  version=1;
}
union SameString {
  1: SameString_id id (hs.strict);
  2: SameString_key key;
  3: builtin.Unit get;
} (hs.prefix = "SameString_with_")

typedef glean.Id RevStringPairs_id

@glean.PredicateAnnotation{
  name="glean.test.RevStringPairs";
  version=1;
}
union RevStringPairs {
  1: RevStringPairs_id id (hs.strict);
  2: RevStringPairs_key key;
  3: builtin.Unit get;
} (hs.prefix = "RevStringPairs_with_")

typedef glean.Id RevStringPairRec_id

@glean.PredicateAnnotation{
  name="glean.test.RevStringPairRec";
  version=1;
}
union RevStringPairRec {
  1: RevStringPairRec_id id (hs.strict);
  2: RevStringPairRec_key key;
  3: builtin.Unit get;
} (hs.prefix = "RevStringPairRec_with_")

typedef glean.Id RevStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.RevStringPair";
  version=1;
}
union RevStringPair {
  1: RevStringPair_id id (hs.strict);
  2: RevStringPair_key key;
  3: builtin.Unit get;
} (hs.prefix = "RevStringPair_with_")

typedef glean.Id RevRevStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.RevRevStringPair";
  version=1;
}
union RevRevStringPair {
  1: RevRevStringPair_id id (hs.strict);
  2: RevRevStringPair_key key;
  3: builtin.Unit get;
} (hs.prefix = "RevRevStringPair_with_")

typedef glean.Id ReflStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.ReflStringPair";
  version=1;
}
union ReflStringPair {
  1: ReflStringPair_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ReflStringPair_with_")

typedef glean.Id RefRef_id

@glean.PredicateAnnotation{
  name="glean.test.RefRef";
  version=4;
}
union RefRef {
  1: RefRef_id id (hs.strict);
  2: Ref key;
  3: builtin.Unit get;
} (hs.prefix = "RefRef_with_")

typedef glean.Id Ref_id

@glean.PredicateAnnotation{
  name="glean.test.Ref";
  version=4;
}
union Ref {
  1: Ref_id id (hs.strict);
  2: Predicate key;
  3: builtin.Unit get;
} (hs.prefix = "Ref_with_")

typedef glean.Id Qux_id

@glean.PredicateAnnotation{
  name="glean.test.Qux";
  version=4;
}
union Qux {
  1: Qux_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Qux_with_")

typedef glean.Id Node_id

@glean.PredicateAnnotation{
  name="glean.test.Node";
  version=4;
}
union Node {
  1: Node_id id (hs.strict);
  2: Node_key key;
  3: builtin.Unit get;
} (hs.prefix = "Node_with_")

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="glean.test.Name";
  version=1;
}
union Name {
  1: Name_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Name_with_")

typedef glean.Id MatchOneAlt_id

@glean.PredicateAnnotation{
  name="glean.test.MatchOneAlt";
  version=1;
}
union MatchOneAlt {
  1: MatchOneAlt_id id (hs.strict);
  2: MatchOneAlt_key key;
  3: builtin.Unit get;
} (hs.prefix = "MatchOneAlt_with_")

typedef glean.Id LeftOr2_id

@glean.PredicateAnnotation{
  name="glean.test.LeftOr2";
  version=1;
}
union LeftOr2 {
  1: LeftOr2_id id (hs.strict);
  2: LeftOr2_key key;
  3: builtin.Unit get;
} (hs.prefix = "LeftOr2_with_")

typedef glean.Id LeftOr_id

@glean.PredicateAnnotation{
  name="glean.test.LeftOr";
  version=1;
}
union LeftOr {
  1: LeftOr_id id (hs.strict);
  2: LeftOr_key key;
  3: builtin.Unit get;
} (hs.prefix = "LeftOr_with_")

typedef glean.Id KeyValue_id

@glean.PredicateAnnotation{
  name="glean.test.KeyValue";
  version=1;
}
union KeyValue {
  1: KeyValue_id id (hs.strict);
  2: KeyValue_key key;
  3: builtin.Unit get;
} (hs.prefix = "KeyValue_with_")

typedef glean.Id IsThree_id

@glean.PredicateAnnotation{
  name="glean.test.IsThree";
  version=1;
}
union IsThree {
  1: IsThree_id id (hs.strict);
  2: glean.Nat key;
  3: builtin.Unit get;
} (hs.prefix = "IsThree_with_")

typedef glean.Id IsGlean_id

@glean.PredicateAnnotation{
  name="glean.test.IsGlean";
  version=1;
}
union IsGlean {
  1: IsGlean_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "IsGlean_with_")

typedef glean.Id FooToFoo_id

@glean.PredicateAnnotation{
  name="glean.test.FooToFoo";
  version=4;
}
union FooToFoo {
  1: FooToFoo_id id (hs.strict);
  2: Foo key;
  3: builtin.Unit get;
} (hs.prefix = "FooToFoo_with_")

typedef glean.Id Foo_id

@glean.PredicateAnnotation{
  name="glean.test.Foo";
  version=4;
}
union Foo {
  1: Foo_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Foo_with_")

typedef glean.Id Expr_id

@glean.PredicateAnnotation{
  name="glean.test.Expr";
  version=1;
}
union Expr {
  1: Expr_id id (hs.strict);
  2: Expr_key key;
  3: builtin.Unit get;
} (hs.prefix = "Expr_with_")

typedef glean.Id Predicate_1_id

@glean.PredicateAnnotation{
  name="glean.test.Predicate";
  version=1;
}
union Predicate_1 {
  1: Predicate_1_id id (hs.strict);
  2: KitchenSink_1 key;
  3: builtin.Unit get;
} (hs.prefix = "Predicate_1_with_")

typedef glean.Id Edge_id

@glean.PredicateAnnotation{
  name="glean.test.Edge";
  version=4;
}
union Edge {
  1: Edge_id id (hs.strict);
  2: Edge_key key;
  3: builtin.Unit get;
} (hs.prefix = "Edge_with_")

typedef glean.Id DualStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.DualStringPair";
  version=1;
}
union DualStringPair {
  1: DualStringPair_id id (hs.strict);
  2: DualStringPair_key key;
  3: builtin.Unit get;
} (hs.prefix = "DualStringPair_with_")

typedef glean.Id DerivedKeyValue2_id

@glean.PredicateAnnotation{
  name="glean.test.DerivedKeyValue2";
  version=1;
}
union DerivedKeyValue2 {
  1: DerivedKeyValue2_id id (hs.strict);
  2: DerivedKeyValue2_key key;
  3: builtin.Unit get;
} (hs.prefix = "DerivedKeyValue2_with_")

typedef glean.Id DerivedKeyValue_id

@glean.PredicateAnnotation{
  name="glean.test.DerivedKeyValue";
  version=1;
}
union DerivedKeyValue {
  1: DerivedKeyValue_id id (hs.strict);
  2: DerivedKeyValue_key key;
  3: builtin.Unit get;
} (hs.prefix = "DerivedKeyValue_with_")

typedef glean.Id Bar_id

@glean.PredicateAnnotation{
  name="glean.test.Bar";
  version=4;
}
union Bar {
  1: Bar_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Bar_with_")

typedef glean.Id Predicate_id

@glean.PredicateAnnotation{
  name="glean.test.Predicate";
  version=4;
}
union Predicate {
  1: Predicate_id id (hs.strict);
  2: KitchenSink key;
  3: builtin.Unit get;
} (hs.prefix = "Predicate_with_")

struct NothingTest_a {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string just;
  3: bool any = false;
}

struct nothingTest_key {
  1: optional NothingTest_a a;
  2: optional glean.Nat b;
}

struct WrappedStringPair {
  1: optional StringPair wrapped (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ViaStringPair_key {
  1: optional string fst;
  2: optional string snd;
}

struct Unbound2_key {
  1: optional string x;
  2: optional string y;
}

struct Unbound_key {
  1: optional string x;
  2: optional string y;
}

typedef Tree TreeToTree_value

struct Tree_left {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Tree just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Tree_right {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Tree just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Tree_key {
  1: optional string node;
  2: optional Tree_left left (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Tree_right right (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Sum {
  1: optional glean.Byte mon;
  2: optional glean.Nat tue;
  3: optional bool wed;
  4: bool any = false;
}

struct StringPairBox_key {
  1: optional StringPair box (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct StringPair_key {
  1: optional string fst;
  2: optional string snd;
}

struct StoredRevStringPairWithA_key {
  1: optional string fst;
  2: optional string snd;
}

struct StoredRevStringPair_key {
  1: optional string fst;
  2: optional string snd;
}

struct SameString_key {
  1: optional string x;
  2: optional string y;
}

struct RevStringPairs_key {
  1: optional string x;
  2: optional RevStringPair r (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct RevStringPairRec_key {
  1: optional string fst;
  2: optional string snd;
}

struct RevStringPair_key {
  1: optional string fst;
  2: optional string snd;
}

struct RevRevStringPair_key {
  1: optional string fst;
  2: optional string snd;
}

struct Node_key {
  1: optional string label;
}

struct MatchOneAlt_key {
  1: optional Sum x (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional glean.Nat y;
}

struct LeftOr2_key {
  1: optional string x;
  2: optional glean.Nat y;
}

struct LeftOr_key {
  1: optional string x;
  2: optional glean.Nat y;
}

struct KeyValue_key {
  1: optional string kstring;
  2: optional glean.Nat knat;
}

struct KeyValue_value {
  1: optional glean.Nat vnat;
  2: optional string vstring;
}

typedef Foo FooToFoo_value

typedef Bar Foo_value

struct Expr_ap_ {
  1: optional Expr fun (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Expr arg (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Expr_lam_ {
  1: optional Name var_ (java.swift.name = "var_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Expr body (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Expr_key {
  1: optional Name var_ (java.swift.name = "var_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional glean.Nat lit;
  3: optional Name prim (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional Expr_ap_ ap (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional Expr_lam_ lam (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: bool any = false;
}

enum Enum {
  red = 0,
  green = 1,
  blue = 2
} (hs.nounknown)

struct Rec {
  1: optional Enum alpha;
  2: optional Sum beta (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union KitchenSink_1_array_of_nat_array {
  1: glean.Nat every;
  2: list<glean.Nat> exact;
}

struct KitchenSink_1_record_ {
  1: optional glean.Byte a;
  2: optional glean.Nat b;
}

struct KitchenSink_1_sum_ {
  1: optional glean.Byte c;
  2: optional glean.Nat d;
  3: bool any = false;
}

struct KitchenSink_1_maybe_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional builtin.Unit just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct KitchenSink_1 {
  1: optional glean.Byte byt;
  2: optional glean.Nat nat;
  3: optional binary array_of_byte;
  4: optional KitchenSink_1_array_of_nat_array array_of_nat;
  5: optional KitchenSink_1_record_ record_ (java.swift.name = "record_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional KitchenSink_1_sum_ sum_ (java.swift.name = "sum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional Rec named_record_ (java.swift.name = "named_record_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional Sum named_sum_ (java.swift.name = "named_sum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional Enum named_enum_ (java.swift.name = "named_enum_");
  10: optional sys.Blob pred (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  11: optional KitchenSink_1_maybe_ maybe_ (java.swift.name = "maybe_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  12: optional bool bool_ (java.swift.name = "bool_");
  13: optional string string_ (java.swift.name = "string_");
}

struct Entity {
  1: optional code_cxx.Entity cxx (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional pp1.Define pp (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Edge_key {
  1: optional Node parent (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Node child (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DualStringPair_key {
  1: optional StringPair fst (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional StringPair snd (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DerivedKeyValue2_key {
  1: optional string kstring;
  2: optional glean.Nat knat;
}

struct DerivedKeyValue2_value {
  1: optional glean.Nat vnat;
  2: optional string vstring;
}

struct DerivedKeyValue_key {
  1: optional string kstring;
  2: optional glean.Nat knat;
  3: optional glean.Nat vnat;
  4: optional string vstring;
}

typedef Qux Bar_value

union ArrayString {
  1: string every;
  2: list<string> exact;
}

union ArrayNat {
  1: glean.Nat every;
  2: list<glean.Nat> exact;
}

typedef binary ArrayByte

union ArrayBool {
  1: bool every;
  2: list<bool> exact;
}

struct KitchenSink_maybe_ {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional builtin.Unit just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct KitchenSink_record_ {
  1: optional glean.Byte a;
  2: optional glean.Nat b;
}

struct KitchenSink_sum_ {
  1: optional Predicate c (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional sys.Blob d (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

enum KitchenSink_enum_ {
  e = 0,
  f = 1,
  g = 2
} (hs.nounknown)

union KitchenSink_array_of_nat_array {
  1: glean.Nat every;
  2: list<glean.Nat> exact;
}

union KitchenSink_array_of_bool_array {
  1: bool every;
  2: list<bool> exact;
}

union KitchenSink_array_of_string_array {
  1: string every;
  2: list<string> exact;
}

union KitchenSink_array_of_pred_array {
  1: Predicate every;
  2: list<Predicate> exact;
}

union KitchenSink_array_of_named_record_array {
  1: Rec every;
  2: list<Rec> exact;
}

union KitchenSink_array_of_named_sum_array {
  1: Sum every;
  2: list<Sum> exact;
}

union KitchenSink_array_of_named_enum_array {
  1: Enum every;
  2: list<Enum> exact;
}

union KitchenSink_array2_of_byte_array {
  1: ArrayByte every;
  2: list<ArrayByte> exact;
}

union KitchenSink_array2_of_nat_array {
  1: ArrayNat every;
  2: list<ArrayNat> exact;
}

union KitchenSink_array2_of_bool_array {
  1: ArrayBool every;
  2: list<ArrayBool> exact;
}

union KitchenSink_array2_of_string_array {
  1: ArrayString every;
  2: list<ArrayString> exact;
}

struct KitchenSink {
  1: optional glean.Byte byt;
  2: optional glean.Nat nat;
  3: optional bool bool_ (java.swift.name = "bool_");
  4: optional string string_ (java.swift.name = "string_");
  5: optional sys.Blob pred (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional KitchenSink_maybe_ maybe_ (java.swift.name = "maybe_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional KitchenSink_record_ record_ (java.swift.name = "record_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional KitchenSink_sum_ sum_ (java.swift.name = "sum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional KitchenSink_enum_ enum_ (java.swift.name = "enum_");
  10: optional Rec named_record_ (java.swift.name = "named_record_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  11: optional Sum named_sum_ (java.swift.name = "named_sum_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  12: optional Enum named_enum_ (java.swift.name = "named_enum_");
  13: optional binary array_of_byte;
  14: optional KitchenSink_array_of_nat_array array_of_nat;
  15: optional KitchenSink_array_of_bool_array array_of_bool;
  16: optional KitchenSink_array_of_string_array array_of_string;
  17: optional KitchenSink_array_of_pred_array array_of_pred (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  18: optional KitchenSink_array_of_named_record_array array_of_named_record (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  19: optional KitchenSink_array_of_named_sum_array array_of_named_sum (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  20: optional KitchenSink_array_of_named_enum_array array_of_named_enum;
  21: optional KitchenSink_array2_of_byte_array array2_of_byte;
  22: optional KitchenSink_array2_of_nat_array array2_of_nat;
  23: optional KitchenSink_array2_of_bool_array array2_of_bool;
  24: optional KitchenSink_array2_of_string_array array2_of_string;
}
