// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/builtin.thrift"
include "glean/schema/v2/code_cxx.thrift"
include "glean/schema/v2/pp1.thrift"
include "glean/schema/v2/sys.thrift"

namespace cpp2 facebook.glean.schema.glean.test
namespace hs Glean.Schema
namespace php glean_schema_glean_test
namespace py glean.schema.glean_test
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.glean_test
namespace rust glean_schema_glean_test

hs_include "glean/schema/v2/glean_test_include.hs"
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
struct nothingTest {
  1: nothingTest_id id (hs.strict);
  2: optional nothingTest_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ViaStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.ViaStringPair";
  version=1;
}
struct ViaStringPair {
  1: ViaStringPair_id id (hs.strict);
  2: optional ViaStringPair_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Unbound2_id

@glean.PredicateAnnotation{
  name="glean.test.Unbound2";
  version=1;
}
struct Unbound2 {
  1: Unbound2_id id (hs.strict);
  2: optional Unbound2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Unbound_id

@glean.PredicateAnnotation{
  name="glean.test.Unbound";
  version=1;
}
struct Unbound {
  1: Unbound_id id (hs.strict);
  2: optional Unbound_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TreeToTree_id

@glean.PredicateAnnotation{
  name="glean.test.TreeToTree";
  version=4;
}
struct TreeToTree {
  1: TreeToTree_id id (hs.strict);
  2: optional Tree key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TreeToTree_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Tree_id

@glean.PredicateAnnotation{
  name="glean.test.Tree";
  version=4;
}
struct Tree {
  1: Tree_id id (hs.strict);
  2: optional Tree_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id StringPairBox_id

@glean.PredicateAnnotation{
  name="glean.test.StringPairBox";
  version=1;
}
struct StringPairBox {
  1: StringPairBox_id id (hs.strict);
  2: optional StringPairBox_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id StringPair_id

@glean.PredicateAnnotation{
  name="glean.test.StringPair";
  version=1;
}
struct StringPair {
  1: StringPair_id id (hs.strict);
  2: optional StringPair_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id StoredRevStringPairWithA_id

@glean.PredicateAnnotation{
  name="glean.test.StoredRevStringPairWithA";
  version=1;
}
struct StoredRevStringPairWithA {
  1: StoredRevStringPairWithA_id id (hs.strict);
  2: optional StoredRevStringPairWithA_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id StoredRevStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.StoredRevStringPair";
  version=1;
}
struct StoredRevStringPair {
  1: StoredRevStringPair_id id (hs.strict);
  2: optional StoredRevStringPair_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SameString_id

@glean.PredicateAnnotation{
  name="glean.test.SameString";
  version=1;
}
struct SameString {
  1: SameString_id id (hs.strict);
  2: optional SameString_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id RevStringPairs_id

@glean.PredicateAnnotation{
  name="glean.test.RevStringPairs";
  version=1;
}
struct RevStringPairs {
  1: RevStringPairs_id id (hs.strict);
  2: optional RevStringPairs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id RevStringPairRec_id

@glean.PredicateAnnotation{
  name="glean.test.RevStringPairRec";
  version=1;
}
struct RevStringPairRec {
  1: RevStringPairRec_id id (hs.strict);
  2: optional RevStringPairRec_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id RevStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.RevStringPair";
  version=1;
}
struct RevStringPair {
  1: RevStringPair_id id (hs.strict);
  2: optional RevStringPair_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id RevRevStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.RevRevStringPair";
  version=1;
}
struct RevRevStringPair {
  1: RevRevStringPair_id id (hs.strict);
  2: optional RevRevStringPair_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ReflStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.ReflStringPair";
  version=1;
}
struct ReflStringPair {
  1: ReflStringPair_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id RefRef_id

@glean.PredicateAnnotation{
  name="glean.test.RefRef";
  version=4;
}
struct RefRef {
  1: RefRef_id id (hs.strict);
  2: optional Ref key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Ref_id

@glean.PredicateAnnotation{
  name="glean.test.Ref";
  version=4;
}
struct Ref {
  1: Ref_id id (hs.strict);
  2: optional Predicate key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Qux_id

@glean.PredicateAnnotation{
  name="glean.test.Qux";
  version=4;
}
struct Qux {
  1: Qux_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Node_id

@glean.PredicateAnnotation{
  name="glean.test.Node";
  version=4;
}
struct Node {
  1: Node_id id (hs.strict);
  2: optional Node_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Name_id

@glean.PredicateAnnotation{
  name="glean.test.Name";
  version=1;
}
struct Name {
  1: Name_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id MatchOneAlt_id

@glean.PredicateAnnotation{
  name="glean.test.MatchOneAlt";
  version=1;
}
struct MatchOneAlt {
  1: MatchOneAlt_id id (hs.strict);
  2: optional MatchOneAlt_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id LeftOr2_id

@glean.PredicateAnnotation{
  name="glean.test.LeftOr2";
  version=1;
}
struct LeftOr2 {
  1: LeftOr2_id id (hs.strict);
  2: optional LeftOr2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id LeftOr_id

@glean.PredicateAnnotation{
  name="glean.test.LeftOr";
  version=1;
}
struct LeftOr {
  1: LeftOr_id id (hs.strict);
  2: optional LeftOr_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id KeyValue_id

@glean.PredicateAnnotation{
  name="glean.test.KeyValue";
  version=1;
}
struct KeyValue {
  1: KeyValue_id id (hs.strict);
  2: optional KeyValue_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional KeyValue_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id IsThree_id

@glean.PredicateAnnotation{
  name="glean.test.IsThree";
  version=1;
}
struct IsThree {
  1: IsThree_id id (hs.strict);
  2: optional glean.Nat key;
}

typedef glean.Id IsGlean_id

@glean.PredicateAnnotation{
  name="glean.test.IsGlean";
  version=1;
}
struct IsGlean {
  1: IsGlean_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id FooToFoo_id

@glean.PredicateAnnotation{
  name="glean.test.FooToFoo";
  version=4;
}
struct FooToFoo {
  1: FooToFoo_id id (hs.strict);
  2: optional Foo key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional FooToFoo_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Foo_id

@glean.PredicateAnnotation{
  name="glean.test.Foo";
  version=4;
}
struct Foo {
  1: Foo_id id (hs.strict);
  2: optional string key;
  3: optional Foo_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Expr_id

@glean.PredicateAnnotation{
  name="glean.test.Expr";
  version=1;
}
struct Expr {
  1: Expr_id id (hs.strict);
  2: optional Expr_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Predicate_1_id

@glean.PredicateAnnotation{
  name="glean.test.Predicate";
  version=1;
}
struct Predicate_1 {
  1: Predicate_1_id id (hs.strict);
  2: optional KitchenSink_1 key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Edge_id

@glean.PredicateAnnotation{
  name="glean.test.Edge";
  version=4;
}
struct Edge {
  1: Edge_id id (hs.strict);
  2: optional Edge_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DualStringPair_id

@glean.PredicateAnnotation{
  name="glean.test.DualStringPair";
  version=1;
}
struct DualStringPair {
  1: DualStringPair_id id (hs.strict);
  2: optional DualStringPair_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DerivedKeyValue2_id

@glean.PredicateAnnotation{
  name="glean.test.DerivedKeyValue2";
  version=1;
}
struct DerivedKeyValue2 {
  1: DerivedKeyValue2_id id (hs.strict);
  2: optional DerivedKeyValue2_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional DerivedKeyValue2_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id DerivedKeyValue_id

@glean.PredicateAnnotation{
  name="glean.test.DerivedKeyValue";
  version=1;
}
struct DerivedKeyValue {
  1: DerivedKeyValue_id id (hs.strict);
  2: optional DerivedKeyValue_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Bar_id

@glean.PredicateAnnotation{
  name="glean.test.Bar";
  version=4;
}
struct Bar {
  1: Bar_id id (hs.strict);
  2: optional string key;
  3: optional Bar_value value (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Predicate_id

@glean.PredicateAnnotation{
  name="glean.test.Predicate";
  version=4;
}
struct Predicate {
  1: Predicate_id id (hs.strict);
  2: optional KitchenSink key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct nothingTest_key {
  1: optional string a;
  2: glean.Nat b;
}

struct WrappedStringPair {
  1: StringPair wrapped;
}

struct ViaStringPair_key {
  1: string fst;
  2: string snd;
}

struct Unbound2_key {
  1: string x;
  2: string y;
}

struct Unbound_key {
  1: string x;
  2: string y;
}

typedef Tree TreeToTree_value

struct Tree_key {
  1: string node;
  2: optional Tree left;
  3: optional Tree right;
}

union Sum {
  1: glean.Byte mon;
  2: glean.Nat tue;
  3: bool wed;
} (hs.nonempty)

struct StringPairBox_key {
  1: StringPair box;
}

struct StringPair_key {
  1: string fst;
  2: string snd;
}

struct StoredRevStringPairWithA_key {
  1: string fst;
  2: string snd;
}

struct StoredRevStringPair_key {
  1: string fst;
  2: string snd;
}

struct SameString_key {
  1: string x;
  2: string y;
}

struct RevStringPairs_key {
  1: string x;
  2: RevStringPair r;
}

struct RevStringPairRec_key {
  1: string fst;
  2: string snd;
}

struct RevStringPair_key {
  1: string fst;
  2: string snd;
}

struct RevRevStringPair_key {
  1: string fst;
  2: string snd;
}

struct Node_key {
  1: string label;
}

struct MatchOneAlt_key {
  1: Sum x;
  2: glean.Nat y;
}

struct LeftOr2_key {
  1: string x;
  2: glean.Nat y;
}

struct LeftOr_key {
  1: string x;
  2: glean.Nat y;
}

struct KeyValue_key {
  1: string kstring;
  2: glean.Nat knat;
}

struct KeyValue_value {
  1: glean.Nat vnat;
  2: string vstring;
}

typedef Foo FooToFoo_value

typedef Bar Foo_value

struct Expr_ap_ {
  1: Expr fun;
  2: Expr arg;
}

struct Expr_lam_ {
  1: Name var_ (java.swift.name = "var_");
  2: Expr body;
}

union Expr_key {
  1: Name var_ (java.swift.name = "var_");
  2: glean.Nat lit;
  3: Name prim;
  4: Expr_ap_ ap;
  5: Expr_lam_ lam;
} (hs.nonempty)

enum Enum {
  red = 0,
  green = 1,
  blue = 2
} (hs.nounknown)

struct Rec {
  1: Enum alpha;
  2: Sum beta;
}

struct KitchenSink_1_record_ {
  1: glean.Byte a;
  2: glean.Nat b;
}

union KitchenSink_1_sum_ {
  1: glean.Byte c;
  2: glean.Nat d;
} (hs.nonempty)

struct KitchenSink_1 {
  1: glean.Byte byt;
  2: glean.Nat nat;
  3: binary array_of_byte;
  4: list<glean.Nat> array_of_nat;
  5: KitchenSink_1_record_ record_ (java.swift.name = "record_");
  6: KitchenSink_1_sum_ sum_ (java.swift.name = "sum_");
  7: Rec named_record_ (java.swift.name = "named_record_");
  8: Sum named_sum_ (java.swift.name = "named_sum_");
  9: Enum named_enum_ (java.swift.name = "named_enum_");
  10: sys.Blob pred;
  11: optional builtin.Unit maybe_ (java.swift.name = "maybe_");
  12: bool bool_ (java.swift.name = "bool_");
  13: string string_ (java.swift.name = "string_");
}

union Entity {
  1: code_cxx.Entity cxx;
  2: pp1.Define pp;
} (hs.nonempty)

struct Edge_key {
  1: Node parent;
  2: Node child;
}

struct DualStringPair_key {
  1: StringPair fst;
  2: StringPair snd;
}

struct DerivedKeyValue2_key {
  1: string kstring;
  2: glean.Nat knat;
}

struct DerivedKeyValue2_value {
  1: glean.Nat vnat;
  2: string vstring;
}

struct DerivedKeyValue_key {
  1: string kstring;
  2: glean.Nat knat;
  3: glean.Nat vnat;
  4: string vstring;
}

typedef Qux Bar_value

typedef list<string> ArrayString

typedef list<glean.Nat> ArrayNat

typedef binary ArrayByte

typedef list<bool> ArrayBool

struct KitchenSink_record_ {
  1: glean.Byte a;
  2: glean.Nat b;
}

union KitchenSink_sum_ {
  1: Predicate c;
  2: sys.Blob d;
} (hs.nonempty)

enum KitchenSink_enum_ {
  e = 0,
  f = 1,
  g = 2
} (hs.nounknown)

struct KitchenSink {
  1: glean.Byte byt;
  2: glean.Nat nat;
  3: bool bool_ (java.swift.name = "bool_");
  4: string string_ (java.swift.name = "string_");
  5: sys.Blob pred;
  6: optional builtin.Unit maybe_ (java.swift.name = "maybe_");
  7: KitchenSink_record_ record_ (java.swift.name = "record_");
  8: KitchenSink_sum_ sum_ (java.swift.name = "sum_");
  9: KitchenSink_enum_ enum_ (java.swift.name = "enum_");
  10: Rec named_record_ (java.swift.name = "named_record_");
  11: Sum named_sum_ (java.swift.name = "named_sum_");
  12: Enum named_enum_ (java.swift.name = "named_enum_");
  13: binary array_of_byte;
  14: list<glean.Nat> array_of_nat;
  15: list<bool> array_of_bool;
  16: list<string> array_of_string;
  17: list<Predicate> array_of_pred;
  18: list<Rec> array_of_named_record;
  19: list<Sum> array_of_named_sum;
  20: list<Enum> array_of_named_enum;
  21: list<ArrayByte> array2_of_byte;
  22: list<ArrayNat> array2_of_nat;
  23: list<ArrayBool> array2_of_bool;
  24: list<ArrayString> array2_of_string;
}
