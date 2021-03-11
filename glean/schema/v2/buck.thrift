// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/builtin.thrift"
include "glean/schema/v2/src.thrift"
include "glean/schema/v2/sys.thrift"

namespace cpp2 facebook.glean.schema.buck
namespace hs Glean.Schema
namespace php glean_schema_buck
namespace py glean.schema.buck
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.buck
namespace rust glean_schema_buck

hs_include "glean/schema/v2/buck_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "Locator": 1,
  "TargetDependencies": 1,
  "Type": 1,
  "Platform": 1,
  "TargetSources": 3,
  "TargetSources_1": 1,
  "OutTarget": 1,
  "OutputLabel": 3,
  "Owner": 3,
  "Owner_1": 1,
  "Target": 2,
  "Target_1": 1,
  "RuleKey": 1,
  "TargetHash": 1,
  "Label": 1,
  "Labels": 1,
  "TargetLinkWhole": 1,
  "TargetIndexer": 3,
  "TargetIndexerName": 3,
  "TargetOut": 1,
  "LocatorReverseDeps": 1,
  "TargetOuts": 3,
  "LocatorWithLabel": 3,
  "OutsTarget": 3,
  "File": 3,
  "File_1": 1,
  "FileResolved": 3,
  "Consumer": 3,
  "TranslationUnit": 2,
  "TranslationUnit_1": 1,
}


typedef glean.Id Type_id

@glean.PredicateAnnotation{
  name="buck.Type";
  version=1;
}
struct Type {
  1: Type_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id TranslationUnit_id

@glean.PredicateAnnotation{
  name="buck.TranslationUnit";
  version=2;
}
struct TranslationUnit {
  1: TranslationUnit_id id (hs.strict);
  2: optional TranslationUnit_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TranslationUnit_1_id

@glean.PredicateAnnotation{
  name="buck.TranslationUnit";
  version=1;
}
struct TranslationUnit_1 {
  1: TranslationUnit_1_id id (hs.strict);
  2: optional TranslationUnit_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetSources_id

@glean.PredicateAnnotation{
  name="buck.TargetSources";
  version=3;
}
struct TargetSources {
  1: TargetSources_id id (hs.strict);
  2: optional TargetSources_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetSources_1_id

@glean.PredicateAnnotation{
  name="buck.TargetSources";
  version=1;
}
struct TargetSources_1 {
  1: TargetSources_1_id id (hs.strict);
  2: optional TargetSources_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetOuts_id

@glean.PredicateAnnotation{
  name="buck.TargetOuts";
  version=3;
}
struct TargetOuts {
  1: TargetOuts_id id (hs.strict);
  2: optional TargetOuts_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetOut_id

@glean.PredicateAnnotation{
  name="buck.TargetOut";
  version=1;
}
struct TargetOut {
  1: TargetOut_id id (hs.strict);
  2: optional TargetOut_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetLinkWhole_id

@glean.PredicateAnnotation{
  name="buck.TargetLinkWhole";
  version=1;
}
struct TargetLinkWhole {
  1: TargetLinkWhole_id id (hs.strict);
  2: optional Target key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetIndexerName_id

@glean.PredicateAnnotation{
  name="buck.TargetIndexerName";
  version=3;
}
struct TargetIndexerName {
  1: TargetIndexerName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id TargetIndexer_id

@glean.PredicateAnnotation{
  name="buck.TargetIndexer";
  version=3;
}
struct TargetIndexer {
  1: TargetIndexer_id id (hs.strict);
  2: optional TargetIndexer_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetHash_id

@glean.PredicateAnnotation{
  name="buck.TargetHash";
  version=1;
}
struct TargetHash {
  1: TargetHash_id id (hs.strict);
  2: optional TargetHash_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Target_id

@glean.PredicateAnnotation{
  name="buck.Target";
  version=2;
}
struct Target {
  1: Target_id id (hs.strict);
  2: optional Target_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Target_1_id

@glean.PredicateAnnotation{
  name="buck.Target";
  version=1;
}
struct Target_1 {
  1: Target_1_id id (hs.strict);
  2: optional Target_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id RuleKey_id

@glean.PredicateAnnotation{
  name="buck.RuleKey";
  version=1;
}
struct RuleKey {
  1: RuleKey_id id (hs.strict);
  2: optional RuleKey_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Platform_id

@glean.PredicateAnnotation{
  name="buck.Platform";
  version=1;
}
struct Platform {
  1: Platform_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Owner_id

@glean.PredicateAnnotation{
  name="buck.Owner";
  version=3;
}
struct Owner {
  1: Owner_id id (hs.strict);
  2: optional Owner_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Owner_1_id

@glean.PredicateAnnotation{
  name="buck.Owner";
  version=1;
}
struct Owner_1 {
  1: Owner_1_id id (hs.strict);
  2: optional Owner_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id OutsTarget_id

@glean.PredicateAnnotation{
  name="buck.OutsTarget";
  version=3;
}
struct OutsTarget {
  1: OutsTarget_id id (hs.strict);
  2: optional OutsTarget_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id OutputLabel_id

@glean.PredicateAnnotation{
  name="buck.OutputLabel";
  version=3;
}
struct OutputLabel {
  1: OutputLabel_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id OutTarget_id

@glean.PredicateAnnotation{
  name="buck.OutTarget";
  version=1;
}
struct OutTarget {
  1: OutTarget_id id (hs.strict);
  2: optional OutTarget_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id LocatorWithLabel_id

@glean.PredicateAnnotation{
  name="buck.LocatorWithLabel";
  version=3;
}
struct LocatorWithLabel {
  1: LocatorWithLabel_id id (hs.strict);
  2: optional LocatorWithLabel_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id LocatorReverseDeps_id

@glean.PredicateAnnotation{
  name="buck.LocatorReverseDeps";
  version=1;
}
struct LocatorReverseDeps {
  1: LocatorReverseDeps_id id (hs.strict);
  2: optional LocatorReverseDeps_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Locator_id

@glean.PredicateAnnotation{
  name="buck.Locator";
  version=1;
}
struct Locator {
  1: Locator_id id (hs.strict);
  2: optional Locator_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Labels_id

@glean.PredicateAnnotation{
  name="buck.Labels";
  version=1;
}
struct Labels {
  1: Labels_id id (hs.strict);
  2: optional list<Label> key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Label_id

@glean.PredicateAnnotation{
  name="buck.Label";
  version=1;
}
struct Label {
  1: Label_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id FileResolved_id

@glean.PredicateAnnotation{
  name="buck.FileResolved";
  version=3;
}
struct FileResolved {
  1: FileResolved_id id (hs.strict);
  2: optional FileResolved_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id File_id

@glean.PredicateAnnotation{
  name="buck.File";
  version=3;
}
struct File {
  1: File_id id (hs.strict);
  2: optional File_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id File_1_id

@glean.PredicateAnnotation{
  name="buck.File";
  version=1;
}
struct File_1 {
  1: File_1_id id (hs.strict);
  2: optional File_1_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TargetDependencies_id

@glean.PredicateAnnotation{
  name="buck.TargetDependencies";
  version=1;
}
struct TargetDependencies {
  1: TargetDependencies_id id (hs.strict);
  2: optional TargetDependencies_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Consumer_id

@glean.PredicateAnnotation{
  name="buck.Consumer";
  version=3;
}
struct Consumer {
  1: Consumer_id id (hs.strict);
  2: optional Consumer_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TranslationUnit_key {
  1: src.File file;
  2: Locator target;
  3: optional Platform platform;
}

struct TranslationUnit_1_key {
  1: src.File file;
  2: Target_1 target;
}

struct TargetSources_key {
  1: Target target;
  2: list<File> headers;
  3: list<File> exportedHeaders;
  4: list<File> srcs;
}

struct TargetSources_1_key {
  1: Target target;
  2: list<File_1> headers;
  3: list<File_1> exportedHeaders;
  4: list<File_1> srcs;
}

struct TargetOuts_key {
  1: Target target;
  2: optional OutputLabel outputLabel;
  3: src.File file;
}

struct TargetOut_key {
  1: Target target;
  2: src.File file;
}

struct TargetIndexer_key {
  1: TargetIndexerName name;
  2: Target target;
}

struct TargetHash_key {
  1: Locator locator;
  2: string targetHash;
}

struct Target_key {
  1: Locator locator;
  2: Type type_ (java.swift.name = "type_");
  3: optional Platform defaultPlatform;
  4: Labels labels;
}

struct Target_1_key {
  1: sys.Blob repo;
  2: sys.Blob name;
  3: optional sys.Blob platform;
}

struct RuleKey_key {
  1: Locator locator;
  2: string ruleKey;
}

struct Owner_key {
  1: src.File source;
  2: TargetSources owner;
}

struct Owner_1_key {
  1: src.File source;
  2: TargetSources_1 owner;
}

struct OutsTarget_key {
  1: src.File file;
  2: Target target;
  3: optional OutputLabel outputLabel;
}

struct OutTarget_key {
  1: src.File file;
  2: Target target;
}

struct LocatorWithLabel_key {
  1: Locator locator;
  2: OutputLabel label;
}

struct LocatorReverseDeps_key {
  1: Locator locator;
  2: list<Locator> rdeps;
}

struct Locator_key {
  1: optional string subdir;
  2: string path;
  3: string name;
}

struct FileResolved_key {
  1: File buckFile;
  2: src.File srcFile;
}

union File_key {
  1: src.File source;
  2: Locator generated;
  3: LocatorWithLabel generatedLabel;
} (hs.nonempty)

union File_1_key {
  1: src.File source;
  2: Locator generated;
} (hs.nonempty)

struct Dependency {
  1: Locator target;
  2: bool explicit_ (java.swift.name = "explicit_");
  3: bool exported;
}

struct TargetDependencies_key {
  1: Target target;
  2: list<Dependency> dependencies;
}

struct Consumer_key {
  1: src.File source;
  2: TargetSources consumer;
}
