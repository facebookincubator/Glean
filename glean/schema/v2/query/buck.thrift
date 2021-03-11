// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/query/builtin.thrift"
include "glean/schema/v2/query/src.thrift"
include "glean/schema/v2/query/sys.thrift"

namespace cpp2 facebook.glean.schema.query.buck
namespace hs Glean.Schema.Query
namespace php glean_schema_query_buck
namespace py glean.schema.query.buck
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.buck
namespace rust glean_schema_query_buck

hs_include "glean/schema/v2/query/buck_include.hs"
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
union Type_ {
  1: Type_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Type__with_")

typedef glean.Id TranslationUnit_id

@glean.PredicateAnnotation{
  name="buck.TranslationUnit";
  version=2;
}
union TranslationUnit {
  1: TranslationUnit_id id (hs.strict);
  2: TranslationUnit_key key;
  3: builtin.Unit get;
} (hs.prefix = "TranslationUnit_with_")

typedef glean.Id TranslationUnit_1_id

@glean.PredicateAnnotation{
  name="buck.TranslationUnit";
  version=1;
}
union TranslationUnit_1 {
  1: TranslationUnit_1_id id (hs.strict);
  2: TranslationUnit_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "TranslationUnit_1_with_")

typedef glean.Id TargetSources_id

@glean.PredicateAnnotation{
  name="buck.TargetSources";
  version=3;
}
union TargetSources {
  1: TargetSources_id id (hs.strict);
  2: TargetSources_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetSources_with_")

typedef glean.Id TargetSources_1_id

@glean.PredicateAnnotation{
  name="buck.TargetSources";
  version=1;
}
union TargetSources_1 {
  1: TargetSources_1_id id (hs.strict);
  2: TargetSources_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetSources_1_with_")

typedef glean.Id TargetOuts_id

@glean.PredicateAnnotation{
  name="buck.TargetOuts";
  version=3;
}
union TargetOuts {
  1: TargetOuts_id id (hs.strict);
  2: TargetOuts_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetOuts_with_")

typedef glean.Id TargetOut_id

@glean.PredicateAnnotation{
  name="buck.TargetOut";
  version=1;
}
union TargetOut {
  1: TargetOut_id id (hs.strict);
  2: TargetOut_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetOut_with_")

typedef glean.Id TargetLinkWhole_id

@glean.PredicateAnnotation{
  name="buck.TargetLinkWhole";
  version=1;
}
union TargetLinkWhole {
  1: TargetLinkWhole_id id (hs.strict);
  2: Target key;
  3: builtin.Unit get;
} (hs.prefix = "TargetLinkWhole_with_")

typedef glean.Id TargetIndexerName_id

@glean.PredicateAnnotation{
  name="buck.TargetIndexerName";
  version=3;
}
union TargetIndexerName {
  1: TargetIndexerName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "TargetIndexerName_with_")

typedef glean.Id TargetIndexer_id

@glean.PredicateAnnotation{
  name="buck.TargetIndexer";
  version=3;
}
union TargetIndexer {
  1: TargetIndexer_id id (hs.strict);
  2: TargetIndexer_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetIndexer_with_")

typedef glean.Id TargetHash_id

@glean.PredicateAnnotation{
  name="buck.TargetHash";
  version=1;
}
union TargetHash {
  1: TargetHash_id id (hs.strict);
  2: TargetHash_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetHash_with_")

typedef glean.Id Target_id

@glean.PredicateAnnotation{
  name="buck.Target";
  version=2;
}
union Target {
  1: Target_id id (hs.strict);
  2: Target_key key;
  3: builtin.Unit get;
} (hs.prefix = "Target_with_")

typedef glean.Id Target_1_id

@glean.PredicateAnnotation{
  name="buck.Target";
  version=1;
}
union Target_1 {
  1: Target_1_id id (hs.strict);
  2: Target_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "Target_1_with_")

typedef glean.Id RuleKey_id

@glean.PredicateAnnotation{
  name="buck.RuleKey";
  version=1;
}
union RuleKey {
  1: RuleKey_id id (hs.strict);
  2: RuleKey_key key;
  3: builtin.Unit get;
} (hs.prefix = "RuleKey_with_")

typedef glean.Id Platform_id

@glean.PredicateAnnotation{
  name="buck.Platform";
  version=1;
}
union Platform {
  1: Platform_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Platform_with_")

typedef glean.Id Owner_id

@glean.PredicateAnnotation{
  name="buck.Owner";
  version=3;
}
union Owner {
  1: Owner_id id (hs.strict);
  2: Owner_key key;
  3: builtin.Unit get;
} (hs.prefix = "Owner_with_")

typedef glean.Id Owner_1_id

@glean.PredicateAnnotation{
  name="buck.Owner";
  version=1;
}
union Owner_1 {
  1: Owner_1_id id (hs.strict);
  2: Owner_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "Owner_1_with_")

typedef glean.Id OutsTarget_id

@glean.PredicateAnnotation{
  name="buck.OutsTarget";
  version=3;
}
union OutsTarget {
  1: OutsTarget_id id (hs.strict);
  2: OutsTarget_key key;
  3: builtin.Unit get;
} (hs.prefix = "OutsTarget_with_")

typedef glean.Id OutputLabel_id

@glean.PredicateAnnotation{
  name="buck.OutputLabel";
  version=3;
}
union OutputLabel {
  1: OutputLabel_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "OutputLabel_with_")

typedef glean.Id OutTarget_id

@glean.PredicateAnnotation{
  name="buck.OutTarget";
  version=1;
}
union OutTarget {
  1: OutTarget_id id (hs.strict);
  2: OutTarget_key key;
  3: builtin.Unit get;
} (hs.prefix = "OutTarget_with_")

typedef glean.Id LocatorWithLabel_id

@glean.PredicateAnnotation{
  name="buck.LocatorWithLabel";
  version=3;
}
union LocatorWithLabel {
  1: LocatorWithLabel_id id (hs.strict);
  2: LocatorWithLabel_key key;
  3: builtin.Unit get;
} (hs.prefix = "LocatorWithLabel_with_")

typedef glean.Id LocatorReverseDeps_id

@glean.PredicateAnnotation{
  name="buck.LocatorReverseDeps";
  version=1;
}
union LocatorReverseDeps {
  1: LocatorReverseDeps_id id (hs.strict);
  2: LocatorReverseDeps_key key;
  3: builtin.Unit get;
} (hs.prefix = "LocatorReverseDeps_with_")

typedef glean.Id Locator_id

@glean.PredicateAnnotation{
  name="buck.Locator";
  version=1;
}
union Locator {
  1: Locator_id id (hs.strict);
  2: Locator_key key;
  3: builtin.Unit get;
} (hs.prefix = "Locator_with_")

typedef glean.Id Labels_id

@glean.PredicateAnnotation{
  name="buck.Labels";
  version=1;
}
union Labels {
  1: Labels_id id (hs.strict);
  2: Labels_array key;
  3: builtin.Unit get;
} (hs.prefix = "Labels_with_")

typedef glean.Id Label_id

@glean.PredicateAnnotation{
  name="buck.Label";
  version=1;
}
union Label {
  1: Label_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "Label_with_")

typedef glean.Id FileResolved_id

@glean.PredicateAnnotation{
  name="buck.FileResolved";
  version=3;
}
union FileResolved {
  1: FileResolved_id id (hs.strict);
  2: FileResolved_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileResolved_with_")

typedef glean.Id File_id

@glean.PredicateAnnotation{
  name="buck.File";
  version=3;
}
union File {
  1: File_id id (hs.strict);
  2: File_key key;
  3: builtin.Unit get;
} (hs.prefix = "File_with_")

typedef glean.Id File_1_id

@glean.PredicateAnnotation{
  name="buck.File";
  version=1;
}
union File_1 {
  1: File_1_id id (hs.strict);
  2: File_1_key key;
  3: builtin.Unit get;
} (hs.prefix = "File_1_with_")

typedef glean.Id TargetDependencies_id

@glean.PredicateAnnotation{
  name="buck.TargetDependencies";
  version=1;
}
union TargetDependencies {
  1: TargetDependencies_id id (hs.strict);
  2: TargetDependencies_key key;
  3: builtin.Unit get;
} (hs.prefix = "TargetDependencies_with_")

typedef glean.Id Consumer_id

@glean.PredicateAnnotation{
  name="buck.Consumer";
  version=3;
}
union Consumer {
  1: Consumer_id id (hs.strict);
  2: Consumer_key key;
  3: builtin.Unit get;
} (hs.prefix = "Consumer_with_")

struct TranslationUnit_platform {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Platform just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct TranslationUnit_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Locator target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TranslationUnit_platform platform (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TranslationUnit_1_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Target_1 target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union TargetSources_headers_array {
  1: File every;
  2: list<File> exact;
}

union TargetSources_exportedHeaders_array {
  1: File every;
  2: list<File> exact;
}

union TargetSources_srcs_array {
  1: File every;
  2: list<File> exact;
}

struct TargetSources_key {
  1: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TargetSources_headers_array headers (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TargetSources_exportedHeaders_array exportedHeaders (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional TargetSources_srcs_array srcs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union TargetSources_1_headers_array {
  1: File_1 every;
  2: list<File_1> exact;
}

union TargetSources_1_exportedHeaders_array {
  1: File_1 every;
  2: list<File_1> exact;
}

union TargetSources_1_srcs_array {
  1: File_1 every;
  2: list<File_1> exact;
}

struct TargetSources_1_key {
  1: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TargetSources_1_headers_array headers (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TargetSources_1_exportedHeaders_array exportedHeaders (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional TargetSources_1_srcs_array srcs (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TargetOuts_outputLabel {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional OutputLabel just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct TargetOuts_key {
  1: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TargetOuts_outputLabel outputLabel (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TargetOut_key {
  1: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TargetIndexer_key {
  1: optional TargetIndexerName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct TargetHash_key {
  1: optional Locator locator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string targetHash;
}

struct Target_defaultPlatform {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Platform just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Target_key {
  1: optional Locator locator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Type_ type_ (java.swift.name = "type_", cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Target_defaultPlatform defaultPlatform (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional Labels labels (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Target_1_platform {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional sys.Blob just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Target_1_key {
  1: optional sys.Blob repo (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional sys.Blob name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional Target_1_platform platform (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct RuleKey_key {
  1: optional Locator locator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string ruleKey;
}

struct Owner_key {
  1: optional src.File source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TargetSources owner (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Owner_1_key {
  1: optional src.File source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TargetSources_1 owner (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct OutsTarget_outputLabel {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional OutputLabel just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct OutsTarget_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional OutsTarget_outputLabel outputLabel (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct OutTarget_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct LocatorWithLabel_key {
  1: optional Locator locator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional OutputLabel label (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union LocatorReverseDeps_rdeps_array {
  1: Locator every;
  2: list<Locator> exact;
}

struct LocatorReverseDeps_key {
  1: optional Locator locator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional LocatorReverseDeps_rdeps_array rdeps (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Locator_subdir {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string just;
  3: bool any = false;
}

struct Locator_key {
  1: optional Locator_subdir subdir;
  2: optional string path;
  3: optional string name;
}

union Labels_array {
  1: Label every;
  2: list<Label> exact;
}

struct FileResolved_key {
  1: optional File buckFile (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File srcFile (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct File_key {
  1: optional src.File source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Locator generated (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional LocatorWithLabel generatedLabel (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: bool any = false;
}

struct File_1_key {
  1: optional src.File source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Locator generated (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Dependency {
  1: optional Locator target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional bool explicit_ (java.swift.name = "explicit_");
  3: optional bool exported;
}

union TargetDependencies_dependencies_array {
  1: Dependency every;
  2: list<Dependency> exact;
}

struct TargetDependencies_key {
  1: optional Target target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TargetDependencies_dependencies_array dependencies (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Consumer_key {
  1: optional src.File source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional TargetSources consumer (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
