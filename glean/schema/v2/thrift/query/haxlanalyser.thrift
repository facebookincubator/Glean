// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/thrift/query/builtin.thrift"
include "glean/schema/v2/thrift/query/hs.thrift"
include "glean/schema/v2/thrift/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.haxlanalyser
namespace hs Glean.Schema.Query
namespace php glean_schema_query_haxlanalyser
namespace py glean.schema.query.haxlanalyser
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.haxlanalyser
namespace rust glean_schema_query_haxlanalyser

hs_include "glean/schema/v2/thrift/query/haxlanalyser_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "ScubaResponse": 1,
  "ClassifierRead": 1,
  "SitevarFetch": 1,
  "Call": 1,
  "FeatureSetFeature": 1,
  "ACDCProperty": 1,
  "ScribeResponse": 1,
  "TallyResponse": 1,
  "HiveResponse": 1,
  "ConfigeratorFetch": 1,
  "TallyConfig": 1,
  "ThriftResponse": 1,
  "Endpoint": 1,
  "Edge": 1,
  "ContextName": 1,
  "TestFile": 1,
  "TallyFetch": 1,
  "Policy": 1,
  "InputFetch": 1,
  "RestrictionResponse": 1,
  "Context": 1,
  "PolicyName": 1,
  "EndpointName": 1,
  "JankyJSONResponse": 1,
  "ThriftFetch": 1,
  "InputKey": 1,
  "ACDCPropertyAccess": 1,
  "PiranhaResponse": 1,
  "LaserDataset": 1,
  "ClassifierFetch": 1,
  "TallyName": 1,
  "LogFeatureResponse": 1,
}


typedef glean.Id ThriftResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ThriftResponse";
  version=1;
}
union ThriftResponse {
  1: ThriftResponse_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ThriftResponse_with_")

typedef glean.Id ThriftFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ThriftFetch";
  version=1;
}
union ThriftFetch {
  1: ThriftFetch_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ThriftFetch_with_")

typedef glean.Id TestFile_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TestFile";
  version=1;
}
union TestFile {
  1: TestFile_id id (hs.strict);
  2: src.File key;
  3: builtin.Unit get;
} (hs.prefix = "TestFile_with_")

typedef glean.Id TallyName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyName";
  version=1;
}
union TallyName {
  1: TallyName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "TallyName_with_")

typedef glean.Id TallyConfig_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyConfig";
  version=1;
}
union TallyConfig {
  1: TallyConfig_id id (hs.strict);
  2: TallyConfig_key key;
  3: builtin.Unit get;
} (hs.prefix = "TallyConfig_with_")

typedef glean.Id TallyFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyFetch";
  version=1;
}
union TallyFetch {
  1: TallyFetch_id id (hs.strict);
  2: Tally key;
  3: builtin.Unit get;
} (hs.prefix = "TallyFetch_with_")

typedef glean.Id TallyResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyResponse";
  version=1;
}
union TallyResponse {
  1: TallyResponse_id id (hs.strict);
  2: Tally key;
  3: builtin.Unit get;
} (hs.prefix = "TallyResponse_with_")

typedef glean.Id SitevarFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.SitevarFetch";
  version=1;
}
union SitevarFetch {
  1: SitevarFetch_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "SitevarFetch_with_")

typedef glean.Id ScubaResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ScubaResponse";
  version=1;
}
union ScubaResponse {
  1: ScubaResponse_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ScubaResponse_with_")

typedef glean.Id ScribeResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ScribeResponse";
  version=1;
}
union ScribeResponse {
  1: ScribeResponse_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ScribeResponse_with_")

typedef glean.Id RestrictionResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.RestrictionResponse";
  version=1;
}
union RestrictionResponse {
  1: RestrictionResponse_id id (hs.strict);
  2: glean.Nat key;
  3: builtin.Unit get;
} (hs.prefix = "RestrictionResponse_with_")

typedef glean.Id PolicyName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.PolicyName";
  version=1;
}
union PolicyName {
  1: PolicyName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "PolicyName_with_")

typedef glean.Id Policy_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Policy";
  version=1;
}
union Policy {
  1: Policy_id id (hs.strict);
  2: Policy_key key;
  3: builtin.Unit get;
} (hs.prefix = "Policy_with_")

typedef glean.Id PiranhaResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.PiranhaResponse";
  version=1;
}
union PiranhaResponse {
  1: PiranhaResponse_id id (hs.strict);
  2: glean.Nat key;
  3: builtin.Unit get;
} (hs.prefix = "PiranhaResponse_with_")

typedef glean.Id LogFeatureResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.LogFeatureResponse";
  version=1;
}
union LogFeatureResponse {
  1: LogFeatureResponse_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "LogFeatureResponse_with_")

typedef glean.Id LaserDataset_id

@glean.PredicateAnnotation{
  name="haxlanalyser.LaserDataset";
  version=1;
}
union LaserDataset {
  1: LaserDataset_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "LaserDataset_with_")

typedef glean.Id JankyJSONResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.JankyJSONResponse";
  version=1;
}
union JankyJSONResponse {
  1: JankyJSONResponse_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "JankyJSONResponse_with_")

typedef glean.Id InputKey_id

@glean.PredicateAnnotation{
  name="haxlanalyser.InputKey";
  version=1;
}
union InputKey {
  1: InputKey_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "InputKey_with_")

typedef glean.Id InputFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.InputFetch";
  version=1;
}
union InputFetch {
  1: InputFetch_id id (hs.strict);
  2: InputFetch_key key;
  3: builtin.Unit get;
} (hs.prefix = "InputFetch_with_")

typedef glean.Id HiveResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.HiveResponse";
  version=1;
}
union HiveResponse {
  1: HiveResponse_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "HiveResponse_with_")

typedef glean.Id FeatureSetFeature_id

@glean.PredicateAnnotation{
  name="haxlanalyser.FeatureSetFeature";
  version=1;
}
union FeatureSetFeature {
  1: FeatureSetFeature_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "FeatureSetFeature_with_")

typedef glean.Id EndpointName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.EndpointName";
  version=1;
}
union EndpointName {
  1: EndpointName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "EndpointName_with_")

typedef glean.Id Endpoint_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Endpoint";
  version=1;
}
union Endpoint {
  1: Endpoint_id id (hs.strict);
  2: Endpoint_key key;
  3: builtin.Unit get;
} (hs.prefix = "Endpoint_with_")

typedef glean.Id Edge_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Edge";
  version=1;
}
union Edge {
  1: Edge_id id (hs.strict);
  2: Edge_key key;
  3: builtin.Unit get;
} (hs.prefix = "Edge_with_")

typedef glean.Id ContextName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ContextName";
  version=1;
}
union ContextName {
  1: ContextName_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ContextName_with_")

typedef glean.Id Context_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Context";
  version=1;
}
union Context {
  1: Context_id id (hs.strict);
  2: Context_key key;
  3: builtin.Unit get;
} (hs.prefix = "Context_with_")

typedef glean.Id ConfigeratorFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ConfigeratorFetch";
  version=1;
}
union ConfigeratorFetch {
  1: ConfigeratorFetch_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ConfigeratorFetch_with_")

typedef glean.Id ClassifierRead_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ClassifierRead";
  version=1;
}
union ClassifierRead {
  1: ClassifierRead_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ClassifierRead_with_")

typedef glean.Id ClassifierFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ClassifierFetch";
  version=1;
}
union ClassifierFetch {
  1: ClassifierFetch_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ClassifierFetch_with_")

typedef glean.Id Call_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Call";
  version=1;
}
union Call {
  1: Call_id id (hs.strict);
  2: Call_key key;
  3: builtin.Unit get;
} (hs.prefix = "Call_with_")

typedef glean.Id ACDCPropertyAccess_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ACDCPropertyAccess";
  version=1;
}
union ACDCPropertyAccess {
  1: ACDCPropertyAccess_id id (hs.strict);
  2: string key;
  3: builtin.Unit get;
} (hs.prefix = "ACDCPropertyAccess_with_")

typedef glean.Id ACDCProperty_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ACDCProperty";
  version=1;
}
union ACDCProperty {
  1: ACDCProperty_id id (hs.strict);
  2: ACDCProperty_key key;
  3: builtin.Unit get;
} (hs.prefix = "ACDCProperty_with_")

enum TallyCounterType {
  counter = 0,
  uniqueCounter = 1,
  topK = 2,
  quantiles = 3,
  uniqueQuantiles = 4,
  firstN = 5,
  lastN = 6,
  moments = 7,
  infiniteCounter = 8
} (hs.nounknown)

struct TallyConfig_key {
  1: optional TallyName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Loc source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Tally {
  1: optional string name;
  2: optional TallyCounterType type;
}

struct Response {
  1: optional JankyJSONResponse janky_json (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ScubaResponse scuba (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional ThriftResponse thrift (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional PiranhaResponse piranha (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional TallyResponse tally (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional LogFeatureResponse log_feature (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional RestrictionResponse restriction (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional HiveResponse hive (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional ScribeResponse scribe (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  10: bool any = false;
}

struct Policy_key {
  1: optional PolicyName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Loc source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct InputFetch_key {
  1: optional InputKey key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Loc source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Fetch {
  1: optional ThriftFetch thrift (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional ConfigeratorFetch configerator (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional TallyFetch tally (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional ClassifierFetch classifier (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional SitevarFetch sitevar (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: bool any = false;
}

struct Node {
  1: optional hs.SourceModule hs_module (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional hs.DefinitionName definition (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional hs.ClassName typeclass (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional hs.ClassInstance class_instance (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional Context context (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  6: optional Policy policy (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  7: optional TallyConfig tally (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  8: optional Endpoint endpoint (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  9: optional ACDCProperty acdc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  10: optional InputFetch input_fetch (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  11: optional Response response (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  12: optional Fetch fetch (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  13: optional ClassifierRead classifier_read (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  14: optional LaserDataset laser_dataset (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  15: optional TestFile test_file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  16: optional Call call (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  17: optional ACDCPropertyAccess acdc_access (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  18: optional FeatureSetFeature feature (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  19: bool any = false;
}

struct Endpoint_key {
  1: optional EndpointName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Loc source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Edge_key {
  1: optional Node origin (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Node dest (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Context_key {
  1: optional ContextName name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.Loc source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Call_key {
  1: optional string ref;
  2: optional string qname;
  3: optional src.Loc loc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct ACDCProperty_key {
  1: optional string name;
  2: optional src.Loc source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
