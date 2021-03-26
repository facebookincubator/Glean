// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/thrift/builtin.thrift"
include "glean/schema/thrift/hs.thrift"
include "glean/schema/thrift/src.thrift"

namespace cpp2 facebook.glean.schema.haxlanalyser
namespace hs Glean.Schema
namespace php glean_schema_haxlanalyser
namespace py glean.schema.haxlanalyser
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.haxlanalyser
namespace rust glean_schema_haxlanalyser

hs_include "glean/schema/thrift/haxlanalyser_include.hs"
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
struct ThriftResponse {
  1: ThriftResponse_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ThriftFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ThriftFetch";
  version=1;
}
struct ThriftFetch {
  1: ThriftFetch_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id TestFile_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TestFile";
  version=1;
}
struct TestFile {
  1: TestFile_id id (hs.strict);
  2: optional src.File key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TallyName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyName";
  version=1;
}
struct TallyName {
  1: TallyName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id TallyConfig_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyConfig";
  version=1;
}
struct TallyConfig {
  1: TallyConfig_id id (hs.strict);
  2: optional TallyConfig_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TallyFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyFetch";
  version=1;
}
struct TallyFetch {
  1: TallyFetch_id id (hs.strict);
  2: optional Tally key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id TallyResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.TallyResponse";
  version=1;
}
struct TallyResponse {
  1: TallyResponse_id id (hs.strict);
  2: optional Tally key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id SitevarFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.SitevarFetch";
  version=1;
}
struct SitevarFetch {
  1: SitevarFetch_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ScubaResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ScubaResponse";
  version=1;
}
struct ScubaResponse {
  1: ScubaResponse_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ScribeResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ScribeResponse";
  version=1;
}
struct ScribeResponse {
  1: ScribeResponse_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id RestrictionResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.RestrictionResponse";
  version=1;
}
struct RestrictionResponse {
  1: RestrictionResponse_id id (hs.strict);
  2: optional glean.Nat key;
}

typedef glean.Id PolicyName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.PolicyName";
  version=1;
}
struct PolicyName {
  1: PolicyName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Policy_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Policy";
  version=1;
}
struct Policy {
  1: Policy_id id (hs.strict);
  2: optional Policy_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PiranhaResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.PiranhaResponse";
  version=1;
}
struct PiranhaResponse {
  1: PiranhaResponse_id id (hs.strict);
  2: optional glean.Nat key;
}

typedef glean.Id LogFeatureResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.LogFeatureResponse";
  version=1;
}
struct LogFeatureResponse {
  1: LogFeatureResponse_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id LaserDataset_id

@glean.PredicateAnnotation{
  name="haxlanalyser.LaserDataset";
  version=1;
}
struct LaserDataset {
  1: LaserDataset_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id JankyJSONResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.JankyJSONResponse";
  version=1;
}
struct JankyJSONResponse {
  1: JankyJSONResponse_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id InputKey_id

@glean.PredicateAnnotation{
  name="haxlanalyser.InputKey";
  version=1;
}
struct InputKey {
  1: InputKey_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id InputFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.InputFetch";
  version=1;
}
struct InputFetch {
  1: InputFetch_id id (hs.strict);
  2: optional InputFetch_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id HiveResponse_id

@glean.PredicateAnnotation{
  name="haxlanalyser.HiveResponse";
  version=1;
}
struct HiveResponse {
  1: HiveResponse_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id FeatureSetFeature_id

@glean.PredicateAnnotation{
  name="haxlanalyser.FeatureSetFeature";
  version=1;
}
struct FeatureSetFeature {
  1: FeatureSetFeature_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id EndpointName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.EndpointName";
  version=1;
}
struct EndpointName {
  1: EndpointName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Endpoint_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Endpoint";
  version=1;
}
struct Endpoint {
  1: Endpoint_id id (hs.strict);
  2: optional Endpoint_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Edge_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Edge";
  version=1;
}
struct Edge {
  1: Edge_id id (hs.strict);
  2: optional Edge_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ContextName_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ContextName";
  version=1;
}
struct ContextName {
  1: ContextName_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Context_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Context";
  version=1;
}
struct Context {
  1: Context_id id (hs.strict);
  2: optional Context_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ConfigeratorFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ConfigeratorFetch";
  version=1;
}
struct ConfigeratorFetch {
  1: ConfigeratorFetch_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ClassifierRead_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ClassifierRead";
  version=1;
}
struct ClassifierRead {
  1: ClassifierRead_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ClassifierFetch_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ClassifierFetch";
  version=1;
}
struct ClassifierFetch {
  1: ClassifierFetch_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id Call_id

@glean.PredicateAnnotation{
  name="haxlanalyser.Call";
  version=1;
}
struct Call {
  1: Call_id id (hs.strict);
  2: optional Call_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id ACDCPropertyAccess_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ACDCPropertyAccess";
  version=1;
}
struct ACDCPropertyAccess {
  1: ACDCPropertyAccess_id id (hs.strict);
  2: optional string key;
}

typedef glean.Id ACDCProperty_id

@glean.PredicateAnnotation{
  name="haxlanalyser.ACDCProperty";
  version=1;
}
struct ACDCProperty {
  1: ACDCProperty_id id (hs.strict);
  2: optional ACDCProperty_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

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
  1: TallyName name;
  2: src.Loc source;
}

struct Tally {
  1: string name;
  2: TallyCounterType type;
}

union Response {
  1: JankyJSONResponse janky_json;
  2: ScubaResponse scuba;
  3: ThriftResponse thrift;
  4: PiranhaResponse piranha;
  5: TallyResponse tally;
  6: LogFeatureResponse log_feature;
  7: RestrictionResponse restriction;
  8: HiveResponse hive;
  9: ScribeResponse scribe;
} (hs.nonempty)

struct Policy_key {
  1: PolicyName name;
  2: src.Loc source;
}

struct InputFetch_key {
  1: InputKey key;
  2: src.Loc source;
}

union Fetch {
  1: ThriftFetch thrift;
  2: ConfigeratorFetch configerator;
  3: TallyFetch tally;
  4: ClassifierFetch classifier;
  5: SitevarFetch sitevar;
} (hs.nonempty)

union Node {
  1: hs.SourceModule hs_module;
  2: hs.DefinitionName definition;
  3: hs.ClassName typeclass;
  4: hs.ClassInstance class_instance;
  5: Context context;
  6: Policy policy;
  7: TallyConfig tally;
  8: Endpoint endpoint;
  9: ACDCProperty acdc;
  10: InputFetch input_fetch;
  11: Response response;
  12: Fetch fetch;
  13: ClassifierRead classifier_read;
  14: LaserDataset laser_dataset;
  15: TestFile test_file;
  16: Call call;
  17: ACDCPropertyAccess acdc_access;
  18: FeatureSetFeature feature;
} (hs.nonempty)

struct Endpoint_key {
  1: EndpointName name;
  2: src.Loc source;
}

struct Edge_key {
  1: Node origin;
  2: Node dest;
}

struct Context_key {
  1: ContextName name;
  2: src.Loc source;
}

struct Call_key {
  1: string ref;
  2: string qname;
  3: src.Loc loc;
}

struct ACDCProperty_key {
  1: string name;
  2: src.Loc source;
}
