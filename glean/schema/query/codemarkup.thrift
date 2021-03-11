// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/query/builtin.thrift"
include "glean/schema/query/code.thrift"
include "glean/schema/query/flow.thrift"
include "glean/schema/query/src.thrift"

namespace cpp2 facebook.glean.schema.query.codemarkup
namespace hs Glean.Schema.Query
namespace php glean_schema_query_codemarkup
namespace py glean.schema.query.codemarkup
namespace py3 glean.schema.query
namespace java.swift com.facebook.glean.schema.query.codemarkup
namespace rust glean_schema_query_codemarkup

hs_include "glean/schema/query/codemarkup_include.hs"
const map<string, i64> PREDICATE_VERSIONS = {
  "FlowModuleNamespaceXRef": 9,
  "HackEntityUses": 9,
  "FileDeclarations": 9,
  "FlowFileEntityXRefs": 9,
  "HaskellFileDirectXRefs": 9,
  "PythonFileDirectXRefs": 9,
  "PythonResolve": 9,
  "EntityToDeclaration": 9,
  "FileEntities": 9,
  "FlowXRefDeclInfo": 9,
  "FlowResolve": 9,
  "Resolve": 9,
  "HackFileDeclarations": 9,
  "FlowCompatibleModuleExport": 9,
  "FlowSameModule": 9,
  "FileAnnotations": 9,
  "FlowXRefInfo": 9,
  "HackResolve": 9,
  "HackFileDirectXRefs": 9,
  "EntityUses": 9,
  "PythonFileEntityXRefs": 9,
  "FlowTypeExportLocation": 9,
  "FlowFileDirectXRefs": 9,
  "HackFileEntityXRefs": 9,
  "FlowImportXRef": 9,
  "FlowModuleExportLocation": 9,
  "PythonFileDeclarations": 9,
  "FileDirectXRefs": 9,
  "FlowDeclarationInfo": 9,
  "FileEntityXRefs": 9,
  "FlowFileDeclarations": 9,
}


typedef glean.Id HackEntityUses_id

@glean.PredicateAnnotation{
  name="codemarkup.HackEntityUses";
  version=9;
}
union HackEntityUses {
  1: HackEntityUses_id id (hs.strict);
  2: HackEntityUses_key key;
  3: builtin.Unit get;
} (hs.prefix = "HackEntityUses_with_")

typedef glean.Id FlowXRefInfo_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowXRefInfo";
  version=9;
}
union FlowXRefInfo {
  1: FlowXRefInfo_id id (hs.strict);
  2: FlowXRefInfo_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowXRefInfo_with_")

typedef glean.Id FlowXRefDeclInfo_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowXRefDeclInfo";
  version=9;
}
union FlowXRefDeclInfo {
  1: FlowXRefDeclInfo_id id (hs.strict);
  2: FlowXRefDeclInfo_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowXRefDeclInfo_with_")

typedef glean.Id FlowTypeExportLocation_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowTypeExportLocation";
  version=9;
}
union FlowTypeExportLocation {
  1: FlowTypeExportLocation_id id (hs.strict);
  2: FlowTypeExportLocation_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowTypeExportLocation_with_")

typedef glean.Id FlowSameModule_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowSameModule";
  version=9;
}
union FlowSameModule {
  1: FlowSameModule_id id (hs.strict);
  2: FlowSameModule_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowSameModule_with_")

typedef glean.Id FlowModuleNamespaceXRef_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowModuleNamespaceXRef";
  version=9;
}
union FlowModuleNamespaceXRef {
  1: FlowModuleNamespaceXRef_id id (hs.strict);
  2: FlowModuleNamespaceXRef_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowModuleNamespaceXRef_with_")

typedef glean.Id FlowModuleExportLocation_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowModuleExportLocation";
  version=9;
}
union FlowModuleExportLocation {
  1: FlowModuleExportLocation_id id (hs.strict);
  2: FlowModuleExportLocation_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowModuleExportLocation_with_")

typedef glean.Id FlowImportXRef_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowImportXRef";
  version=9;
}
union FlowImportXRef {
  1: FlowImportXRef_id id (hs.strict);
  2: FlowImportXRef_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowImportXRef_with_")

typedef glean.Id FlowDeclarationInfo_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowDeclarationInfo";
  version=9;
}
union FlowDeclarationInfo {
  1: FlowDeclarationInfo_id id (hs.strict);
  2: FlowDeclarationInfo_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowDeclarationInfo_with_")

typedef glean.Id FlowCompatibleModuleExport_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowCompatibleModuleExport";
  version=9;
}
union FlowCompatibleModuleExport {
  1: FlowCompatibleModuleExport_id id (hs.strict);
  2: FlowCompatibleModuleExport_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowCompatibleModuleExport_with_")

typedef glean.Id EntityUses_id

@glean.PredicateAnnotation{
  name="codemarkup.EntityUses";
  version=9;
}
union EntityUses {
  1: EntityUses_id id (hs.strict);
  2: EntityUses_key key;
  3: builtin.Unit get;
} (hs.prefix = "EntityUses_with_")

typedef glean.Id FileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FileDirectXRefs";
  version=9;
}
union FileDirectXRefs {
  1: FileDirectXRefs_id id (hs.strict);
  2: FileDirectXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileDirectXRefs_with_")

typedef glean.Id FileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FileEntityXRefs";
  version=9;
}
union FileEntityXRefs {
  1: FileEntityXRefs_id id (hs.strict);
  2: FileEntityXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileEntityXRefs_with_")

typedef glean.Id FlowFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowFileDirectXRefs";
  version=9;
}
union FlowFileDirectXRefs {
  1: FlowFileDirectXRefs_id id (hs.strict);
  2: FlowFileDirectXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowFileDirectXRefs_with_")

typedef glean.Id FlowFileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowFileEntityXRefs";
  version=9;
}
union FlowFileEntityXRefs {
  1: FlowFileEntityXRefs_id id (hs.strict);
  2: FlowFileEntityXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowFileEntityXRefs_with_")

typedef glean.Id HackFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.HackFileDirectXRefs";
  version=9;
}
union HackFileDirectXRefs {
  1: HackFileDirectXRefs_id id (hs.strict);
  2: HackFileDirectXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "HackFileDirectXRefs_with_")

typedef glean.Id HackFileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.HackFileEntityXRefs";
  version=9;
}
union HackFileEntityXRefs {
  1: HackFileEntityXRefs_id id (hs.strict);
  2: HackFileEntityXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "HackFileEntityXRefs_with_")

typedef glean.Id HaskellFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.HaskellFileDirectXRefs";
  version=9;
}
union HaskellFileDirectXRefs {
  1: HaskellFileDirectXRefs_id id (hs.strict);
  2: HaskellFileDirectXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "HaskellFileDirectXRefs_with_")

typedef glean.Id PythonFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonFileDirectXRefs";
  version=9;
}
union PythonFileDirectXRefs {
  1: PythonFileDirectXRefs_id id (hs.strict);
  2: PythonFileDirectXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "PythonFileDirectXRefs_with_")

typedef glean.Id PythonFileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonFileEntityXRefs";
  version=9;
}
union PythonFileEntityXRefs {
  1: PythonFileEntityXRefs_id id (hs.strict);
  2: PythonFileEntityXRefs_key key;
  3: builtin.Unit get;
} (hs.prefix = "PythonFileEntityXRefs_with_")

typedef glean.Id EntityToDeclaration_id

@glean.PredicateAnnotation{
  name="codemarkup.EntityToDeclaration";
  version=9;
}
union EntityToDeclaration {
  1: EntityToDeclaration_id id (hs.strict);
  2: EntityToDeclaration_key key;
  3: builtin.Unit get;
} (hs.prefix = "EntityToDeclaration_with_")

typedef glean.Id FileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.FileDeclarations";
  version=9;
}
union FileDeclarations {
  1: FileDeclarations_id id (hs.strict);
  2: FileDeclarations_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileDeclarations_with_")

typedef glean.Id FileEntities_id

@glean.PredicateAnnotation{
  name="codemarkup.FileEntities";
  version=9;
}
union FileEntities {
  1: FileEntities_id id (hs.strict);
  2: FileEntities_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileEntities_with_")

typedef glean.Id FlowFileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowFileDeclarations";
  version=9;
}
union FlowFileDeclarations {
  1: FlowFileDeclarations_id id (hs.strict);
  2: FlowFileDeclarations_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowFileDeclarations_with_")

typedef glean.Id FlowResolve_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowResolve";
  version=9;
}
union FlowResolve {
  1: FlowResolve_id id (hs.strict);
  2: FlowResolve_key key;
  3: builtin.Unit get;
} (hs.prefix = "FlowResolve_with_")

typedef glean.Id HackFileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.HackFileDeclarations";
  version=9;
}
union HackFileDeclarations {
  1: HackFileDeclarations_id id (hs.strict);
  2: HackFileDeclarations_key key;
  3: builtin.Unit get;
} (hs.prefix = "HackFileDeclarations_with_")

typedef glean.Id HackResolve_id

@glean.PredicateAnnotation{
  name="codemarkup.HackResolve";
  version=9;
}
union HackResolve {
  1: HackResolve_id id (hs.strict);
  2: HackResolve_key key;
  3: builtin.Unit get;
} (hs.prefix = "HackResolve_with_")

typedef glean.Id PythonFileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonFileDeclarations";
  version=9;
}
union PythonFileDeclarations {
  1: PythonFileDeclarations_id id (hs.strict);
  2: PythonFileDeclarations_key key;
  3: builtin.Unit get;
} (hs.prefix = "PythonFileDeclarations_with_")

typedef glean.Id PythonResolve_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonResolve";
  version=9;
}
union PythonResolve {
  1: PythonResolve_id id (hs.strict);
  2: PythonResolve_key key;
  3: builtin.Unit get;
} (hs.prefix = "PythonResolve_with_")

typedef glean.Id Resolve_id

@glean.PredicateAnnotation{
  name="codemarkup.Resolve";
  version=9;
}
union Resolve {
  1: Resolve_id id (hs.strict);
  2: Resolve_key key;
  3: builtin.Unit get;
} (hs.prefix = "Resolve_with_")

typedef glean.Id FileAnnotations_id

@glean.PredicateAnnotation{
  name="codemarkup.FileAnnotations";
  version=9;
}
union FileAnnotations {
  1: FileAnnotations_id id (hs.strict);
  2: FileAnnotations_key key;
  3: builtin.Unit get;
} (hs.prefix = "FileAnnotations_with_")

struct LinkTo {
  1: optional src.FileLocation localRepo (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: bool any = false;
}

struct HackEntityUses_key {
  1: optional code.Entity target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowXRefInfo_key {
  1: optional flow.XRef ref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional flow.Range srcLoc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional flow.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional flow.Range targetLoc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowXRefDeclInfo_key {
  1: optional flow.XRef ref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional flow.Range srcLoc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional flow.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  4: optional flow.Range targetLoc (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  5: optional flow.SomeDeclaration entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowTypeExportLocation_key {
  1: optional flow.ModuleTypeExport moduleTypeExport (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowSameModule_key {
  1: optional flow.Module left (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional flow.Module right (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowModuleNamespaceXRef_key {
  1: optional flow.Declaration local (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowModuleExportLocation_key {
  1: optional flow.ModuleExport local (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowImportXRef_key {
  1: optional flow.Declaration local (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File targetFile (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan targetSpan (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowDeclarationInfo_key {
  1: optional flow.SomeDeclaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional flow.Name name (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowCompatibleModuleExport_key {
  1: optional flow.ModuleExport left (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional flow.ModuleExport right (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EntityUses_key {
  1: optional code.Entity target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Declaration {
  1: optional string name;
  2: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct DirectXRef {
  1: optional Declaration target (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional src.ByteSpan source (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileDirectXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileEntityXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowFileDirectXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowFileEntityXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct HackFileDirectXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct HackFileEntityXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct HaskellFileDirectXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct PythonFileDirectXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct PythonFileEntityXRefs_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional DirectXRef xref (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct EntityToDeclaration_key {
  1: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileDeclarations_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileEntities_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowFileDeclarations_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FlowResolve_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct HackFileDeclarations_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct HackResolve_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct PythonFileDeclarations_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Declaration declaration (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct PythonResolve_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Resolve_key {
  1: optional Declaration decl (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional code.Entity entity (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct Annotation_linkTo {
  1: optional builtin.Unit nothing (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional LinkTo just (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  3: bool any = false;
}

struct Annotation {
  1: optional src.ByteSpan span (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional string shortName;
  3: optional Annotation_linkTo linkTo (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

struct FileAnnotations_key {
  1: optional src.File file (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
  2: optional Annotation annotation (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}
