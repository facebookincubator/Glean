// To re-generate this file:
//
//   ./glean/schema/sync
//
// @generated
// @nolint

include "glean/if/glean.thrift"
include "glean/schema/v2/builtin.thrift"
include "glean/schema/v2/code.thrift"
include "glean/schema/v2/flow.thrift"
include "glean/schema/v2/src.thrift"

namespace cpp2 facebook.glean.schema.codemarkup
namespace hs Glean.Schema
namespace php glean_schema_codemarkup
namespace py glean.schema.codemarkup
namespace py3 glean.schema
namespace java.swift com.facebook.glean.schema.codemarkup
namespace rust glean_schema_codemarkup

hs_include "glean/schema/v2/codemarkup_include.hs"
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
  "FlowTypeImportXRef": 9,
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
struct HackEntityUses {
  1: HackEntityUses_id id (hs.strict);
  2: optional HackEntityUses_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowXRefInfo_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowXRefInfo";
  version=9;
}
struct FlowXRefInfo {
  1: FlowXRefInfo_id id (hs.strict);
  2: optional FlowXRefInfo_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowXRefDeclInfo_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowXRefDeclInfo";
  version=9;
}
struct FlowXRefDeclInfo {
  1: FlowXRefDeclInfo_id id (hs.strict);
  2: optional FlowXRefDeclInfo_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowTypeImportXRef_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowTypeImportXRef";
  version=9;
}
struct FlowTypeImportXRef {
  1: FlowTypeImportXRef_id id (hs.strict);
  2: optional FlowTypeImportXRef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowTypeExportLocation_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowTypeExportLocation";
  version=9;
}
struct FlowTypeExportLocation {
  1: FlowTypeExportLocation_id id (hs.strict);
  2: optional FlowTypeExportLocation_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowSameModule_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowSameModule";
  version=9;
}
struct FlowSameModule {
  1: FlowSameModule_id id (hs.strict);
  2: optional FlowSameModule_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowModuleNamespaceXRef_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowModuleNamespaceXRef";
  version=9;
}
struct FlowModuleNamespaceXRef {
  1: FlowModuleNamespaceXRef_id id (hs.strict);
  2: optional FlowModuleNamespaceXRef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowModuleExportLocation_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowModuleExportLocation";
  version=9;
}
struct FlowModuleExportLocation {
  1: FlowModuleExportLocation_id id (hs.strict);
  2: optional FlowModuleExportLocation_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowImportXRef_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowImportXRef";
  version=9;
}
struct FlowImportXRef {
  1: FlowImportXRef_id id (hs.strict);
  2: optional FlowImportXRef_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowDeclarationInfo_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowDeclarationInfo";
  version=9;
}
struct FlowDeclarationInfo {
  1: FlowDeclarationInfo_id id (hs.strict);
  2: optional FlowDeclarationInfo_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowCompatibleModuleExport_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowCompatibleModuleExport";
  version=9;
}
struct FlowCompatibleModuleExport {
  1: FlowCompatibleModuleExport_id id (hs.strict);
  2: optional FlowCompatibleModuleExport_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EntityUses_id

@glean.PredicateAnnotation{
  name="codemarkup.EntityUses";
  version=9;
}
struct EntityUses {
  1: EntityUses_id id (hs.strict);
  2: optional EntityUses_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FileDirectXRefs";
  version=9;
}
struct FileDirectXRefs {
  1: FileDirectXRefs_id id (hs.strict);
  2: optional FileDirectXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FileEntityXRefs";
  version=9;
}
struct FileEntityXRefs {
  1: FileEntityXRefs_id id (hs.strict);
  2: optional FileEntityXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowFileDirectXRefs";
  version=9;
}
struct FlowFileDirectXRefs {
  1: FlowFileDirectXRefs_id id (hs.strict);
  2: optional FlowFileDirectXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowFileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowFileEntityXRefs";
  version=9;
}
struct FlowFileEntityXRefs {
  1: FlowFileEntityXRefs_id id (hs.strict);
  2: optional FlowFileEntityXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id HackFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.HackFileDirectXRefs";
  version=9;
}
struct HackFileDirectXRefs {
  1: HackFileDirectXRefs_id id (hs.strict);
  2: optional HackFileDirectXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id HackFileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.HackFileEntityXRefs";
  version=9;
}
struct HackFileEntityXRefs {
  1: HackFileEntityXRefs_id id (hs.strict);
  2: optional HackFileEntityXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id HaskellFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.HaskellFileDirectXRefs";
  version=9;
}
struct HaskellFileDirectXRefs {
  1: HaskellFileDirectXRefs_id id (hs.strict);
  2: optional HaskellFileDirectXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PythonFileDirectXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonFileDirectXRefs";
  version=9;
}
struct PythonFileDirectXRefs {
  1: PythonFileDirectXRefs_id id (hs.strict);
  2: optional PythonFileDirectXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PythonFileEntityXRefs_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonFileEntityXRefs";
  version=9;
}
struct PythonFileEntityXRefs {
  1: PythonFileEntityXRefs_id id (hs.strict);
  2: optional PythonFileEntityXRefs_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id EntityToDeclaration_id

@glean.PredicateAnnotation{
  name="codemarkup.EntityToDeclaration";
  version=9;
}
struct EntityToDeclaration {
  1: EntityToDeclaration_id id (hs.strict);
  2: optional EntityToDeclaration_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.FileDeclarations";
  version=9;
}
struct FileDeclarations {
  1: FileDeclarations_id id (hs.strict);
  2: optional FileDeclarations_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileEntities_id

@glean.PredicateAnnotation{
  name="codemarkup.FileEntities";
  version=9;
}
struct FileEntities {
  1: FileEntities_id id (hs.strict);
  2: optional FileEntities_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowFileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowFileDeclarations";
  version=9;
}
struct FlowFileDeclarations {
  1: FlowFileDeclarations_id id (hs.strict);
  2: optional FlowFileDeclarations_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FlowResolve_id

@glean.PredicateAnnotation{
  name="codemarkup.FlowResolve";
  version=9;
}
struct FlowResolve {
  1: FlowResolve_id id (hs.strict);
  2: optional FlowResolve_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id HackFileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.HackFileDeclarations";
  version=9;
}
struct HackFileDeclarations {
  1: HackFileDeclarations_id id (hs.strict);
  2: optional HackFileDeclarations_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id HackResolve_id

@glean.PredicateAnnotation{
  name="codemarkup.HackResolve";
  version=9;
}
struct HackResolve {
  1: HackResolve_id id (hs.strict);
  2: optional HackResolve_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PythonFileDeclarations_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonFileDeclarations";
  version=9;
}
struct PythonFileDeclarations {
  1: PythonFileDeclarations_id id (hs.strict);
  2: optional PythonFileDeclarations_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id PythonResolve_id

@glean.PredicateAnnotation{
  name="codemarkup.PythonResolve";
  version=9;
}
struct PythonResolve {
  1: PythonResolve_id id (hs.strict);
  2: optional PythonResolve_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id Resolve_id

@glean.PredicateAnnotation{
  name="codemarkup.Resolve";
  version=9;
}
struct Resolve {
  1: Resolve_id id (hs.strict);
  2: optional Resolve_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

typedef glean.Id FileAnnotations_id

@glean.PredicateAnnotation{
  name="codemarkup.FileAnnotations";
  version=9;
}
struct FileAnnotations {
  1: FileAnnotations_id id (hs.strict);
  2: optional FileAnnotations_key key (cpp.ref = "true", cpp2.ref = "true", rust.box, swift.recursive_reference = "true");
}

union LinkTo {
  1: src.FileLocation localRepo;
} (hs.nonempty)

struct HackEntityUses_key {
  1: code.Entity target;
  2: src.File file;
  3: src.ByteSpan span;
}

struct FlowXRefInfo_key {
  1: flow.XRef ref;
  2: flow.Range srcLoc;
  3: flow.Name name;
  4: flow.Range targetLoc;
}

struct FlowXRefDeclInfo_key {
  1: flow.XRef ref;
  2: flow.Range srcLoc;
  3: flow.Name name;
  4: flow.Range targetLoc;
  5: flow.SomeDeclaration entity;
}

struct FlowTypeImportXRef_key {
  1: flow.TypeDeclaration local;
  2: src.File targetFile;
  3: src.ByteSpan targetSpan;
}

struct FlowTypeExportLocation_key {
  1: flow.ModuleTypeExport moduleTypeExport;
  2: src.File file;
  3: src.ByteSpan span;
}

struct FlowSameModule_key {
  1: flow.Module left;
  2: flow.Module right;
}

struct FlowModuleNamespaceXRef_key {
  1: flow.Declaration local;
  2: src.File file;
}

struct FlowModuleExportLocation_key {
  1: flow.ModuleExport local;
  2: src.File file;
  3: src.ByteSpan span;
}

struct FlowImportXRef_key {
  1: flow.Declaration local;
  2: src.File targetFile;
  3: src.ByteSpan targetSpan;
}

struct FlowDeclarationInfo_key {
  1: flow.SomeDeclaration decl;
  2: flow.Name name;
  3: src.ByteSpan span;
}

struct FlowCompatibleModuleExport_key {
  1: flow.ModuleExport left;
  2: flow.ModuleExport right;
}

struct EntityUses_key {
  1: code.Entity target;
  2: src.File file;
  3: src.ByteSpan span;
}

struct Declaration {
  1: string name;
  2: src.File file;
  3: src.ByteSpan span;
}

struct DirectXRef {
  1: Declaration target;
  2: src.ByteSpan source;
}

struct FileDirectXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
}

struct FileEntityXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
  3: code.Entity entity;
}

struct FlowFileDirectXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
}

struct FlowFileEntityXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
  3: code.Entity entity;
}

struct HackFileDirectXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
}

struct HackFileEntityXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
  3: code.Entity entity;
}

struct HaskellFileDirectXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
}

struct PythonFileDirectXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
}

struct PythonFileEntityXRefs_key {
  1: src.File file;
  2: DirectXRef xref;
  3: code.Entity entity;
}

struct EntityToDeclaration_key {
  1: code.Entity entity;
  2: Declaration decl;
}

struct FileDeclarations_key {
  1: src.File file;
  2: Declaration decl;
}

struct FileEntities_key {
  1: src.File file;
  2: Declaration decl;
  3: code.Entity entity;
}

struct FlowFileDeclarations_key {
  1: src.File file;
  2: Declaration declaration;
}

struct FlowResolve_key {
  1: Declaration decl;
  2: code.Entity entity;
}

struct HackFileDeclarations_key {
  1: src.File file;
  2: Declaration declaration;
}

struct HackResolve_key {
  1: Declaration decl;
  2: code.Entity entity;
}

struct PythonFileDeclarations_key {
  1: src.File file;
  2: Declaration declaration;
}

struct PythonResolve_key {
  1: Declaration decl;
  2: code.Entity entity;
}

struct Resolve_key {
  1: Declaration decl;
  2: code.Entity entity;
}

struct Annotation {
  1: src.ByteSpan span;
  2: string shortName;
  3: optional LinkTo linkTo;
}

struct FileAnnotations_key {
  1: src.File file;
  2: Annotation annotation;
}
