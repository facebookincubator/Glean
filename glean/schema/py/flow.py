# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.flow.types import (
    FlowTypeEntityImportUses,
    TypeExport,
    ImportDeclaration,
    DeclarationLocation,
    DeclarationInfo,
    FileDeclaration,
    ModuleTypeExport,
    LocalDeclarationReference,
    FlowTypeImportXRef,
    Name,
    SearchByModule,
    Range,
    Declaration,
    DeclarationUses,
    SourceOfTypeExport,
    FlowCompatibleExport,
    FlowEntityUsesAll,
    NameLowerCase,
    SearchByFileModule,
    Module,
    FlowSameModule,
    FlowModuleNamespaceXRef,
    Documentation,
    SearchByName,
    StringToFileModule,
    FlowExportLocation,
    FlowXRefDeclInfo,
    SearchTypeByModuleExport,
    FlowEntityImportUses,
    Export,
    TypeImportDeclaration,
    FlowImportXRef,
    ModuleExport,
    MemberDeclarationInfo,
    TypeDeclarationInfo,
    FileXRef,
    SourceOfExport,
    MemberDeclaration,
    Type,
    FileOfStringModule,
    TypeDeclaration,
    DeclarationNameSpan,
    MemberDeclarationReference,
    FlowTypeExportLocation,
    TypeDeclarationReference,
)


class FlowFlowTypeEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowTypeEntityImportUses.3 {{ target = _, local = _ }}", FlowTypeEntityImportUses

  @staticmethod
  def angle_query(*, target: Tuple[()], local: Tuple[()]) -> "FlowFlowTypeEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeExport.3 {json.dumps(key)}", TypeExport

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "FlowTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.ImportDeclaration.3 {{ declaration = _, import_ = _ }}", ImportDeclaration

  @staticmethod
  def angle_query(*, declaration: Tuple[()], import_: Tuple[()]) -> "FlowImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationLocation.3 {{ decl = _, file = _, span = _ }}", DeclarationLocation

  @staticmethod
  def angle_query(*, decl: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "FlowDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationInfo.3 {{ declaration = _, type = _, documentation = _ }}", DeclarationInfo

  @staticmethod
  def angle_query(*, declaration: Tuple[()], type: Tuple[()], documentation: Tuple[()]) -> "FlowDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FileDeclaration.3 {{ file = _, declaration = _ }}", FileDeclaration

  @staticmethod
  def angle_query(*, file: Tuple[()], declaration: Tuple[()]) -> "FlowFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.ModuleTypeExport.3 {{ module = _, typeExport = _ }}", ModuleTypeExport

  @staticmethod
  def angle_query(*, module: Tuple[()], typeExport: Tuple[()]) -> "FlowModuleTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowLocalDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.LocalDeclarationReference.3 {{ declaration = _, loc = _ }}", LocalDeclarationReference

  @staticmethod
  def angle_query(*, declaration: Tuple[()], loc: Tuple[()]) -> "FlowLocalDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowTypeImportXRef.3 {{ local = _, entity = _, targetFile = _, targetSpan = _ }}", FlowTypeImportXRef

  @staticmethod
  def angle_query(*, local: Tuple[()], entity: Tuple[()], targetFile: Tuple[()], targetSpan: Tuple[()]) -> "FlowFlowTypeImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Name.3 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: str) -> "FlowName":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchByModule.3 {{ string_ = _, name = _, decl = _ }}", SearchByModule

  @staticmethod
  def angle_query(*, string_: str, name: Tuple[()], decl: Tuple[()]) -> "FlowSearchByModule":
    raise Exception("this function can only be called from @angle_query")

class FlowRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Range.3 {{ module = _, span = _ }}", Range

  @staticmethod
  def angle_query(*, module: Tuple[()], span: Tuple[()]) -> "FlowRange":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Declaration.3 {{ name = _, loc = _ }}", Declaration

  @staticmethod
  def angle_query(*, name: Tuple[()], loc: Tuple[()]) -> "FlowDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationUses.3 {{ target = _, file = _, use = _ }}", DeclarationUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], use: Tuple[()]) -> "FlowDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SourceOfTypeExport.3 {{ moduleTypeExport = _, source = _ }}", SourceOfTypeExport

  @staticmethod
  def angle_query(*, moduleTypeExport: Tuple[()], source: Tuple[()]) -> "FlowSourceOfTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowCompatibleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowCompatibleExport.3 {{ left = _, right = _ }}", FlowCompatibleExport

  @staticmethod
  def angle_query(*, left: Tuple[()], right: Tuple[()]) -> "FlowFlowCompatibleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityUsesAll(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowEntityUsesAll.3 {{ target = _, file = _, span = _ }}", FlowEntityUsesAll

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "FlowFlowEntityUsesAll":
    raise Exception("this function can only be called from @angle_query")

class FlowNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.NameLowerCase.3 {{ nameLowerCase = _, name = _ }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: str, name: Tuple[()]) -> "FlowNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchByFileModule.3 {{ file = _, name = _, decl = _ }}", SearchByFileModule

  @staticmethod
  def angle_query(*, file: Tuple[()], name: Tuple[()], decl: Tuple[()]) -> "FlowSearchByFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Module.3 {json.dumps(key)}", Module

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "FlowModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowSameModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowSameModule.3 {{ left = _, right = _ }}", FlowSameModule

  @staticmethod
  def angle_query(*, left: Tuple[()], right: Tuple[()]) -> "FlowFlowSameModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowModuleNamespaceXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowModuleNamespaceXRef.3 {{ local = _, entity = _, file = _ }}", FlowModuleNamespaceXRef

  @staticmethod
  def angle_query(*, local: Tuple[()], entity: Tuple[()], file: Tuple[()]) -> "FlowFlowModuleNamespaceXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Documentation.3 {json.dumps(key)}", Documentation

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "FlowDocumentation":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchByName.3 {{ name = _, decl = _ }}", SearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()], decl: Tuple[()]) -> "FlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class FlowStringToFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.StringToFileModule.3 {{ string_ = _, file = _ }}", StringToFileModule

  @staticmethod
  def angle_query(*, string_: str, file: Tuple[()]) -> "FlowStringToFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowExportLocation.3 {{ module = _, export_ = _, entity = _, file = _, span = _ }}", FlowExportLocation

  @staticmethod
  def angle_query(*, module: Tuple[()], export_: Tuple[()], entity: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "FlowFlowExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowXRefDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowXRefDeclInfo.3 {{ ref = _, srcLoc = _, name = _, targetLoc = _, entity = _ }}", FlowXRefDeclInfo

  @staticmethod
  def angle_query(*, ref: Tuple[()], srcLoc: Tuple[()], name: Tuple[()], targetLoc: Tuple[()], entity: Tuple[()]) -> "FlowFlowXRefDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchTypeByModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchTypeByModuleExport.3 {{ string_ = _, name = _, decl = _ }}", SearchTypeByModuleExport

  @staticmethod
  def angle_query(*, string_: str, name: Tuple[()], decl: Tuple[()]) -> "FlowSearchTypeByModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowEntityImportUses.3 {{ target = _, local = _ }}", FlowEntityImportUses

  @staticmethod
  def angle_query(*, target: Tuple[()], local: Tuple[()]) -> "FlowFlowEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Export.3 {json.dumps(key)}", Export

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "FlowExport":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeImportDeclaration.3 {{ typeDeclaration = _, import_ = _ }}", TypeImportDeclaration

  @staticmethod
  def angle_query(*, typeDeclaration: Tuple[()], import_: Tuple[()]) -> "FlowTypeImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowImportXRef.3 {{ local = _, entity = _, targetFile = _, targetSpan = _ }}", FlowImportXRef

  @staticmethod
  def angle_query(*, local: Tuple[()], entity: Tuple[()], targetFile: Tuple[()], targetSpan: Tuple[()]) -> "FlowFlowImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.ModuleExport.3 {{ module = _, export_ = _ }}", ModuleExport

  @staticmethod
  def angle_query(*, module: Tuple[()], export_: Tuple[()]) -> "FlowModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.MemberDeclarationInfo.3 {{ memberDeclaration = _, type = _, documentation = _ }}", MemberDeclarationInfo

  @staticmethod
  def angle_query(*, memberDeclaration: Tuple[()], type: Tuple[()], documentation: Tuple[()]) -> "FlowMemberDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeDeclarationInfo.3 {{ typeDeclaration = _, type = _, documentation = _ }}", TypeDeclarationInfo

  @staticmethod
  def angle_query(*, typeDeclaration: Tuple[()], type: Tuple[()], documentation: Tuple[()]) -> "FlowTypeDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FileXRef.3 {{ file = _, ref = _ }}", FileXRef

  @staticmethod
  def angle_query(*, file: Tuple[()], ref: Tuple[()]) -> "FlowFileXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SourceOfExport.3 {{ moduleExport = _, source = _ }}", SourceOfExport

  @staticmethod
  def angle_query(*, moduleExport: Tuple[()], source: Tuple[()]) -> "FlowSourceOfExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.MemberDeclaration.3 {{ name = _, loc = _ }}", MemberDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], loc: Tuple[()]) -> "FlowMemberDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Type.3 {json.dumps(key)}", Type

  @staticmethod
  def angle_query(*, arg: str) -> "FlowType":
    raise Exception("this function can only be called from @angle_query")

class FlowFileOfStringModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FileOfStringModule.3 {{ file = _, string_ = _ }}", FileOfStringModule

  @staticmethod
  def angle_query(*, file: Tuple[()], string_: str) -> "FlowFileOfStringModule":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeDeclaration.3 {{ name = _, loc = _ }}", TypeDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()], loc: Tuple[()]) -> "FlowTypeDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationNameSpan.3 {{ decl = _, name = _, span = _ }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Tuple[()], name: Tuple[()], span: Tuple[()]) -> "FlowDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.MemberDeclarationReference.3 {{ memberDeclaration = _, loc = _ }}", MemberDeclarationReference

  @staticmethod
  def angle_query(*, memberDeclaration: Tuple[()], loc: Tuple[()]) -> "FlowMemberDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowTypeExportLocation.3 {{ moduleTypeExport = _, entity = _, file = _, span = _ }}", FlowTypeExportLocation

  @staticmethod
  def angle_query(*, moduleTypeExport: Tuple[()], entity: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "FlowFlowTypeExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeDeclarationReference.3 {{ typeDeclaration = _, loc = _ }}", TypeDeclarationReference

  @staticmethod
  def angle_query(*, typeDeclaration: Tuple[()], loc: Tuple[()]) -> "FlowTypeDeclarationReference":
    raise Exception("this function can only be called from @angle_query")


