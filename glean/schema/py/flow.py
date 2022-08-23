# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"flow.FlowTypeEntityImportUses.3 {{ }}", FlowTypeEntityImportUses
    return f"flow.FlowTypeEntityImportUses.3 {{ target = _, local = _ }}", FlowTypeEntityImportUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, local: Optional[Tuple[()]] = None) -> "FlowFlowTypeEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.TypeExport.3 {{ }}", TypeExport
    return f"flow.TypeExport.3 {json.dumps(key)}", TypeExport

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FlowTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.ImportDeclaration.3 {{ }}", ImportDeclaration
    return f"flow.ImportDeclaration.3 {{ declaration = _, import_ = _ }}", ImportDeclaration

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None) -> "FlowImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.DeclarationLocation.3 {{ }}", DeclarationLocation
    return f"flow.DeclarationLocation.3 {{ decl = _, file = _, span = _ }}", DeclarationLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "FlowDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.DeclarationInfo.3 {{ }}", DeclarationInfo
    return f"flow.DeclarationInfo.3 {{ declaration = _, type = _, documentation = _ }}", DeclarationInfo

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, documentation: Optional[Tuple[()]] = None) -> "FlowDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FileDeclaration.3 {{ }}", FileDeclaration
    return f"flow.FileDeclaration.3 {{ file = _, declaration = _ }}", FileDeclaration

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "FlowFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.ModuleTypeExport.3 {{ }}", ModuleTypeExport
    return f"flow.ModuleTypeExport.3 {{ module = _, typeExport = _ }}", ModuleTypeExport

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, typeExport: Optional[Tuple[()]] = None) -> "FlowModuleTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowLocalDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.LocalDeclarationReference.3 {{ }}", LocalDeclarationReference
    return f"flow.LocalDeclarationReference.3 {{ declaration = _, loc = _ }}", LocalDeclarationReference

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "FlowLocalDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowTypeImportXRef.3 {{ }}", FlowTypeImportXRef
    return f"flow.FlowTypeImportXRef.3 {{ local = _, entity = _, targetFile = _, targetSpan = _ }}", FlowTypeImportXRef

  @staticmethod
  def angle_query(*, local: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, targetFile: Optional[Tuple[()]] = None, targetSpan: Optional[Tuple[()]] = None) -> "FlowFlowTypeImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.Name.3 {{ }}", Name
    return f"flow.Name.3 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "FlowName":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.SearchByModule.3 {{ }}", SearchByModule
    return f"flow.SearchByModule.3 {{ string_ = _, name = _, decl = _ }}", SearchByModule

  @staticmethod
  def angle_query(*, string_: Optional[str] = None, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchByModule":
    raise Exception("this function can only be called from @angle_query")

class FlowRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.Range.3 {{ }}", Range
    return f"flow.Range.3 {{ module = _, span = _ }}", Range

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "FlowRange":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.Declaration.3 {{ }}", Declaration
    return f"flow.Declaration.3 {{ name = _, loc = _ }}", Declaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "FlowDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.DeclarationUses.3 {{ }}", DeclarationUses
    return f"flow.DeclarationUses.3 {{ target = _, file = _, use = _ }}", DeclarationUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, use: Optional[Tuple[()]] = None) -> "FlowDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.SourceOfTypeExport.3 {{ }}", SourceOfTypeExport
    return f"flow.SourceOfTypeExport.3 {{ moduleTypeExport = _, source = _ }}", SourceOfTypeExport

  @staticmethod
  def angle_query(*, moduleTypeExport: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "FlowSourceOfTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowCompatibleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowCompatibleExport.3 {{ }}", FlowCompatibleExport
    return f"flow.FlowCompatibleExport.3 {{ left = _, right = _ }}", FlowCompatibleExport

  @staticmethod
  def angle_query(*, left: Optional[Tuple[()]] = None, right: Optional[Tuple[()]] = None) -> "FlowFlowCompatibleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityUsesAll(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowEntityUsesAll.3 {{ }}", FlowEntityUsesAll
    return f"flow.FlowEntityUsesAll.3 {{ target = _, file = _, span = _ }}", FlowEntityUsesAll

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "FlowFlowEntityUsesAll":
    raise Exception("this function can only be called from @angle_query")

class FlowNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.NameLowerCase.3 {{ }}", NameLowerCase
    return f"flow.NameLowerCase.3 {{ nameLowerCase = _, name = _ }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "FlowNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.SearchByFileModule.3 {{ }}", SearchByFileModule
    return f"flow.SearchByFileModule.3 {{ file = _, name = _, decl = _ }}", SearchByFileModule

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchByFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.Module.3 {{ }}", Module
    return f"flow.Module.3 {json.dumps(key)}", Module

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FlowModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowSameModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowSameModule.3 {{ }}", FlowSameModule
    return f"flow.FlowSameModule.3 {{ left = _, right = _ }}", FlowSameModule

  @staticmethod
  def angle_query(*, left: Optional[Tuple[()]] = None, right: Optional[Tuple[()]] = None) -> "FlowFlowSameModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowModuleNamespaceXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowModuleNamespaceXRef.3 {{ }}", FlowModuleNamespaceXRef
    return f"flow.FlowModuleNamespaceXRef.3 {{ local = _, entity = _, file = _ }}", FlowModuleNamespaceXRef

  @staticmethod
  def angle_query(*, local: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "FlowFlowModuleNamespaceXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.Documentation.3 {{ }}", Documentation
    return f"flow.Documentation.3 {json.dumps(key)}", Documentation

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FlowDocumentation":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.SearchByName.3 {{ }}", SearchByName
    return f"flow.SearchByName.3 {{ name = _, decl = _ }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class FlowStringToFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.StringToFileModule.3 {{ }}", StringToFileModule
    return f"flow.StringToFileModule.3 {{ string_ = _, file = _ }}", StringToFileModule

  @staticmethod
  def angle_query(*, string_: Optional[str] = None, file: Optional[Tuple[()]] = None) -> "FlowStringToFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowExportLocation.3 {{ }}", FlowExportLocation
    return f"flow.FlowExportLocation.3 {{ module = _, export_ = _, entity = _, file = _, span = _ }}", FlowExportLocation

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, export_: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "FlowFlowExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowXRefDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowXRefDeclInfo.3 {{ }}", FlowXRefDeclInfo
    return f"flow.FlowXRefDeclInfo.3 {{ ref = _, srcLoc = _, name = _, targetLoc = _, entity = _ }}", FlowXRefDeclInfo

  @staticmethod
  def angle_query(*, ref: Optional[Tuple[()]] = None, srcLoc: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, targetLoc: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "FlowFlowXRefDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchTypeByModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.SearchTypeByModuleExport.3 {{ }}", SearchTypeByModuleExport
    return f"flow.SearchTypeByModuleExport.3 {{ string_ = _, name = _, decl = _ }}", SearchTypeByModuleExport

  @staticmethod
  def angle_query(*, string_: Optional[str] = None, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchTypeByModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowEntityImportUses.3 {{ }}", FlowEntityImportUses
    return f"flow.FlowEntityImportUses.3 {{ target = _, local = _ }}", FlowEntityImportUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, local: Optional[Tuple[()]] = None) -> "FlowFlowEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.Export.3 {{ }}", Export
    return f"flow.Export.3 {json.dumps(key)}", Export

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FlowExport":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.TypeImportDeclaration.3 {{ }}", TypeImportDeclaration
    return f"flow.TypeImportDeclaration.3 {{ typeDeclaration = _, import_ = _ }}", TypeImportDeclaration

  @staticmethod
  def angle_query(*, typeDeclaration: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None) -> "FlowTypeImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowImportXRef.3 {{ }}", FlowImportXRef
    return f"flow.FlowImportXRef.3 {{ local = _, entity = _, targetFile = _, targetSpan = _ }}", FlowImportXRef

  @staticmethod
  def angle_query(*, local: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, targetFile: Optional[Tuple[()]] = None, targetSpan: Optional[Tuple[()]] = None) -> "FlowFlowImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.ModuleExport.3 {{ }}", ModuleExport
    return f"flow.ModuleExport.3 {{ module = _, export_ = _ }}", ModuleExport

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, export_: Optional[Tuple[()]] = None) -> "FlowModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.MemberDeclarationInfo.3 {{ }}", MemberDeclarationInfo
    return f"flow.MemberDeclarationInfo.3 {{ memberDeclaration = _, type = _, documentation = _ }}", MemberDeclarationInfo

  @staticmethod
  def angle_query(*, memberDeclaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, documentation: Optional[Tuple[()]] = None) -> "FlowMemberDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.TypeDeclarationInfo.3 {{ }}", TypeDeclarationInfo
    return f"flow.TypeDeclarationInfo.3 {{ typeDeclaration = _, type = _, documentation = _ }}", TypeDeclarationInfo

  @staticmethod
  def angle_query(*, typeDeclaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, documentation: Optional[Tuple[()]] = None) -> "FlowTypeDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FileXRef.3 {{ }}", FileXRef
    return f"flow.FileXRef.3 {{ file = _, ref = _ }}", FileXRef

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, ref: Optional[Tuple[()]] = None) -> "FlowFileXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.SourceOfExport.3 {{ }}", SourceOfExport
    return f"flow.SourceOfExport.3 {{ moduleExport = _, source = _ }}", SourceOfExport

  @staticmethod
  def angle_query(*, moduleExport: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "FlowSourceOfExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.MemberDeclaration.3 {{ }}", MemberDeclaration
    return f"flow.MemberDeclaration.3 {{ name = _, loc = _ }}", MemberDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "FlowMemberDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.Type.3 {{ }}", Type
    return f"flow.Type.3 {json.dumps(key)}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "FlowType":
    raise Exception("this function can only be called from @angle_query")

class FlowFileOfStringModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FileOfStringModule.3 {{ }}", FileOfStringModule
    return f"flow.FileOfStringModule.3 {{ file = _, string_ = _ }}", FileOfStringModule

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, string_: Optional[str] = None) -> "FlowFileOfStringModule":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.TypeDeclaration.3 {{ }}", TypeDeclaration
    return f"flow.TypeDeclaration.3 {{ name = _, loc = _ }}", TypeDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "FlowTypeDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.DeclarationNameSpan.3 {{ }}", DeclarationNameSpan
    return f"flow.DeclarationNameSpan.3 {{ decl = _, name = _, span = _ }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "FlowDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.MemberDeclarationReference.3 {{ }}", MemberDeclarationReference
    return f"flow.MemberDeclarationReference.3 {{ memberDeclaration = _, loc = _ }}", MemberDeclarationReference

  @staticmethod
  def angle_query(*, memberDeclaration: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "FlowMemberDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.FlowTypeExportLocation.3 {{ }}", FlowTypeExportLocation
    return f"flow.FlowTypeExportLocation.3 {{ moduleTypeExport = _, entity = _, file = _, span = _ }}", FlowTypeExportLocation

  @staticmethod
  def angle_query(*, moduleTypeExport: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "FlowFlowTypeExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"flow.TypeDeclarationReference.3 {{ }}", TypeDeclarationReference
    return f"flow.TypeDeclarationReference.3 {{ typeDeclaration = _, loc = _ }}", TypeDeclarationReference

  @staticmethod
  def angle_query(*, typeDeclaration: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "FlowTypeDeclarationReference":
    raise Exception("this function can only be called from @angle_query")


