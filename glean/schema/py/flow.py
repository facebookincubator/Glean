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
    return f"flow.FlowTypeEntityImportUses.3 { { } }", FlowTypeEntityImportUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowTypeEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeExport.3 { json.dumps(key) }", TypeExport

  @staticmethod
  def angle_query(*, name: str) -> "FlowTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.ImportDeclaration.3 { { } }", ImportDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationLocation.3 { { } }", DeclarationLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationInfo.3 { { } }", DeclarationInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FileDeclaration.3 { { } }", FileDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.ModuleTypeExport.3 { { } }", ModuleTypeExport

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowModuleTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowLocalDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.LocalDeclarationReference.3 { { } }", LocalDeclarationReference

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowLocalDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowTypeImportXRef.3 { { } }", FlowTypeImportXRef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowTypeImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Name.3 { json.dumps(key) }", Name

  @staticmethod
  def angle_query(*, name: str) -> "FlowName":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchByModule.3 { { } }", SearchByModule

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchByModule":
    raise Exception("this function can only be called from @angle_query")

class FlowRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Range.3 { { } }", Range

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowRange":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Declaration.3 { { } }", Declaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationUses.3 { { } }", DeclarationUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SourceOfTypeExport.3 { { } }", SourceOfTypeExport

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSourceOfTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowCompatibleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowCompatibleExport.3 { { } }", FlowCompatibleExport

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowCompatibleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityUsesAll(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowEntityUsesAll.3 { { } }", FlowEntityUsesAll

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowEntityUsesAll":
    raise Exception("this function can only be called from @angle_query")

class FlowNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.NameLowerCase.3 { { } }", NameLowerCase

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchByFileModule.3 { { } }", SearchByFileModule

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchByFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Module.3 { json.dumps(key) }", Module

  @staticmethod
  def angle_query(*, name: str) -> "FlowModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowSameModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowSameModule.3 { { } }", FlowSameModule

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowSameModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowModuleNamespaceXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowModuleNamespaceXRef.3 { { } }", FlowModuleNamespaceXRef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowModuleNamespaceXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Documentation.3 { json.dumps(key) }", Documentation

  @staticmethod
  def angle_query(*, name: str) -> "FlowDocumentation":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchByName.3 { { } }", SearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class FlowStringToFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.StringToFileModule.3 { { } }", StringToFileModule

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowStringToFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowExportLocation.3 { { } }", FlowExportLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowXRefDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowXRefDeclInfo.3 { { } }", FlowXRefDeclInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowXRefDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchTypeByModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SearchTypeByModuleExport.3 { { } }", SearchTypeByModuleExport

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchTypeByModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowEntityImportUses.3 { { } }", FlowEntityImportUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Export.3 { json.dumps(key) }", Export

  @staticmethod
  def angle_query(*, name: str) -> "FlowExport":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeImportDeclaration.3 { { } }", TypeImportDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowImportXRef.3 { { } }", FlowImportXRef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.ModuleExport.3 { { } }", ModuleExport

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.MemberDeclarationInfo.3 { { } }", MemberDeclarationInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowMemberDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeDeclarationInfo.3 { { } }", TypeDeclarationInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FileXRef.3 { { } }", FileXRef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFileXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.SourceOfExport.3 { { } }", SourceOfExport

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSourceOfExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.MemberDeclaration.3 { { } }", MemberDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowMemberDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.Type.3 { json.dumps(key) }", Type

  @staticmethod
  def angle_query(*, name: str) -> "FlowType":
    raise Exception("this function can only be called from @angle_query")

class FlowFileOfStringModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FileOfStringModule.3 { { } }", FileOfStringModule

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFileOfStringModule":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeDeclaration.3 { { } }", TypeDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.DeclarationNameSpan.3 { { } }", DeclarationNameSpan

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.MemberDeclarationReference.3 { { } }", MemberDeclarationReference

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowMemberDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.FlowTypeExportLocation.3 { { } }", FlowTypeExportLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowTypeExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"flow.TypeDeclarationReference.3 { { } }", TypeDeclarationReference

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeDeclarationReference":
    raise Exception("this function can only be called from @angle_query")


