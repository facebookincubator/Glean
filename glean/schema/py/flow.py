# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class FlowFlowTypeEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowTypeEntityImportUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowTypeEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.TypeExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FlowTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.ImportDeclaration.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.DeclarationLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.DeclarationInfo.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FileDeclaration.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.ModuleTypeExport.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowModuleTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowLocalDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.LocalDeclarationReference.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowLocalDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowTypeImportXRef.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowTypeImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.Name.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FlowName":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.SearchByModule.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchByModule":
    raise Exception("this function can only be called from @angle_query")

class FlowRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.Range.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowRange":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.Declaration.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.DeclarationUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.SourceOfTypeExport.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSourceOfTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowCompatibleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowCompatibleExport.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowCompatibleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityUsesAll(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowEntityUsesAll.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowEntityUsesAll":
    raise Exception("this function can only be called from @angle_query")

class FlowNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.NameLowerCase.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.SearchByFileModule.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchByFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.Module.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FlowModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowSameModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowSameModule.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowSameModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowModuleNamespaceXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowModuleNamespaceXRef.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowModuleNamespaceXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.Documentation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FlowDocumentation":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.SearchByName.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class FlowStringToFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.StringToFileModule.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowStringToFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowExportLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowXRefDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowXRefDeclInfo.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowXRefDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchTypeByModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.SearchTypeByModuleExport.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSearchTypeByModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowEntityImportUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.Export.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FlowExport":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.TypeImportDeclaration.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowImportXRef.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.ModuleExport.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.MemberDeclarationInfo.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowMemberDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.TypeDeclarationInfo.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FileXRef.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFileXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.SourceOfExport.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowSourceOfExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.MemberDeclaration.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowMemberDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.Type.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "FlowType":
    raise Exception("this function can only be called from @angle_query")

class FlowFileOfStringModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FileOfStringModule.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFileOfStringModule":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.TypeDeclaration.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.DeclarationNameSpan.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.MemberDeclarationReference.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowMemberDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.FlowTypeExportLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowFlowTypeExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"flow.TypeDeclarationReference.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "FlowTypeDeclarationReference":
    raise Exception("this function can only be called from @angle_query")


