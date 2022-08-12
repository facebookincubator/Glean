# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSFlowFlowTypeEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowTypeEntityImportUses.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowTypeEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class GSFlowTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.TypeExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowTypeExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.ImportDeclaration.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSFlowDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.DeclarationLocation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GSFlowDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.DeclarationInfo.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FileDeclaration.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSFlowModuleTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.ModuleTypeExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowModuleTypeExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowLocalDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.LocalDeclarationReference.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowLocalDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowTypeImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowTypeImportXRef.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowTypeImportXRef":
    raise Exception("this function can only be called from @angle_query")

class GSFlowName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.Name.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowName":
    raise Exception("this function can only be called from @angle_query")

class GSFlowSearchByModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.SearchByModule.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowSearchByModule":
    raise Exception("this function can only be called from @angle_query")

class GSFlowRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.Range.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowRange":
    raise Exception("this function can only be called from @angle_query")

class GSFlowDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.Declaration.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSFlowDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.DeclarationUses.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class GSFlowSourceOfTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.SourceOfTypeExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowSourceOfTypeExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowCompatibleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowCompatibleExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowCompatibleExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowEntityUsesAll(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowEntityUsesAll.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowEntityUsesAll":
    raise Exception("this function can only be called from @angle_query")

class GSFlowNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.NameLowerCase.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSFlowSearchByFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.SearchByFileModule.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowSearchByFileModule":
    raise Exception("this function can only be called from @angle_query")

class GSFlowModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.Module.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowModule":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowSameModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowSameModule.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowSameModule":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowModuleNamespaceXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowModuleNamespaceXRef.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowModuleNamespaceXRef":
    raise Exception("this function can only be called from @angle_query")

class GSFlowDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.Documentation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowDocumentation":
    raise Exception("this function can only be called from @angle_query")

class GSFlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.SearchByName.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSFlowStringToFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.StringToFileModule.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowStringToFileModule":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowExportLocation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowExportLocation":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowXRefDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowXRefDeclInfo.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowXRefDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class GSFlowSearchTypeByModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.SearchTypeByModuleExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowSearchTypeByModuleExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowEntityImportUses.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class GSFlowExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.Export.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowTypeImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.TypeImportDeclaration.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowTypeImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowImportXRef.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowImportXRef":
    raise Exception("this function can only be called from @angle_query")

class GSFlowModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.ModuleExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowModuleExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowMemberDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.MemberDeclarationInfo.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowMemberDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class GSFlowTypeDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.TypeDeclarationInfo.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowTypeDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFileXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FileXRef.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFileXRef":
    raise Exception("this function can only be called from @angle_query")

class GSFlowSourceOfExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.SourceOfExport.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowSourceOfExport":
    raise Exception("this function can only be called from @angle_query")

class GSFlowMemberDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.MemberDeclaration.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowMemberDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSFlowType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.Type.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowType":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFileOfStringModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FileOfStringModule.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFileOfStringModule":
    raise Exception("this function can only be called from @angle_query")

class GSFlowTypeDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.TypeDeclaration.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowTypeDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSFlowDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.DeclarationNameSpan.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class GSFlowMemberDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.MemberDeclarationReference.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowMemberDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class GSFlowFlowTypeExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.FlowTypeExportLocation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowFlowTypeExportLocation":
    raise Exception("this function can only be called from @angle_query")

class GSFlowTypeDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"flow.TypeDeclarationReference.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSFlowTypeDeclarationReference":
    raise Exception("this function can only be called from @angle_query")


