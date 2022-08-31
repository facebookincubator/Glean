# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], target: ast.Expr, local: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowTypeEntityImportUses.3 {{ target = {angle_for(__env, target)}, local = {angle_for(__env, local)} }}", FlowTypeEntityImportUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, local: Optional["FlowTypeDeclaration"] = None) -> "FlowFlowTypeEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.TypeExport.3 {angle_for(__env, arg)}", TypeExport

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FlowTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.ImportDeclaration.3 {{ declaration = {angle_for(__env, declaration)}, import_ = {angle_for(__env, import_)} }}", ImportDeclaration

  @staticmethod
  def angle_query(*, declaration: Optional["FlowDeclaration"] = None, import_: Optional[Tuple[()]] = None) -> "FlowImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.DeclarationLocation.3 {{ decl = {angle_for(__env, decl)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "FlowDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr, documentation: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.DeclarationInfo.3 {{ declaration = {angle_for(__env, declaration)}, type = {angle_for(__env, type)}, documentation = {angle_for(__env, documentation)} }}", DeclarationInfo

  @staticmethod
  def angle_query(*, declaration: Optional["FlowDeclaration"] = None, type: Optional["FlowType"] = None, documentation: Optional[Tuple[()]] = None) -> "FlowDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FileDeclaration.3 {{ file = {angle_for(__env, file)}, declaration = {angle_for(__env, declaration)} }}", FileDeclaration

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, declaration: Optional[Tuple[()]] = None) -> "FlowFileDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, typeExport: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.ModuleTypeExport.3 {{ module = {angle_for(__env, module)}, typeExport = {angle_for(__env, typeExport)} }}", ModuleTypeExport

  @staticmethod
  def angle_query(*, module: Optional["FlowModule"] = None, typeExport: Optional["FlowTypeExport"] = None) -> "FlowModuleTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowLocalDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.LocalDeclarationReference.3 {{ declaration = {angle_for(__env, declaration)}, loc = {angle_for(__env, loc)} }}", LocalDeclarationReference

  @staticmethod
  def angle_query(*, declaration: Optional["FlowDeclaration"] = None, loc: Optional["FlowRange"] = None) -> "FlowLocalDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local: ast.Expr, entity: ast.Expr, targetFile: ast.Expr, targetSpan: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowTypeImportXRef.3 {{ local = {angle_for(__env, local)}, entity = {angle_for(__env, entity)}, targetFile = {angle_for(__env, targetFile)}, targetSpan = {angle_for(__env, targetSpan)} }}", FlowTypeImportXRef

  @staticmethod
  def angle_query(*, local: Optional["FlowTypeDeclaration"] = None, entity: Optional[Tuple[()]] = None, targetFile: Optional["SrcFile"] = None, targetSpan: Optional[Tuple[()]] = None) -> "FlowFlowTypeImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.Name.3 {angle_for(__env, arg)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "FlowName":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], string_: ast.Expr, name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.SearchByModule.3 {{ string_ = {angle_for(__env, string_)}, name = {angle_for(__env, name)}, decl = {angle_for(__env, decl)} }}", SearchByModule

  @staticmethod
  def angle_query(*, string_: Optional[str] = None, name: Optional["FlowName"] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchByModule":
    raise Exception("this function can only be called from @angle_query")

class FlowRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.Range.3 {{ module = {angle_for(__env, module)}, span = {angle_for(__env, span)} }}", Range

  @staticmethod
  def angle_query(*, module: Optional["FlowModule"] = None, span: Optional[Tuple[()]] = None) -> "FlowRange":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.Declaration.3 {{ name = {angle_for(__env, name)}, loc = {angle_for(__env, loc)} }}", Declaration

  @staticmethod
  def angle_query(*, name: Optional["FlowName"] = None, loc: Optional["FlowRange"] = None) -> "FlowDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, use: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.DeclarationUses.3 {{ target = {angle_for(__env, target)}, file = {angle_for(__env, file)}, use = {angle_for(__env, use)} }}", DeclarationUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, use: Optional[Tuple[()]] = None) -> "FlowDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfTypeExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], moduleTypeExport: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.SourceOfTypeExport.3 {{ moduleTypeExport = {angle_for(__env, moduleTypeExport)}, source = {angle_for(__env, source)} }}", SourceOfTypeExport

  @staticmethod
  def angle_query(*, moduleTypeExport: Optional["FlowModuleTypeExport"] = None, source: Optional[Tuple[()]] = None) -> "FlowSourceOfTypeExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowCompatibleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], left: ast.Expr, right: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowCompatibleExport.3 {{ left = {angle_for(__env, left)}, right = {angle_for(__env, right)} }}", FlowCompatibleExport

  @staticmethod
  def angle_query(*, left: Optional["FlowExport"] = None, right: Optional["FlowExport"] = None) -> "FlowFlowCompatibleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityUsesAll(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowEntityUsesAll.3 {{ target = {angle_for(__env, target)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", FlowEntityUsesAll

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "FlowFlowEntityUsesAll":
    raise Exception("this function can only be called from @angle_query")

class FlowNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.NameLowerCase.3 {{ nameLowerCase = {angle_for(__env, nameLowerCase)}, name = {angle_for(__env, name)} }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["FlowName"] = None) -> "FlowNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.SearchByFileModule.3 {{ file = {angle_for(__env, file)}, name = {angle_for(__env, name)}, decl = {angle_for(__env, decl)} }}", SearchByFileModule

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, name: Optional["FlowName"] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchByFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.Module.3 {angle_for(__env, arg)}", Module

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FlowModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowSameModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], left: ast.Expr, right: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowSameModule.3 {{ left = {angle_for(__env, left)}, right = {angle_for(__env, right)} }}", FlowSameModule

  @staticmethod
  def angle_query(*, left: Optional["FlowModule"] = None, right: Optional["FlowModule"] = None) -> "FlowFlowSameModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowModuleNamespaceXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local: ast.Expr, entity: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowModuleNamespaceXRef.3 {{ local = {angle_for(__env, local)}, entity = {angle_for(__env, entity)}, file = {angle_for(__env, file)} }}", FlowModuleNamespaceXRef

  @staticmethod
  def angle_query(*, local: Optional["FlowDeclaration"] = None, entity: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None) -> "FlowFlowModuleNamespaceXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.Documentation.3 {angle_for(__env, arg)}", Documentation

  @staticmethod
  def angle_query(*, arg: Optional["FlowRange"] = None) -> "FlowDocumentation":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.SearchByName.3 {{ name = {angle_for(__env, name)}, decl = {angle_for(__env, decl)} }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional["FlowName"] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchByName":
    raise Exception("this function can only be called from @angle_query")

class FlowStringToFileModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], string_: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.StringToFileModule.3 {{ string_ = {angle_for(__env, string_)}, file = {angle_for(__env, file)} }}", StringToFileModule

  @staticmethod
  def angle_query(*, string_: Optional[str] = None, file: Optional["SrcFile"] = None) -> "FlowStringToFileModule":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, export_: ast.Expr, entity: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowExportLocation.3 {{ module = {angle_for(__env, module)}, export_ = {angle_for(__env, export_)}, entity = {angle_for(__env, entity)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", FlowExportLocation

  @staticmethod
  def angle_query(*, module: Optional["FlowModule"] = None, export_: Optional["FlowExport"] = None, entity: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "FlowFlowExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowXRefDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], ref: ast.Expr, srcLoc: ast.Expr, name: ast.Expr, targetLoc: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowXRefDeclInfo.3 {{ ref = {angle_for(__env, ref)}, srcLoc = {angle_for(__env, srcLoc)}, name = {angle_for(__env, name)}, targetLoc = {angle_for(__env, targetLoc)}, entity = {angle_for(__env, entity)} }}", FlowXRefDeclInfo

  @staticmethod
  def angle_query(*, ref: Optional[Tuple[()]] = None, srcLoc: Optional["FlowRange"] = None, name: Optional["FlowName"] = None, targetLoc: Optional["FlowRange"] = None, entity: Optional[Tuple[()]] = None) -> "FlowFlowXRefDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowSearchTypeByModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], string_: ast.Expr, name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.SearchTypeByModuleExport.3 {{ string_ = {angle_for(__env, string_)}, name = {angle_for(__env, name)}, decl = {angle_for(__env, decl)} }}", SearchTypeByModuleExport

  @staticmethod
  def angle_query(*, string_: Optional[str] = None, name: Optional["FlowName"] = None, decl: Optional[Tuple[()]] = None) -> "FlowSearchTypeByModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowEntityImportUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, local: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowEntityImportUses.3 {{ target = {angle_for(__env, target)}, local = {angle_for(__env, local)} }}", FlowEntityImportUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, local: Optional["FlowDeclaration"] = None) -> "FlowFlowEntityImportUses":
    raise Exception("this function can only be called from @angle_query")

class FlowExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.Export.3 {angle_for(__env, arg)}", Export

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "FlowExport":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], typeDeclaration: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.TypeImportDeclaration.3 {{ typeDeclaration = {angle_for(__env, typeDeclaration)}, import_ = {angle_for(__env, import_)} }}", TypeImportDeclaration

  @staticmethod
  def angle_query(*, typeDeclaration: Optional["FlowTypeDeclaration"] = None, import_: Optional[Tuple[()]] = None) -> "FlowTypeImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowImportXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local: ast.Expr, entity: ast.Expr, targetFile: ast.Expr, targetSpan: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowImportXRef.3 {{ local = {angle_for(__env, local)}, entity = {angle_for(__env, entity)}, targetFile = {angle_for(__env, targetFile)}, targetSpan = {angle_for(__env, targetSpan)} }}", FlowImportXRef

  @staticmethod
  def angle_query(*, local: Optional["FlowDeclaration"] = None, entity: Optional[Tuple[()]] = None, targetFile: Optional["SrcFile"] = None, targetSpan: Optional[Tuple[()]] = None) -> "FlowFlowImportXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowModuleExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, export_: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.ModuleExport.3 {{ module = {angle_for(__env, module)}, export_ = {angle_for(__env, export_)} }}", ModuleExport

  @staticmethod
  def angle_query(*, module: Optional["FlowModule"] = None, export_: Optional["FlowExport"] = None) -> "FlowModuleExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], memberDeclaration: ast.Expr, type: ast.Expr, documentation: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.MemberDeclarationInfo.3 {{ memberDeclaration = {angle_for(__env, memberDeclaration)}, type = {angle_for(__env, type)}, documentation = {angle_for(__env, documentation)} }}", MemberDeclarationInfo

  @staticmethod
  def angle_query(*, memberDeclaration: Optional["FlowMemberDeclaration"] = None, type: Optional["FlowType"] = None, documentation: Optional[Tuple[()]] = None) -> "FlowMemberDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], typeDeclaration: ast.Expr, type: ast.Expr, documentation: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.TypeDeclarationInfo.3 {{ typeDeclaration = {angle_for(__env, typeDeclaration)}, type = {angle_for(__env, type)}, documentation = {angle_for(__env, documentation)} }}", TypeDeclarationInfo

  @staticmethod
  def angle_query(*, typeDeclaration: Optional["FlowTypeDeclaration"] = None, type: Optional["FlowType"] = None, documentation: Optional[Tuple[()]] = None) -> "FlowTypeDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")

class FlowFileXRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, ref: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FileXRef.3 {{ file = {angle_for(__env, file)}, ref = {angle_for(__env, ref)} }}", FileXRef

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, ref: Optional[Tuple[()]] = None) -> "FlowFileXRef":
    raise Exception("this function can only be called from @angle_query")

class FlowSourceOfExport(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], moduleExport: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.SourceOfExport.3 {{ moduleExport = {angle_for(__env, moduleExport)}, source = {angle_for(__env, source)} }}", SourceOfExport

  @staticmethod
  def angle_query(*, moduleExport: Optional["FlowModuleExport"] = None, source: Optional[Tuple[()]] = None) -> "FlowSourceOfExport":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.MemberDeclaration.3 {{ name = {angle_for(__env, name)}, loc = {angle_for(__env, loc)} }}", MemberDeclaration

  @staticmethod
  def angle_query(*, name: Optional["FlowName"] = None, loc: Optional["FlowRange"] = None) -> "FlowMemberDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.Type.3 {angle_for(__env, arg)}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "FlowType":
    raise Exception("this function can only be called from @angle_query")

class FlowFileOfStringModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, string_: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FileOfStringModule.3 {{ file = {angle_for(__env, file)}, string_ = {angle_for(__env, string_)} }}", FileOfStringModule

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, string_: Optional[str] = None) -> "FlowFileOfStringModule":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.TypeDeclaration.3 {{ name = {angle_for(__env, name)}, loc = {angle_for(__env, loc)} }}", TypeDeclaration

  @staticmethod
  def angle_query(*, name: Optional["FlowName"] = None, loc: Optional["FlowRange"] = None) -> "FlowTypeDeclaration":
    raise Exception("this function can only be called from @angle_query")

class FlowDeclarationNameSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, name: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.DeclarationNameSpan.3 {{ decl = {angle_for(__env, decl)}, name = {angle_for(__env, name)}, span = {angle_for(__env, span)} }}", DeclarationNameSpan

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, name: Optional["FlowName"] = None, span: Optional[Tuple[()]] = None) -> "FlowDeclarationNameSpan":
    raise Exception("this function can only be called from @angle_query")

class FlowMemberDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], memberDeclaration: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.MemberDeclarationReference.3 {{ memberDeclaration = {angle_for(__env, memberDeclaration)}, loc = {angle_for(__env, loc)} }}", MemberDeclarationReference

  @staticmethod
  def angle_query(*, memberDeclaration: Optional["FlowMemberDeclaration"] = None, loc: Optional["FlowRange"] = None) -> "FlowMemberDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class FlowFlowTypeExportLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], moduleTypeExport: ast.Expr, entity: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.FlowTypeExportLocation.3 {{ moduleTypeExport = {angle_for(__env, moduleTypeExport)}, entity = {angle_for(__env, entity)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", FlowTypeExportLocation

  @staticmethod
  def angle_query(*, moduleTypeExport: Optional["FlowModuleTypeExport"] = None, entity: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "FlowFlowTypeExportLocation":
    raise Exception("this function can only be called from @angle_query")

class FlowTypeDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], typeDeclaration: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"flow.TypeDeclarationReference.3 {{ typeDeclaration = {angle_for(__env, typeDeclaration)}, loc = {angle_for(__env, loc)} }}", TypeDeclarationReference

  @staticmethod
  def angle_query(*, typeDeclaration: Optional["FlowTypeDeclaration"] = None, loc: Optional["FlowRange"] = None) -> "FlowTypeDeclarationReference":
    raise Exception("this function can only be called from @angle_query")


