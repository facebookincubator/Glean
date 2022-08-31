# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.python.types import (
    NameLowerCase,
    LocalName,
    IsTopLevelDefinition,
    ClassDefinition,
    NonImportDeclaration,
    DeclarationReference,
    DefinitionsByFile,
    Module,
    DefinitionLocation,
    ClassBySName,
    DeclarationUses,
    ImportStatement,
    ModuleDefinition,
    DeclarationBySName,
    DeclarationWithSName,
    ModuleBySName,
    ImportStarStatement,
    DeclarationLocation,
    ClassDeclaration,
    DeclarationToName,
    DeclarationDefinition,
    DeclarationByName,
    Name,
    ImportStatementByName,
    DocstringContent,
    VariableDefinition,
    Type,
    DirectXRefsByFile,
    VariableBySName,
    DeclarationWithLocalName,
    FileDirectXRefs,
    FunctionDeclaration,
    SNameToName,
    NameToSName,
    ImportStatementByAsName,
    FunctionDefinition,
    ImportStarsByFile,
    IsTopLevelDeclaration,
    SearchByLocalName,
    DeclarationsByFile,
    XRefsViaNameByFile,
    DeclarationWithName,
    VariableDeclaration,
    FunctionBySName,
    ImportStarLocation,
    ImportStatementByAsSName,
    DefinitionDeclaration,
    ContainingTopLevelDeclaration,
    SName,
    LocalNameLowerCase,
)


class PythonNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.NameLowerCase.1 {{ nameLowerCase = {angle_for(__env, nameLowerCase)}, name = {angle_for(__env, name)} }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["PythonName"] = None) -> "PythonNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.LocalName.3 {angle_for(__env, arg)}", LocalName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDefinition.3 {angle_for(__env, arg)}", IsTopLevelDefinition

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, bases: ast.Expr, keywords: ast.Expr, decorators: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ClassDefinition.2 {{ declaration = {angle_for(__env, declaration)}, bases = {angle_for(__env, bases)}, keywords = {angle_for(__env, keywords)}, decorators = {angle_for(__env, decorators)}, docstring = {angle_for(__env, docstring)} }}", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonClassDeclaration"] = None, bases: Optional[Tuple[()]] = None, keywords: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonNonImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.NonImportDeclaration.2 {angle_for(__env, arg)}", NonImportDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonNonImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationReference.3 {{ target = {angle_for(__env, target)}, source = {angle_for(__env, source)} }}", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "PythonDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DefinitionsByFile.3 {{ file = {angle_for(__env, file)}, span = {angle_for(__env, span)}, definition = {angle_for(__env, definition)} }}", DefinitionsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDefinitionsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.Module.1 {{ name = {angle_for(__env, name)} }}", Module

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonModule":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DefinitionLocation.3 {{ definition = {angle_for(__env, definition)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ClassBySName.2 {angle_for(__env, arg)}", ClassBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonClassBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationUses.2 {{ declaration = {angle_for(__env, declaration)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], from_name: ast.Expr, as_name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatement.2 {{ from_name = {angle_for(__env, from_name)}, as_name = {angle_for(__env, as_name)} }}", ImportStatement

  @staticmethod
  def angle_query(*, from_name: Optional["PythonName"] = None, as_name: Optional["PythonName"] = None) -> "PythonImportStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ModuleDefinition.2 {{ module = {angle_for(__env, module)}, docstring = {angle_for(__env, docstring)} }}", ModuleDefinition

  @staticmethod
  def angle_query(*, module: Optional["PythonModule"] = None, docstring: Optional[Tuple[()]] = None) -> "PythonModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationBySName.2 {angle_for(__env, arg)}", DeclarationBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonDeclarationBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], sname: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationWithSName.3 {{ sname = {angle_for(__env, sname)}, declaration = {angle_for(__env, declaration)} }}", DeclarationWithSName

  @staticmethod
  def angle_query(*, sname: Optional["PythonSName"] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithSName":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ModuleBySName.2 {angle_for(__env, arg)}", ModuleBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonModuleBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], from_name: ast.Expr, into_module: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStarStatement.3 {{ from_name = {angle_for(__env, from_name)}, into_module = {angle_for(__env, into_module)} }}", ImportStarStatement

  @staticmethod
  def angle_query(*, from_name: Optional["PythonName"] = None, into_module: Optional["PythonModule"] = None) -> "PythonImportStarStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationLocation.2 {{ declaration = {angle_for(__env, declaration)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, bases: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ClassDeclaration.1 {{ name = {angle_for(__env, name)}, bases = {angle_for(__env, bases)} }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, bases: Optional[Tuple[()]] = None) -> "PythonClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationToName.2 {angle_for(__env, arg)}", DeclarationToName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationToName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationDefinition.3 {{ declaration = {angle_for(__env, declaration)}, definition = {angle_for(__env, definition)} }}", DeclarationDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDeclarationDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationByName.2 {angle_for(__env, arg)}", DeclarationByName

  @staticmethod
  def angle_query(*, arg: Optional["PythonName"] = None) -> "PythonDeclarationByName":
    raise Exception("this function can only be called from @angle_query")

class PythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.Name.1 {angle_for(__env, arg)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], as_name: ast.Expr, from_name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatementByName.2 {{ as_name = {angle_for(__env, as_name)}, from_name = {angle_for(__env, from_name)} }}", ImportStatementByName

  @staticmethod
  def angle_query(*, as_name: Optional["PythonName"] = None, from_name: Optional["PythonName"] = None) -> "PythonImportStatementByName":
    raise Exception("this function can only be called from @angle_query")

class PythonDocstringContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DocstringContent.2 {angle_for(__env, arg)}", DocstringContent

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonDocstringContent":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"python.VariableDefinition.2 {{ declaration = {angle_for(__env, declaration)}, type = {angle_for(__env, type)} }}", VariableDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonVariableDeclaration"] = None, type: Optional[Tuple[()]] = None) -> "PythonVariableDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.Type.1 {angle_for(__env, arg)}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonType":
    raise Exception("this function can only be called from @angle_query")

class PythonDirectXRefsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DirectXRefsByFile.2 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)} }}", DirectXRefsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None) -> "PythonDirectXRefsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.VariableBySName.2 {angle_for(__env, arg)}", VariableBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonVariableBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local_name: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationWithLocalName.3 {{ local_name = {angle_for(__env, local_name)}, declaration = {angle_for(__env, declaration)} }}", DeclarationWithLocalName

  @staticmethod
  def angle_query(*, local_name: Optional["PythonName"] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonFileDirectXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FileDirectXRefs.2 {{ file = {angle_for(__env, file)}, xrefs = {angle_for(__env, xrefs)} }}", FileDirectXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonFileDirectXRefs":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FunctionDeclaration.1 {{ name = {angle_for(__env, name)} }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSNameToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.SNameToName.2 {angle_for(__env, arg)}", SNameToName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonSNameToName":
    raise Exception("this function can only be called from @angle_query")

class PythonNameToSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.NameToSName.2 {angle_for(__env, arg)}", NameToSName

  @staticmethod
  def angle_query(*, arg: Optional["PythonName"] = None) -> "PythonNameToSName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsName.3 {{ name = {angle_for(__env, name)}, import_ = {angle_for(__env, import_)} }}", ImportStatementByAsName

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, import_: Optional["PythonImportStatement"] = None) -> "PythonImportStatementByAsName":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, is_async: ast.Expr, returns: ast.Expr, params: ast.Expr, posonly_params: ast.Expr, kwonly_params: ast.Expr, star_arg: ast.Expr, star_kwarg: ast.Expr, decorators: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FunctionDefinition.2 {{ declaration = {angle_for(__env, declaration)}, is_async = {angle_for(__env, is_async)}, returns = {angle_for(__env, returns)}, params = {angle_for(__env, params)}, posonly_params = {angle_for(__env, posonly_params)}, kwonly_params = {angle_for(__env, kwonly_params)}, star_arg = {angle_for(__env, star_arg)}, star_kwarg = {angle_for(__env, star_kwarg)}, decorators = {angle_for(__env, decorators)}, docstring = {angle_for(__env, docstring)} }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonFunctionDeclaration"] = None, is_async: Optional[bool] = None, returns: Optional[Tuple[()]] = None, params: Optional[Tuple[()]] = None, posonly_params: Optional[Tuple[()]] = None, kwonly_params: Optional[Tuple[()]] = None, star_arg: Optional[Tuple[()]] = None, star_kwarg: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStarsByFile.3 {{ file = {angle_for(__env, file)}, span = {angle_for(__env, span)}, declaration = {angle_for(__env, declaration)} }}", ImportStarsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional["PythonImportStarStatement"] = None) -> "PythonImportStarsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDeclaration.3 {angle_for(__env, arg)}", IsTopLevelDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSearchByLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"python.SearchByLocalName.3 {{ name = {angle_for(__env, name)}, decl = {angle_for(__env, decl)} }}", SearchByLocalName

  @staticmethod
  def angle_query(*, name: Optional["PythonLocalName"] = None, decl: Optional[Tuple[()]] = None) -> "PythonSearchByLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationsByFile.2 {{ file = {angle_for(__env, file)}, span = {angle_for(__env, span)}, declaration = {angle_for(__env, declaration)} }}", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonXRefsViaNameByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"python.XRefsViaNameByFile.2 {{ file = {angle_for(__env, file)}, xrefs = {angle_for(__env, xrefs)} }}", XRefsViaNameByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonXRefsViaNameByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationWithName.2 {{ name = {angle_for(__env, name)}, declaration = {angle_for(__env, declaration)} }}", DeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.VariableDeclaration.1 {{ name = {angle_for(__env, name)} }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FunctionBySName.2 {angle_for(__env, arg)}", FunctionBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonFunctionBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], import_star: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStarLocation.3 {{ import_star = {angle_for(__env, import_star)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", ImportStarLocation

  @staticmethod
  def angle_query(*, import_star: Optional["PythonImportStarStatement"] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonImportStarLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], sname: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsSName.3 {{ sname = {angle_for(__env, sname)}, import_ = {angle_for(__env, import_)} }}", ImportStatementByAsSName

  @staticmethod
  def angle_query(*, sname: Optional["PythonSName"] = None, import_: Optional["PythonImportStatement"] = None) -> "PythonImportStatementByAsSName":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DefinitionDeclaration.3 {{ definition = {angle_for(__env, definition)}, declaration = {angle_for(__env, declaration)} }}", DefinitionDeclaration

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDefinitionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonContainingTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ContainingTopLevelDeclaration.3 {{ declaration = {angle_for(__env, declaration)}, container = {angle_for(__env, container)} }}", ContainingTopLevelDeclaration

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "PythonContainingTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local_name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"python.SName.2 {{ local_name = {angle_for(__env, local_name)}, parent = {angle_for(__env, parent)} }}", SName

  @staticmethod
  def angle_query(*, local_name: Optional["PythonName"] = None, parent: Optional[Tuple[()]] = None) -> "PythonSName":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lowercase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.LocalNameLowerCase.3 {{ lowercase = {angle_for(__env, lowercase)}, name = {angle_for(__env, name)} }}", LocalNameLowerCase

  @staticmethod
  def angle_query(*, lowercase: Optional[str] = None, name: Optional["PythonLocalName"] = None) -> "PythonLocalNameLowerCase":
    raise Exception("this function can only be called from @angle_query")


