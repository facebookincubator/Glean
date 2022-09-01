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
    return f"python.NameLowerCase.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')])) or '_' } }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["PythonName"] = None) -> "PythonNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class PythonLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.LocalName.3 { angle_for(__env, arg, None) or '_' }", LocalName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonLocalName":
    raise Exception("this function can only be called from @angle_query")



class PythonIsTopLevelDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDefinition.3 { angle_for(__env, arg, None) or '_' }", IsTopLevelDefinition

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, bases: ast.Expr, keywords: ast.Expr, decorators: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ClassDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, bases, 'bases'), angle_for(__env, keywords, 'keywords'), angle_for(__env, decorators, 'decorators'), angle_for(__env, docstring, 'docstring')])) or '_' } }}", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonClassDeclaration"] = None, bases: Optional[Tuple[()]] = None, keywords: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonClassDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonNonImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.NonImportDeclaration.2 { angle_for(__env, arg, None) or '_' }", NonImportDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonNonImportDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationReference.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')])) or '_' } }}", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "PythonDeclarationReference":
    raise Exception("this function can only be called from @angle_query")



class PythonDefinitionsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DefinitionsByFile.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, definition, 'definition')])) or '_' } }}", DefinitionsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDefinitionsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.Module.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", Module

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonModule":
    raise Exception("this function can only be called from @angle_query")



class PythonDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DefinitionLocation.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")



class PythonClassBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ClassBySName.2 { angle_for(__env, arg, None) or '_' }", ClassBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonClassBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationUses.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationUses":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], from_name: ast.Expr, as_name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatement.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, from_name, 'from_name'), angle_for(__env, as_name, 'as_name')])) or '_' } }}", ImportStatement

  @staticmethod
  def angle_query(*, from_name: Optional["PythonName"] = None, as_name: Optional["PythonName"] = None) -> "PythonImportStatement":
    raise Exception("this function can only be called from @angle_query")



class PythonModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ModuleDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, docstring, 'docstring')])) or '_' } }}", ModuleDefinition

  @staticmethod
  def angle_query(*, module: Optional["PythonModule"] = None, docstring: Optional[Tuple[()]] = None) -> "PythonModuleDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationBySName.2 { angle_for(__env, arg, None) or '_' }", DeclarationBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonDeclarationBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationWithSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], sname: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationWithSName.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, sname, 'sname'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", DeclarationWithSName

  @staticmethod
  def angle_query(*, sname: Optional["PythonSName"] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithSName":
    raise Exception("this function can only be called from @angle_query")



class PythonModuleBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ModuleBySName.2 { angle_for(__env, arg, None) or '_' }", ModuleBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonModuleBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStarStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], from_name: ast.Expr, into_module: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStarStatement.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, from_name, 'from_name'), angle_for(__env, into_module, 'into_module')])) or '_' } }}", ImportStarStatement

  @staticmethod
  def angle_query(*, from_name: Optional["PythonName"] = None, into_module: Optional["PythonModule"] = None) -> "PythonImportStarStatement":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")



class PythonClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, bases: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ClassDeclaration.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, bases, 'bases')])) or '_' } }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, bases: Optional[Tuple[()]] = None) -> "PythonClassDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationToName.2 { angle_for(__env, arg, None) or '_' }", DeclarationToName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationToName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationDefinition.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, definition, 'definition')])) or '_' } }}", DeclarationDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDeclarationDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationByName.2 { angle_for(__env, arg, None) or '_' }", DeclarationByName

  @staticmethod
  def angle_query(*, arg: Optional["PythonName"] = None) -> "PythonDeclarationByName":
    raise Exception("this function can only be called from @angle_query")



class PythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.Name.1 { angle_for(__env, arg, None) or '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatementByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], as_name: ast.Expr, from_name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatementByName.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, as_name, 'as_name'), angle_for(__env, from_name, 'from_name')])) or '_' } }}", ImportStatementByName

  @staticmethod
  def angle_query(*, as_name: Optional["PythonName"] = None, from_name: Optional["PythonName"] = None) -> "PythonImportStatementByName":
    raise Exception("this function can only be called from @angle_query")



class PythonDocstringContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DocstringContent.2 { angle_for(__env, arg, None) or '_' }", DocstringContent

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonDocstringContent":
    raise Exception("this function can only be called from @angle_query")



class PythonVariableDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"python.VariableDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type')])) or '_' } }}", VariableDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonVariableDeclaration"] = None, type: Optional[Tuple[()]] = None) -> "PythonVariableDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.Type.1 { angle_for(__env, arg, None) or '_' }", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonType":
    raise Exception("this function can only be called from @angle_query")



class PythonDirectXRefsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DirectXRefsByFile.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref')])) or '_' } }}", DirectXRefsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None) -> "PythonDirectXRefsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonVariableBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.VariableBySName.2 { angle_for(__env, arg, None) or '_' }", VariableBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonVariableBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationWithLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local_name: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationWithLocalName.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, local_name, 'local_name'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", DeclarationWithLocalName

  @staticmethod
  def angle_query(*, local_name: Optional["PythonName"] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithLocalName":
    raise Exception("this function can only be called from @angle_query")



class PythonFileDirectXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FileDirectXRefs.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", FileDirectXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonFileDirectXRefs":
    raise Exception("this function can only be called from @angle_query")



class PythonFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FunctionDeclaration.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonSNameToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.SNameToName.2 { angle_for(__env, arg, None) or '_' }", SNameToName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonSNameToName":
    raise Exception("this function can only be called from @angle_query")



class PythonNameToSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.NameToSName.2 { angle_for(__env, arg, None) or '_' }", NameToSName

  @staticmethod
  def angle_query(*, arg: Optional["PythonName"] = None) -> "PythonNameToSName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatementByAsName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsName.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, import_, 'import_')])) or '_' } }}", ImportStatementByAsName

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, import_: Optional["PythonImportStatement"] = None) -> "PythonImportStatementByAsName":
    raise Exception("this function can only be called from @angle_query")



class PythonFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, is_async: ast.Expr, returns: ast.Expr, params: ast.Expr, posonly_params: ast.Expr, kwonly_params: ast.Expr, star_arg: ast.Expr, star_kwarg: ast.Expr, decorators: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FunctionDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, is_async, 'is_async'), angle_for(__env, returns, 'returns'), angle_for(__env, params, 'params'), angle_for(__env, posonly_params, 'posonly_params'), angle_for(__env, kwonly_params, 'kwonly_params'), angle_for(__env, star_arg, 'star_arg'), angle_for(__env, star_kwarg, 'star_kwarg'), angle_for(__env, decorators, 'decorators'), angle_for(__env, docstring, 'docstring')])) or '_' } }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonFunctionDeclaration"] = None, is_async: Optional[bool] = None, returns: Optional[Tuple[()]] = None, params: Optional[Tuple[()]] = None, posonly_params: Optional[Tuple[()]] = None, kwonly_params: Optional[Tuple[()]] = None, star_arg: Optional[Tuple[()]] = None, star_kwarg: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStarsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStarsByFile.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", ImportStarsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional["PythonImportStarStatement"] = None) -> "PythonImportStarsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonIsTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDeclaration.3 { angle_for(__env, arg, None) or '_' }", IsTopLevelDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonSearchByLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    return f"python.SearchByLocalName.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, decl, 'decl')])) or '_' } }}", SearchByLocalName

  @staticmethod
  def angle_query(*, name: Optional["PythonLocalName"] = None, decl: Optional[Tuple[()]] = None) -> "PythonSearchByLocalName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationsByFile.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonXRefsViaNameByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"python.XRefsViaNameByFile.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", XRefsViaNameByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonXRefsViaNameByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DeclarationWithName.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", DeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")



class PythonVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.VariableDeclaration.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')])) or '_' } }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonFunctionBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"python.FunctionBySName.2 { angle_for(__env, arg, None) or '_' }", FunctionBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonFunctionBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStarLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], import_star: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStarLocation.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, import_star, 'import_star'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", ImportStarLocation

  @staticmethod
  def angle_query(*, import_star: Optional["PythonImportStarStatement"] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "PythonImportStarLocation":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatementByAsSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], sname: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsSName.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, sname, 'sname'), angle_for(__env, import_, 'import_')])) or '_' } }}", ImportStatementByAsSName

  @staticmethod
  def angle_query(*, sname: Optional["PythonSName"] = None, import_: Optional["PythonImportStatement"] = None) -> "PythonImportStatementByAsSName":
    raise Exception("this function can only be called from @angle_query")



class PythonDefinitionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"python.DefinitionDeclaration.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", DefinitionDeclaration

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDefinitionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonContainingTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    return f"python.ContainingTopLevelDeclaration.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, container, 'container')])) or '_' } }}", ContainingTopLevelDeclaration

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "PythonContainingTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local_name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"python.SName.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, local_name, 'local_name'), angle_for(__env, parent, 'parent')])) or '_' } }}", SName

  @staticmethod
  def angle_query(*, local_name: Optional["PythonName"] = None, parent: Optional[Tuple[()]] = None) -> "PythonSName":
    raise Exception("this function can only be called from @angle_query")



class PythonLocalNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lowercase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"python.LocalNameLowerCase.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, lowercase, 'lowercase'), angle_for(__env, name, 'name')])) or '_' } }}", LocalNameLowerCase

  @staticmethod
  def angle_query(*, lowercase: Optional[str] = None, name: Optional["PythonLocalName"] = None) -> "PythonLocalNameLowerCase":
    raise Exception("this function can only be called from @angle_query")




