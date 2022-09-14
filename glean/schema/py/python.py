# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
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
    Parameter,
    XRefViaName,
    Declaration,
    Docstring,
    Definition,
    DirectXRef,
)


class PythonNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')]))
    return f"python.NameLowerCase.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["PythonName"] = None) -> "PythonNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class PythonLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.LocalName.3 { query_fields if query_fields else '_' }", LocalName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonLocalName":
    raise Exception("this function can only be called from @angle_query")



class PythonIsTopLevelDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.IsTopLevelDefinition.3 { query_fields if query_fields else '_' }", IsTopLevelDefinition

  @staticmethod
  def angle_query(*, arg: Optional["PythonDefinition"] = None) -> "PythonIsTopLevelDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, bases: ast.Expr, keywords: ast.Expr, decorators: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, bases, 'bases'), angle_for(__env, keywords, 'keywords'), angle_for(__env, decorators, 'decorators'), angle_for(__env, docstring, 'docstring')]))
    return f"python.ClassDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonClassDeclaration"] = None, bases: Optional[Union[Just[List["PythonClassDeclaration"]], Just[None]]] = None, keywords: Optional[Union[Just[List["PythonParameter"]], Just[None]]] = None, decorators: Optional[Union[Just[List["PythonDecorator"]], Just[None]]] = None, docstring: Optional[Union[Just["PythonDocstring"], Just[None]]] = None) -> "PythonClassDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonNonImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.NonImportDeclaration.2 { query_fields if query_fields else '_' }", NonImportDeclaration

  @staticmethod
  def angle_query(*, arg: Optional["PythonDeclaration"] = None) -> "PythonNonImportDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')]))
    return f"python.DeclarationReference.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional["PythonDeclaration"] = None, source: Optional["PythonDeclaration"] = None) -> "PythonDeclarationReference":
    raise Exception("this function can only be called from @angle_query")



class PythonDefinitionsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, definition, 'definition')]))
    return f"python.DefinitionsByFile.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None, definition: Optional["PythonDefinition"] = None) -> "PythonDefinitionsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"python.Module.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Module

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonModule":
    raise Exception("this function can only be called from @angle_query")



class PythonDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"python.DefinitionLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Optional["PythonDefinition"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "PythonDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")



class PythonClassBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.ClassBySName.2 { query_fields if query_fields else '_' }", ClassBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonClassBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"python.DeclarationUses.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional["PythonDeclaration"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "PythonDeclarationUses":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], from_name: ast.Expr, as_name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, from_name, 'from_name'), angle_for(__env, as_name, 'as_name')]))
    return f"python.ImportStatement.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportStatement

  @staticmethod
  def angle_query(*, from_name: Optional["PythonName"] = None, as_name: Optional["PythonName"] = None) -> "PythonImportStatement":
    raise Exception("this function can only be called from @angle_query")



class PythonModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, docstring, 'docstring')]))
    return f"python.ModuleDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ModuleDefinition

  @staticmethod
  def angle_query(*, module: Optional["PythonModule"] = None, docstring: Optional[Union[Just["PythonDocstring"], Just[None]]] = None) -> "PythonModuleDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.DeclarationBySName.2 { query_fields if query_fields else '_' }", DeclarationBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonDeclarationBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationWithSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], sname: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, sname, 'sname'), angle_for(__env, declaration, 'declaration')]))
    return f"python.DeclarationWithSName.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationWithSName

  @staticmethod
  def angle_query(*, sname: Optional["PythonSName"] = None, declaration: Optional["PythonDeclaration"] = None) -> "PythonDeclarationWithSName":
    raise Exception("this function can only be called from @angle_query")



class PythonModuleBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.ModuleBySName.2 { query_fields if query_fields else '_' }", ModuleBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonModuleBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStarStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], from_name: ast.Expr, into_module: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, from_name, 'from_name'), angle_for(__env, into_module, 'into_module')]))
    return f"python.ImportStarStatement.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportStarStatement

  @staticmethod
  def angle_query(*, from_name: Optional["PythonName"] = None, into_module: Optional["PythonModule"] = None) -> "PythonImportStarStatement":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"python.DeclarationLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional["PythonDeclaration"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "PythonDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")



class PythonClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, bases: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, bases, 'bases')]))
    return f"python.ClassDeclaration.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, bases: Optional[Union[Just[List["PythonName"]], Just[None]]] = None) -> "PythonClassDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.DeclarationToName.2 { query_fields if query_fields else '_' }", DeclarationToName

  @staticmethod
  def angle_query(*, arg: Optional["PythonDeclaration"] = None) -> "PythonDeclarationToName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, definition, 'definition')]))
    return f"python.DeclarationDefinition.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonDeclaration"] = None, definition: Optional["PythonDefinition"] = None) -> "PythonDeclarationDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.DeclarationByName.2 { query_fields if query_fields else '_' }", DeclarationByName

  @staticmethod
  def angle_query(*, arg: Optional["PythonName"] = None) -> "PythonDeclarationByName":
    raise Exception("this function can only be called from @angle_query")



class PythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.Name.1 { query_fields if query_fields else '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatementByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], as_name: ast.Expr, from_name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, as_name, 'as_name'), angle_for(__env, from_name, 'from_name')]))
    return f"python.ImportStatementByName.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportStatementByName

  @staticmethod
  def angle_query(*, as_name: Optional["PythonName"] = None, from_name: Optional["PythonName"] = None) -> "PythonImportStatementByName":
    raise Exception("this function can only be called from @angle_query")



class PythonDocstringContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.DocstringContent.2 { query_fields if query_fields else '_' }", DocstringContent

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonDocstringContent":
    raise Exception("this function can only be called from @angle_query")



class PythonVariableDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, type, 'type')]))
    return f"python.VariableDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", VariableDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonVariableDeclaration"] = None, type: Optional[Union[Just["PythonType"], Just[None]]] = None) -> "PythonVariableDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.Type.1 { query_fields if query_fields else '_' }", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonType":
    raise Exception("this function can only be called from @angle_query")



class PythonDirectXRefsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref')]))
    return f"python.DirectXRefsByFile.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DirectXRefsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["PythonDirectXRef"] = None) -> "PythonDirectXRefsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonVariableBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.VariableBySName.2 { query_fields if query_fields else '_' }", VariableBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonVariableBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationWithLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local_name: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, local_name, 'local_name'), angle_for(__env, declaration, 'declaration')]))
    return f"python.DeclarationWithLocalName.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationWithLocalName

  @staticmethod
  def angle_query(*, local_name: Optional["PythonName"] = None, declaration: Optional["PythonDeclaration"] = None) -> "PythonDeclarationWithLocalName":
    raise Exception("this function can only be called from @angle_query")



class PythonFileDirectXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')]))
    return f"python.FileDirectXRefs.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileDirectXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[List["PythonDirectXRef"]] = None) -> "PythonFileDirectXRefs":
    raise Exception("this function can only be called from @angle_query")



class PythonFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"python.FunctionDeclaration.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonSNameToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.SNameToName.2 { query_fields if query_fields else '_' }", SNameToName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonSNameToName":
    raise Exception("this function can only be called from @angle_query")



class PythonNameToSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.NameToSName.2 { query_fields if query_fields else '_' }", NameToSName

  @staticmethod
  def angle_query(*, arg: Optional["PythonName"] = None) -> "PythonNameToSName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatementByAsName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, import_, 'import_')]))
    return f"python.ImportStatementByAsName.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportStatementByAsName

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, import_: Optional["PythonImportStatement"] = None) -> "PythonImportStatementByAsName":
    raise Exception("this function can only be called from @angle_query")



class PythonFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, is_async: ast.Expr, returns: ast.Expr, params: ast.Expr, posonly_params: ast.Expr, kwonly_params: ast.Expr, star_arg: ast.Expr, star_kwarg: ast.Expr, decorators: ast.Expr, docstring: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, is_async, 'is_async'), angle_for(__env, returns, 'returns'), angle_for(__env, params, 'params'), angle_for(__env, posonly_params, 'posonly_params'), angle_for(__env, kwonly_params, 'kwonly_params'), angle_for(__env, star_arg, 'star_arg'), angle_for(__env, star_kwarg, 'star_kwarg'), angle_for(__env, decorators, 'decorators'), angle_for(__env, docstring, 'docstring')]))
    return f"python.FunctionDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional["PythonFunctionDeclaration"] = None, is_async: Optional[bool] = None, returns: Optional[Union[Just["PythonType"], Just[None]]] = None, params: Optional[List["PythonParameter"]] = None, posonly_params: Optional[Union[Just[List["PythonParameter"]], Just[None]]] = None, kwonly_params: Optional[Union[Just[List["PythonParameter"]], Just[None]]] = None, star_arg: Optional[Union[Just["PythonParameter"], Just[None]]] = None, star_kwarg: Optional[Union[Just["PythonParameter"], Just[None]]] = None, decorators: Optional[Union[Just[List["PythonDecorator"]], Just[None]]] = None, docstring: Optional[Union[Just["PythonDocstring"], Just[None]]] = None) -> "PythonFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStarsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, declaration, 'declaration')]))
    return f"python.ImportStarsByFile.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportStarsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None, declaration: Optional["PythonImportStarStatement"] = None) -> "PythonImportStarsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonIsTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.IsTopLevelDeclaration.3 { query_fields if query_fields else '_' }", IsTopLevelDeclaration

  @staticmethod
  def angle_query(*, arg: Optional["PythonDeclaration"] = None) -> "PythonIsTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonSearchByLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, decl: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, decl, 'decl')]))
    return f"python.SearchByLocalName.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByLocalName

  @staticmethod
  def angle_query(*, name: Optional["PythonLocalName"] = None, decl: Optional["PythonDeclaration"] = None) -> "PythonSearchByLocalName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, declaration, 'declaration')]))
    return f"python.DeclarationsByFile.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None, declaration: Optional["PythonDeclaration"] = None) -> "PythonDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonXRefsViaNameByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')]))
    return f"python.XRefsViaNameByFile.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefsViaNameByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[List["PythonXRefViaName"]] = None) -> "PythonXRefsViaNameByFile":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, declaration, 'declaration')]))
    return f"python.DeclarationWithName.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, declaration: Optional["PythonDeclaration"] = None) -> "PythonDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")



class PythonVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"python.VariableDeclaration.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None) -> "PythonVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonFunctionBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"python.FunctionBySName.2 { query_fields if query_fields else '_' }", FunctionBySName

  @staticmethod
  def angle_query(*, arg: Optional["PythonSName"] = None) -> "PythonFunctionBySName":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStarLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], import_star: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, import_star, 'import_star'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"python.ImportStarLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportStarLocation

  @staticmethod
  def angle_query(*, import_star: Optional["PythonImportStarStatement"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "PythonImportStarLocation":
    raise Exception("this function can only be called from @angle_query")



class PythonImportStatementByAsSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], sname: ast.Expr, import_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, sname, 'sname'), angle_for(__env, import_, 'import_')]))
    return f"python.ImportStatementByAsSName.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ImportStatementByAsSName

  @staticmethod
  def angle_query(*, sname: Optional["PythonSName"] = None, import_: Optional["PythonImportStatement"] = None) -> "PythonImportStatementByAsSName":
    raise Exception("this function can only be called from @angle_query")



class PythonDefinitionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition'), angle_for(__env, declaration, 'declaration')]))
    return f"python.DefinitionDeclaration.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionDeclaration

  @staticmethod
  def angle_query(*, definition: Optional["PythonDefinition"] = None, declaration: Optional["PythonDeclaration"] = None) -> "PythonDefinitionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonContainingTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, container: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, container, 'container')]))
    return f"python.ContainingTopLevelDeclaration.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContainingTopLevelDeclaration

  @staticmethod
  def angle_query(*, declaration: Optional["PythonDeclaration"] = None, container: Optional["PythonDeclaration"] = None) -> "PythonContainingTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")



class PythonSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], local_name: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, local_name, 'local_name'), angle_for(__env, parent, 'parent')]))
    return f"python.SName.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SName

  @staticmethod
  def angle_query(*, local_name: Optional["PythonName"] = None, parent: Optional[Union[Just["PythonSName"], Just[None]]] = None) -> "PythonSName":
    raise Exception("this function can only be called from @angle_query")



class PythonLocalNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lowercase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, lowercase, 'lowercase'), angle_for(__env, name, 'name')]))
    return f"python.LocalNameLowerCase.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LocalNameLowerCase

  @staticmethod
  def angle_query(*, lowercase: Optional[str] = None, name: Optional["PythonLocalName"] = None) -> "PythonLocalNameLowerCase":
    raise Exception("this function can only be called from @angle_query")





class PythonParameter(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, value, 'value')]))
    return f"python.Parameter.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Parameter

  @staticmethod
  def angle_query(*, name: Optional["PythonName"] = None, type: Optional[Union[Just["PythonType"], Just[None]]] = None, value: Optional[Union[Just[str], Just[None]]] = None) -> "PythonParameter":
    raise Exception("this function can only be called from @angle_query")



class PythonXRefViaName(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')]))
    return f"python.XRefViaName.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefViaName

  @staticmethod
  def angle_query(*, target: Optional["PythonName"] = None, source: Optional["SrcByteSpan"] = None) -> "PythonXRefViaName":
    raise Exception("this function can only be called from @angle_query")



class PythonDeclaration(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cls: ast.Expr, func: ast.Expr, variable: ast.Expr, imp: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, cls, 'cls'), angle_for(__env, func, 'func'), angle_for(__env, variable, 'variable'), angle_for(__env, imp, 'imp'), angle_for(__env, module, 'module')]))
    return f"python.Declaration.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Declaration

  @staticmethod
  def angle_query_cls(*, cls: "PythonClassDeclaration") -> "PythonDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_func(*, func: "PythonFunctionDeclaration") -> "PythonDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_variable(*, variable: "PythonVariableDeclaration") -> "PythonDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_imp(*, imp: "PythonImportStatement") -> "PythonDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_module(*, module: "PythonModule") -> "PythonDeclaration":
    raise Exception("this function can only be called from @angle_query")




class PythonDocstring(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location')]))
    return f"python.Docstring.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Docstring

  @staticmethod
  def angle_query(*, location: Optional["SrcByteSpan"] = None) -> "PythonDocstring":
    raise Exception("this function can only be called from @angle_query")



class PythonDefinition(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cls: ast.Expr, func: ast.Expr, variable: ast.Expr, module: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, cls, 'cls'), angle_for(__env, func, 'func'), angle_for(__env, variable, 'variable'), angle_for(__env, module, 'module')]))
    return f"python.Definition.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Definition

  @staticmethod
  def angle_query_cls(*, cls: "PythonClassDefinition") -> "PythonDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_func(*, func: "PythonFunctionDefinition") -> "PythonDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_variable(*, variable: "PythonVariableDefinition") -> "PythonDefinition":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_module(*, module: "PythonModuleDefinition") -> "PythonDefinition":
    raise Exception("this function can only be called from @angle_query")




class PythonDirectXRef(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')]))
    return f"python.DirectXRef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DirectXRef

  @staticmethod
  def angle_query(*, target: Optional["PythonDeclaration"] = None, source: Optional["SrcByteSpan"] = None) -> "PythonDirectXRef":
    raise Exception("this function can only be called from @angle_query")





PythonDecorator = str
