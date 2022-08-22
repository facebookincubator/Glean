# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.NameLowerCase.1 {{ nameLowerCase = _, name = _ }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "PythonNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.LocalName.3 {json.dumps(key)}", LocalName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDefinition.3 {json.dumps(key)}", IsTopLevelDefinition

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ClassDefinition.2 {{ declaration = _, bases = _, keywords = _, decorators = _, docstring = _ }}", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, bases: Optional[Tuple[()]] = None, keywords: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonNonImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.NonImportDeclaration.2 {json.dumps(key)}", NonImportDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonNonImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationReference.3 {{ target = _, source = _ }}", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "PythonDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DefinitionsByFile.3 {{ file = _, span = _, definition = _ }}", DefinitionsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDefinitionsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.Module.1 {{ name = _ }}", Module

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "PythonModule":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DefinitionLocation.3 {{ definition = _, file = _, span = _ }}", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ClassBySName.2 {json.dumps(key)}", ClassBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonClassBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationUses.2 {{ declaration = _, file = _, span = _ }}", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatement.2 {{ from_name = _, as_name = _ }}", ImportStatement

  @staticmethod
  def angle_query(*, from_name: Optional[Tuple[()]] = None, as_name: Optional[Tuple[()]] = None) -> "PythonImportStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ModuleDefinition.2 {{ module = _, docstring = _ }}", ModuleDefinition

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationBySName.2 {json.dumps(key)}", DeclarationBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationWithSName.3 {{ sname = _, declaration = _ }}", DeclarationWithSName

  @staticmethod
  def angle_query(*, sname: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithSName":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ModuleBySName.2 {json.dumps(key)}", ModuleBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonModuleBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStarStatement.3 {{ from_name = _, into_module = _ }}", ImportStarStatement

  @staticmethod
  def angle_query(*, from_name: Optional[Tuple[()]] = None, into_module: Optional[Tuple[()]] = None) -> "PythonImportStarStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationLocation.2 {{ declaration = _, file = _, span = _ }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ClassDeclaration.1 {{ name = _, bases = _ }}", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, bases: Optional[Tuple[()]] = None) -> "PythonClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationToName.2 {json.dumps(key)}", DeclarationToName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationToName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationDefinition.3 {{ declaration = _, definition = _ }}", DeclarationDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDeclarationDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationByName.2 {json.dumps(key)}", DeclarationByName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationByName":
    raise Exception("this function can only be called from @angle_query")

class PythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.Name.1 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatementByName.2 {{ as_name = _, from_name = _ }}", ImportStatementByName

  @staticmethod
  def angle_query(*, as_name: Optional[Tuple[()]] = None, from_name: Optional[Tuple[()]] = None) -> "PythonImportStatementByName":
    raise Exception("this function can only be called from @angle_query")

class PythonDocstringContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DocstringContent.2 {json.dumps(key)}", DocstringContent

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonDocstringContent":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.VariableDefinition.2 {{ declaration = _, type = _ }}", VariableDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "PythonVariableDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.Type.1 {json.dumps(key)}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonType":
    raise Exception("this function can only be called from @angle_query")

class PythonDirectXRefsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DirectXRefsByFile.2 {{ file = _, xref = _ }}", DirectXRefsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None) -> "PythonDirectXRefsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.VariableBySName.2 {json.dumps(key)}", VariableBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonVariableBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationWithLocalName.3 {{ local_name = _, declaration = _ }}", DeclarationWithLocalName

  @staticmethod
  def angle_query(*, local_name: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonFileDirectXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FileDirectXRefs.2 {{ file = _, xrefs = _ }}", FileDirectXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonFileDirectXRefs":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FunctionDeclaration.1 {{ name = _ }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "PythonFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSNameToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.SNameToName.2 {json.dumps(key)}", SNameToName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonSNameToName":
    raise Exception("this function can only be called from @angle_query")

class PythonNameToSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.NameToSName.2 {json.dumps(key)}", NameToSName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonNameToSName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsName.3 {{ name = _, import_ = _ }}", ImportStatementByAsName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None) -> "PythonImportStatementByAsName":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FunctionDefinition.2 {{ declaration = _, is_async = _, returns = _, params = _, posonly_params = _, kwonly_params = _, star_arg = _, star_kwarg = _, decorators = _, docstring = _ }}", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, is_async: Optional[bool] = None, returns: Optional[Tuple[()]] = None, params: Optional[Tuple[()]] = None, posonly_params: Optional[Tuple[()]] = None, kwonly_params: Optional[Tuple[()]] = None, star_arg: Optional[Tuple[()]] = None, star_kwarg: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStarsByFile.3 {{ file = _, span = _, declaration = _ }}", ImportStarsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonImportStarsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDeclaration.3 {json.dumps(key)}", IsTopLevelDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSearchByLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.SearchByLocalName.3 {{ name = _, decl = _ }}", SearchByLocalName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "PythonSearchByLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationsByFile.2 {{ file = _, span = _, declaration = _ }}", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonXRefsViaNameByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.XRefsViaNameByFile.2 {{ file = _, xrefs = _ }}", XRefsViaNameByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonXRefsViaNameByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationWithName.2 {{ name = _, declaration = _ }}", DeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.VariableDeclaration.1 {{ name = _ }}", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "PythonVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FunctionBySName.2 {json.dumps(key)}", FunctionBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonFunctionBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStarLocation.3 {{ import_star = _, file = _, span = _ }}", ImportStarLocation

  @staticmethod
  def angle_query(*, import_star: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonImportStarLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsSName.3 {{ sname = _, import_ = _ }}", ImportStatementByAsSName

  @staticmethod
  def angle_query(*, sname: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None) -> "PythonImportStatementByAsSName":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DefinitionDeclaration.3 {{ definition = _, declaration = _ }}", DefinitionDeclaration

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDefinitionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonContainingTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ContainingTopLevelDeclaration.3 {{ declaration = _, container = _ }}", ContainingTopLevelDeclaration

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "PythonContainingTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.SName.2 {{ local_name = _, parent = _ }}", SName

  @staticmethod
  def angle_query(*, local_name: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "PythonSName":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.LocalNameLowerCase.3 {{ lowercase = _, name = _ }}", LocalNameLowerCase

  @staticmethod
  def angle_query(*, lowercase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "PythonLocalNameLowerCase":
    raise Exception("this function can only be called from @angle_query")


