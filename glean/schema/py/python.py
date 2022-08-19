# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
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
    return f"python.NameLowerCase.1 { { } }", NameLowerCase

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.LocalName.3 { json.dumps(key) }", LocalName

  @staticmethod
  def angle_query(*, name: str) -> "PythonLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDefinition.3 { json.dumps(key) }", IsTopLevelDefinition

  @staticmethod
  def angle_query(*, name: str) -> "PythonIsTopLevelDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ClassDefinition.2 { { } }", ClassDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonNonImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.NonImportDeclaration.2 { json.dumps(key) }", NonImportDeclaration

  @staticmethod
  def angle_query(*, name: str) -> "PythonNonImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationReference.3 { { } }", DeclarationReference

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DefinitionsByFile.3 { { } }", DefinitionsByFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDefinitionsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.Module.1 { { } }", Module

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonModule":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DefinitionLocation.3 { { } }", DefinitionLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ClassBySName.2 { json.dumps(key) }", ClassBySName

  @staticmethod
  def angle_query(*, name: str) -> "PythonClassBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationUses.2 { { } }", DeclarationUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatement.2 { { } }", ImportStatement

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonImportStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ModuleDefinition.2 { { } }", ModuleDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationBySName.2 { json.dumps(key) }", DeclarationBySName

  @staticmethod
  def angle_query(*, name: str) -> "PythonDeclarationBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationWithSName.3 { { } }", DeclarationWithSName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationWithSName":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ModuleBySName.2 { json.dumps(key) }", ModuleBySName

  @staticmethod
  def angle_query(*, name: str) -> "PythonModuleBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStarStatement.3 { { } }", ImportStarStatement

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonImportStarStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationLocation.2 { { } }", DeclarationLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ClassDeclaration.1 { { } }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationToName.2 { json.dumps(key) }", DeclarationToName

  @staticmethod
  def angle_query(*, name: str) -> "PythonDeclarationToName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationDefinition.3 { { } }", DeclarationDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationByName.2 { json.dumps(key) }", DeclarationByName

  @staticmethod
  def angle_query(*, name: str) -> "PythonDeclarationByName":
    raise Exception("this function can only be called from @angle_query")

class PythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.Name.1 { json.dumps(key) }", Name

  @staticmethod
  def angle_query(*, name: str) -> "PythonName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatementByName.2 { { } }", ImportStatementByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonImportStatementByName":
    raise Exception("this function can only be called from @angle_query")

class PythonDocstringContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DocstringContent.2 { json.dumps(key) }", DocstringContent

  @staticmethod
  def angle_query(*, name: str) -> "PythonDocstringContent":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.VariableDefinition.2 { { } }", VariableDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonVariableDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.Type.1 { json.dumps(key) }", Type

  @staticmethod
  def angle_query(*, name: str) -> "PythonType":
    raise Exception("this function can only be called from @angle_query")

class PythonDirectXRefsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DirectXRefsByFile.2 { { } }", DirectXRefsByFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDirectXRefsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.VariableBySName.2 { json.dumps(key) }", VariableBySName

  @staticmethod
  def angle_query(*, name: str) -> "PythonVariableBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationWithLocalName.3 { { } }", DeclarationWithLocalName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationWithLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonFileDirectXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FileDirectXRefs.2 { { } }", FileDirectXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonFileDirectXRefs":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FunctionDeclaration.1 { { } }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSNameToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.SNameToName.2 { json.dumps(key) }", SNameToName

  @staticmethod
  def angle_query(*, name: str) -> "PythonSNameToName":
    raise Exception("this function can only be called from @angle_query")

class PythonNameToSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.NameToSName.2 { json.dumps(key) }", NameToSName

  @staticmethod
  def angle_query(*, name: str) -> "PythonNameToSName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsName.3 { { } }", ImportStatementByAsName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonImportStatementByAsName":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FunctionDefinition.2 { { } }", FunctionDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStarsByFile.3 { { } }", ImportStarsByFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonImportStarsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.IsTopLevelDeclaration.3 { json.dumps(key) }", IsTopLevelDeclaration

  @staticmethod
  def angle_query(*, name: str) -> "PythonIsTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSearchByLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.SearchByLocalName.3 { { } }", SearchByLocalName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonSearchByLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationsByFile.2 { { } }", DeclarationsByFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonXRefsViaNameByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.XRefsViaNameByFile.2 { { } }", XRefsViaNameByFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonXRefsViaNameByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DeclarationWithName.2 { { } }", DeclarationWithName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.VariableDeclaration.1 { { } }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.FunctionBySName.2 { json.dumps(key) }", FunctionBySName

  @staticmethod
  def angle_query(*, name: str) -> "PythonFunctionBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStarLocation.3 { { } }", ImportStarLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonImportStarLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ImportStatementByAsSName.3 { { } }", ImportStatementByAsSName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonImportStatementByAsSName":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.DefinitionDeclaration.3 { { } }", DefinitionDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonDefinitionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonContainingTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.ContainingTopLevelDeclaration.3 { { } }", ContainingTopLevelDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonContainingTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.SName.2 { { } }", SName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonSName":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"python.LocalNameLowerCase.3 { { } }", LocalNameLowerCase

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "PythonLocalNameLowerCase":
    raise Exception("this function can only be called from @angle_query")


