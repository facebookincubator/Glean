# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.NameLowerCase.1 {{ }}", NameLowerCase
    return f"python.NameLowerCase.1 { concatenateFields(key) }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "PythonNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.LocalName.3 {{ }}", LocalName
    return f"python.LocalName.3 {key}", LocalName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.IsTopLevelDefinition.3 {{ }}", IsTopLevelDefinition
    return f"python.IsTopLevelDefinition.3 {key}", IsTopLevelDefinition

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ClassDefinition.2 {{ }}", ClassDefinition
    return f"python.ClassDefinition.2 { concatenateFields(key) }", ClassDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, bases: Optional[Tuple[()]] = None, keywords: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonClassDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonNonImportDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.NonImportDeclaration.2 {{ }}", NonImportDeclaration
    return f"python.NonImportDeclaration.2 {key}", NonImportDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonNonImportDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationReference.3 {{ }}", DeclarationReference
    return f"python.DeclarationReference.3 { concatenateFields(key) }", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "PythonDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DefinitionsByFile.3 {{ }}", DefinitionsByFile
    return f"python.DefinitionsByFile.3 { concatenateFields(key) }", DefinitionsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDefinitionsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.Module.1 {{ }}", Module
    return f"python.Module.1 { concatenateFields(key) }", Module

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "PythonModule":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DefinitionLocation.3 {{ }}", DefinitionLocation
    return f"python.DefinitionLocation.3 { concatenateFields(key) }", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ClassBySName.2 {{ }}", ClassBySName
    return f"python.ClassBySName.2 {key}", ClassBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonClassBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationUses.2 {{ }}", DeclarationUses
    return f"python.DeclarationUses.2 { concatenateFields(key) }", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationUses":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ImportStatement.2 {{ }}", ImportStatement
    return f"python.ImportStatement.2 { concatenateFields(key) }", ImportStatement

  @staticmethod
  def angle_query(*, from_name: Optional[Tuple[()]] = None, as_name: Optional[Tuple[()]] = None) -> "PythonImportStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ModuleDefinition.2 {{ }}", ModuleDefinition
    return f"python.ModuleDefinition.2 { concatenateFields(key) }", ModuleDefinition

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonModuleDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationBySName.2 {{ }}", DeclarationBySName
    return f"python.DeclarationBySName.2 {key}", DeclarationBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationWithSName.3 {{ }}", DeclarationWithSName
    return f"python.DeclarationWithSName.3 { concatenateFields(key) }", DeclarationWithSName

  @staticmethod
  def angle_query(*, sname: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithSName":
    raise Exception("this function can only be called from @angle_query")

class PythonModuleBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ModuleBySName.2 {{ }}", ModuleBySName
    return f"python.ModuleBySName.2 {key}", ModuleBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonModuleBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarStatement(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ImportStarStatement.3 {{ }}", ImportStarStatement
    return f"python.ImportStarStatement.3 { concatenateFields(key) }", ImportStarStatement

  @staticmethod
  def angle_query(*, from_name: Optional[Tuple[()]] = None, into_module: Optional[Tuple[()]] = None) -> "PythonImportStarStatement":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationLocation.2 {{ }}", DeclarationLocation
    return f"python.DeclarationLocation.2 { concatenateFields(key) }", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonClassDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ClassDeclaration.1 {{ }}", ClassDeclaration
    return f"python.ClassDeclaration.1 { concatenateFields(key) }", ClassDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, bases: Optional[Tuple[()]] = None) -> "PythonClassDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationToName.2 {{ }}", DeclarationToName
    return f"python.DeclarationToName.2 {key}", DeclarationToName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationToName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationDefinition.3 {{ }}", DeclarationDefinition
    return f"python.DeclarationDefinition.3 { concatenateFields(key) }", DeclarationDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "PythonDeclarationDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationByName.2 {{ }}", DeclarationByName
    return f"python.DeclarationByName.2 {key}", DeclarationByName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonDeclarationByName":
    raise Exception("this function can only be called from @angle_query")

class PythonName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.Name.1 {{ }}", Name
    return f"python.Name.1 {key}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ImportStatementByName.2 {{ }}", ImportStatementByName
    return f"python.ImportStatementByName.2 { concatenateFields(key) }", ImportStatementByName

  @staticmethod
  def angle_query(*, as_name: Optional[Tuple[()]] = None, from_name: Optional[Tuple[()]] = None) -> "PythonImportStatementByName":
    raise Exception("this function can only be called from @angle_query")

class PythonDocstringContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DocstringContent.2 {{ }}", DocstringContent
    return f"python.DocstringContent.2 {key}", DocstringContent

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonDocstringContent":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.VariableDefinition.2 {{ }}", VariableDefinition
    return f"python.VariableDefinition.2 { concatenateFields(key) }", VariableDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None) -> "PythonVariableDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.Type.1 {{ }}", Type
    return f"python.Type.1 {key}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "PythonType":
    raise Exception("this function can only be called from @angle_query")

class PythonDirectXRefsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DirectXRefsByFile.2 {{ }}", DirectXRefsByFile
    return f"python.DirectXRefsByFile.2 { concatenateFields(key) }", DirectXRefsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None) -> "PythonDirectXRefsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.VariableBySName.2 {{ }}", VariableBySName
    return f"python.VariableBySName.2 {key}", VariableBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonVariableBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationWithLocalName.3 {{ }}", DeclarationWithLocalName
    return f"python.DeclarationWithLocalName.3 { concatenateFields(key) }", DeclarationWithLocalName

  @staticmethod
  def angle_query(*, local_name: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonFileDirectXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.FileDirectXRefs.2 {{ }}", FileDirectXRefs
    return f"python.FileDirectXRefs.2 { concatenateFields(key) }", FileDirectXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonFileDirectXRefs":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.FunctionDeclaration.1 {{ }}", FunctionDeclaration
    return f"python.FunctionDeclaration.1 { concatenateFields(key) }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "PythonFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSNameToName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.SNameToName.2 {{ }}", SNameToName
    return f"python.SNameToName.2 {key}", SNameToName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonSNameToName":
    raise Exception("this function can only be called from @angle_query")

class PythonNameToSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.NameToSName.2 {{ }}", NameToSName
    return f"python.NameToSName.2 {key}", NameToSName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonNameToSName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ImportStatementByAsName.3 {{ }}", ImportStatementByAsName
    return f"python.ImportStatementByAsName.3 { concatenateFields(key) }", ImportStatementByAsName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None) -> "PythonImportStatementByAsName":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.FunctionDefinition.2 {{ }}", FunctionDefinition
    return f"python.FunctionDefinition.2 { concatenateFields(key) }", FunctionDefinition

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, is_async: Optional[bool] = None, returns: Optional[Tuple[()]] = None, params: Optional[Tuple[()]] = None, posonly_params: Optional[Tuple[()]] = None, kwonly_params: Optional[Tuple[()]] = None, star_arg: Optional[Tuple[()]] = None, star_kwarg: Optional[Tuple[()]] = None, decorators: Optional[Tuple[()]] = None, docstring: Optional[Tuple[()]] = None) -> "PythonFunctionDefinition":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ImportStarsByFile.3 {{ }}", ImportStarsByFile
    return f"python.ImportStarsByFile.3 { concatenateFields(key) }", ImportStarsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonImportStarsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonIsTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.IsTopLevelDeclaration.3 {{ }}", IsTopLevelDeclaration
    return f"python.IsTopLevelDeclaration.3 {key}", IsTopLevelDeclaration

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonIsTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSearchByLocalName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.SearchByLocalName.3 {{ }}", SearchByLocalName
    return f"python.SearchByLocalName.3 { concatenateFields(key) }", SearchByLocalName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "PythonSearchByLocalName":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationsByFile.2 {{ }}", DeclarationsByFile
    return f"python.DeclarationsByFile.2 { concatenateFields(key) }", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonXRefsViaNameByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.XRefsViaNameByFile.2 {{ }}", XRefsViaNameByFile
    return f"python.XRefsViaNameByFile.2 { concatenateFields(key) }", XRefsViaNameByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "PythonXRefsViaNameByFile":
    raise Exception("this function can only be called from @angle_query")

class PythonDeclarationWithName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DeclarationWithName.2 {{ }}", DeclarationWithName
    return f"python.DeclarationWithName.2 { concatenateFields(key) }", DeclarationWithName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDeclarationWithName":
    raise Exception("this function can only be called from @angle_query")

class PythonVariableDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.VariableDeclaration.1 {{ }}", VariableDeclaration
    return f"python.VariableDeclaration.1 { concatenateFields(key) }", VariableDeclaration

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "PythonVariableDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonFunctionBySName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.FunctionBySName.2 {{ }}", FunctionBySName
    return f"python.FunctionBySName.2 {key}", FunctionBySName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "PythonFunctionBySName":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStarLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ImportStarLocation.3 {{ }}", ImportStarLocation
    return f"python.ImportStarLocation.3 { concatenateFields(key) }", ImportStarLocation

  @staticmethod
  def angle_query(*, import_star: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "PythonImportStarLocation":
    raise Exception("this function can only be called from @angle_query")

class PythonImportStatementByAsSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ImportStatementByAsSName.3 {{ }}", ImportStatementByAsSName
    return f"python.ImportStatementByAsSName.3 { concatenateFields(key) }", ImportStatementByAsSName

  @staticmethod
  def angle_query(*, sname: Optional[Tuple[()]] = None, import_: Optional[Tuple[()]] = None) -> "PythonImportStatementByAsSName":
    raise Exception("this function can only be called from @angle_query")

class PythonDefinitionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.DefinitionDeclaration.3 {{ }}", DefinitionDeclaration
    return f"python.DefinitionDeclaration.3 { concatenateFields(key) }", DefinitionDeclaration

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "PythonDefinitionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonContainingTopLevelDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.ContainingTopLevelDeclaration.3 {{ }}", ContainingTopLevelDeclaration
    return f"python.ContainingTopLevelDeclaration.3 { concatenateFields(key) }", ContainingTopLevelDeclaration

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, container: Optional[Tuple[()]] = None) -> "PythonContainingTopLevelDeclaration":
    raise Exception("this function can only be called from @angle_query")

class PythonSName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.SName.2 {{ }}", SName
    return f"python.SName.2 { concatenateFields(key) }", SName

  @staticmethod
  def angle_query(*, local_name: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "PythonSName":
    raise Exception("this function can only be called from @angle_query")

class PythonLocalNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"python.LocalNameLowerCase.3 {{ }}", LocalNameLowerCase
    return f"python.LocalNameLowerCase.3 { concatenateFields(key) }", LocalNameLowerCase

  @staticmethod
  def angle_query(*, lowercase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "PythonLocalNameLowerCase":
    raise Exception("this function can only be called from @angle_query")


