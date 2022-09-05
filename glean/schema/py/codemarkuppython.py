# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just
from glean.schema.py.src import *


from glean.schema.codemarkuppython.types import (
    pythonPythonEntityInfo,
    pythonPythonEntityLocation,
    pythonPythonResolveLocation,
    pythonPythonContainsChildEntity,
    pythonPythonFileEntityXRefLocations,
    pythonPythonEntityKind,
    pythonPythonEntityUses,
    pythonNonImportPythonDeclarationInfo,
    pythonNonImportPythonDeclarationKind,
    pythonPythonEntityNameAndLocation,
)


class CodemarkupPythonPythonEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityInfo.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')])) or '_' } }}", pythonPythonEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonPythonEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", pythonPythonEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonPythonResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonResolveLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", pythonPythonResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonPythonContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonContainsChildEntity.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')])) or '_' } }}", pythonPythonContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonPythonFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonFileEntityXRefLocations.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", pythonPythonFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonPythonEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityKind.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')])) or '_' } }}", pythonPythonEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonPythonEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityUses.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", pythonPythonEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonNonImportPythonDeclarationInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.NonImportPythonDeclarationInfo.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, info, 'info')])) or '_' } }}", pythonNonImportPythonDeclarationInfo

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupPythonNonImportPythonDeclarationInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonNonImportPythonDeclarationKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.NonImportPythonDeclarationKind.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, kind, 'kind')])) or '_' } }}", pythonNonImportPythonDeclarationKind

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupPythonNonImportPythonDeclarationKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPythonPythonEntityNameAndLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, name: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.python.PythonEntityNameAndLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", pythonPythonEntityNameAndLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, name: Optional[str] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupPythonPythonEntityNameAndLocation":
    raise Exception("this function can only be called from @angle_query")




