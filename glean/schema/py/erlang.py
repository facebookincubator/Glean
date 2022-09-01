# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.erlang.types import (
    DeclarationReference,
    DeclarationWithFqn,
    FunctionDeclaration,
    DeclarationToFqn,
    SearchByName,
    DeclarationsByFile,
    DeclarationLocation,
    NameLowerCase,
    XRefsViaFqnByFile,
    DeclarationUses,
)


class ErlangDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationReference.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')])) or '_' } }}", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "ErlangDeclarationReference":
    raise Exception("this function can only be called from @angle_query")



class ErlangDeclarationWithFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fqn: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationWithFqn.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fqn, 'fqn'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", DeclarationWithFqn

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationWithFqn":
    raise Exception("this function can only be called from @angle_query")



class ErlangFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fqn: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.FunctionDeclaration.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fqn, 'fqn'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "ErlangFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")



class ErlangDeclarationToFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationToFqn.1 { angle_for(__env, arg, None) or '_' }", DeclarationToFqn

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ErlangDeclarationToFqn":
    raise Exception("this function can only be called from @angle_query")



class ErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, func: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.SearchByName.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, func, 'func')])) or '_' } }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, func: Optional[Tuple[()]] = None) -> "ErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")



class ErlangDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationsByFile.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")



class ErlangDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationLocation.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")



class ErlangNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowercase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.NameLowerCase.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowercase, 'nameLowercase'), angle_for(__env, name, 'name')])) or '_' } }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: Optional[str] = None, name: Optional[str] = None) -> "ErlangNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class ErlangXRefsViaFqnByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.XRefsViaFqnByFile.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')])) or '_' } }}", XRefsViaFqnByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[List[Tuple[()]]] = None) -> "ErlangXRefsViaFqnByFile":
    raise Exception("this function can only be called from @angle_query")



class ErlangDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationUses.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationUses":
    raise Exception("this function can only be called from @angle_query")




