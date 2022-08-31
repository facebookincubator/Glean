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
    return f"erlang.DeclarationReference.1 {{ target = {angle_for(__env, target)}, source = {angle_for(__env, source)} }}", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "ErlangDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationWithFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fqn: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationWithFqn.1 {{ fqn = {angle_for(__env, fqn)}, declaration = {angle_for(__env, declaration)} }}", DeclarationWithFqn

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationWithFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fqn: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.FunctionDeclaration.1 {{ fqn = {angle_for(__env, fqn)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "ErlangFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationToFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationToFqn.1 {angle_for(__env, arg)}", DeclarationToFqn

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ErlangDeclarationToFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, func: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.SearchByName.1 {{ name = {angle_for(__env, name)}, func = {angle_for(__env, func)} }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, func: Optional[Tuple[()]] = None) -> "ErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationsByFile.1 {{ file = {angle_for(__env, file)}, span = {angle_for(__env, span)}, declaration = {angle_for(__env, declaration)} }}", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationLocation.1 {{ declaration = {angle_for(__env, declaration)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class ErlangNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowercase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.NameLowerCase.1 {{ nameLowercase = {angle_for(__env, nameLowercase)}, name = {angle_for(__env, name)} }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: Optional[str] = None, name: Optional[str] = None) -> "ErlangNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class ErlangXRefsViaFqnByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.XRefsViaFqnByFile.1 {{ file = {angle_for(__env, file)}, xrefs = {angle_for(__env, xrefs)} }}", XRefsViaFqnByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[Tuple[()]] = None) -> "ErlangXRefsViaFqnByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"erlang.DeclarationUses.1 {{ declaration = {angle_for(__env, declaration)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationUses":
    raise Exception("this function can only be called from @angle_query")


