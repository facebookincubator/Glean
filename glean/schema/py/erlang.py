# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationReference.1 {{ target = _, source = _ }}", DeclarationReference

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "ErlangDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationWithFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationWithFqn.1 {{ fqn = _, declaration = _ }}", DeclarationWithFqn

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationWithFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.FunctionDeclaration.1 {{ fqn = _, file = _, span = _ }}", FunctionDeclaration

  @staticmethod
  def angle_query(*, fqn: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "ErlangFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationToFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationToFqn.1 {json.dumps(key)}", DeclarationToFqn

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "ErlangDeclarationToFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.SearchByName.1 {{ name = _, func = _ }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, func: Optional[Tuple[()]] = None) -> "ErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationsByFile.1 {{ file = _, span = _, declaration = _ }}", DeclarationsByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "ErlangDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationLocation.1 {{ declaration = _, file = _, span = _ }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class ErlangNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.NameLowerCase.1 {{ nameLowercase = _, name = _ }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowercase: Optional[str] = None, name: Optional[str] = None) -> "ErlangNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class ErlangXRefsViaFqnByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.XRefsViaFqnByFile.1 {{ file = _, xrefs = _ }}", XRefsViaFqnByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "ErlangXRefsViaFqnByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationUses.1 {{ declaration = _, file = _, span = _ }}", DeclarationUses

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "ErlangDeclarationUses":
    raise Exception("this function can only be called from @angle_query")


