# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
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
    return f"erlang.DeclarationReference.1 { { } }", DeclarationReference

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationWithFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationWithFqn.1 { { } }", DeclarationWithFqn

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangDeclarationWithFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.FunctionDeclaration.1 { { } }", FunctionDeclaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationToFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationToFqn.1 { json.dumps(key) }", DeclarationToFqn

  @staticmethod
  def angle_query(*, name: str) -> "ErlangDeclarationToFqn":
    raise Exception("this function can only be called from @angle_query")

class ErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.SearchByName.1 { { } }", SearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationsByFile.1 { { } }", DeclarationsByFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationLocation.1 { { } }", DeclarationLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class ErlangNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.NameLowerCase.1 { { } }", NameLowerCase

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class ErlangXRefsViaFqnByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.XRefsViaFqnByFile.1 { { } }", XRefsViaFqnByFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangXRefsViaFqnByFile":
    raise Exception("this function can only be called from @angle_query")

class ErlangDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"erlang.DeclarationUses.1 { { } }", DeclarationUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "ErlangDeclarationUses":
    raise Exception("this function can only be called from @angle_query")


