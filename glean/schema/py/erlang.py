# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSErlangDeclarationReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.DeclarationReference.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangDeclarationReference":
    raise Exception("this function can only be called from @angle_query")

class GSErlangDeclarationWithFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.DeclarationWithFqn.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangDeclarationWithFqn":
    raise Exception("this function can only be called from @angle_query")

class GSErlangFunctionDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.FunctionDeclaration.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangFunctionDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSErlangDeclarationToFqn(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.DeclarationToFqn.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangDeclarationToFqn":
    raise Exception("this function can only be called from @angle_query")

class GSErlangSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.SearchByName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSErlangDeclarationsByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.DeclarationsByFile.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangDeclarationsByFile":
    raise Exception("this function can only be called from @angle_query")

class GSErlangDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.DeclarationLocation.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GSErlangNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.NameLowerCase.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSErlangXRefsViaFqnByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.XRefsViaFqnByFile.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangXRefsViaFqnByFile":
    raise Exception("this function can only be called from @angle_query")

class GSErlangDeclarationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"erlang.DeclarationUses.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSErlangDeclarationUses":
    raise Exception("this function can only be called from @angle_query")


