# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkupsearch.types import (
    SearchSearchByNameAndKind,
    SearchSearchEntityByName,
    SearchSearchEntityByLowerCaseName,
    SearchSearchByName,
    SearchEntityLocationAndKind,
    SearchSearchByScope,
)


class CodemarkupSearchSearchByNameAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByNameAndKind.1 {{ searchcase = _, name = _, entity = _, location = _, kind = _ }}", SearchSearchByNameAndKind

  @staticmethod
  def angle_query(*, searchcase: Tuple[()], name: str, entity: Tuple[()], location: Tuple[()], kind: Tuple[()]) -> "CodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByName.1 {{ name = _, entity = _, location = _, kind = _ }}", SearchSearchEntityByName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()], location: Tuple[()], kind: Tuple[()]) -> "CodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 {{ name = _, entity = _, location = _, kind = _ }}", SearchSearchEntityByLowerCaseName

  @staticmethod
  def angle_query(*, name: str, entity: Tuple[()], location: Tuple[()], kind: Tuple[()]) -> "CodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByName.1 {{ searchcase = _, name = _, entity = _, location = _, kind = _, language = _ }}", SearchSearchByName

  @staticmethod
  def angle_query(*, searchcase: Tuple[()], name: str, entity: Tuple[()], location: Tuple[()], kind: Tuple[()], language: Tuple[()]) -> "CodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.EntityLocationAndKind.1 {{ entity = _, location = _, kind = _ }}", SearchEntityLocationAndKind

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()], kind: Tuple[()]) -> "CodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByScope.1 {{ searchcase = _, name = _, scope = _, entity = _, location = _, kind = _, language = _ }}", SearchSearchByScope

  @staticmethod
  def angle_query(*, searchcase: Tuple[()], name: str, scope: Tuple[()], entity: Tuple[()], location: Tuple[()], kind: Tuple[()], language: Tuple[()]) -> "CodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")


