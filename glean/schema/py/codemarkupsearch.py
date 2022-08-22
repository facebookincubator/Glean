# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByName.1 {{ name = _, entity = _, location = _, kind = _ }}", SearchSearchEntityByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 {{ name = _, entity = _, location = _, kind = _ }}", SearchSearchEntityByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByName.1 {{ searchcase = _, name = _, entity = _, location = _, kind = _, language = _ }}", SearchSearchByName

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.EntityLocationAndKind.1 {{ entity = _, location = _, kind = _ }}", SearchEntityLocationAndKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.search.SearchByScope.1 {{ searchcase = _, name = _, scope = _, entity = _, location = _, kind = _, language = _ }}", SearchSearchByScope

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")


