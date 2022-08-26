# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.search.SearchByNameAndKind.1 {{ }}", SearchSearchByNameAndKind
    return f"codemarkup.search.SearchByNameAndKind.1 { concatenateFields(key) }", SearchSearchByNameAndKind

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByNameAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.search.SearchEntityByName.1 {{ }}", SearchSearchEntityByName
    return f"codemarkup.search.SearchEntityByName.1 { concatenateFields(key) }", SearchSearchEntityByName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchEntityByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchEntityByLowerCaseName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.search.SearchEntityByLowerCaseName.1 {{ }}", SearchSearchEntityByLowerCaseName
    return f"codemarkup.search.SearchEntityByLowerCaseName.1 { concatenateFields(key) }", SearchSearchEntityByLowerCaseName

  @staticmethod
  def angle_query(*, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchEntityByLowerCaseName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.search.SearchByName.1 {{ }}", SearchSearchByName
    return f"codemarkup.search.SearchByName.1 { concatenateFields(key) }", SearchSearchByName

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByName":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchEntityLocationAndKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.search.EntityLocationAndKind.1 {{ }}", SearchEntityLocationAndKind
    return f"codemarkup.search.EntityLocationAndKind.1 { concatenateFields(key) }", SearchEntityLocationAndKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupSearchEntityLocationAndKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchSearchByScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.search.SearchByScope.1 {{ }}", SearchSearchByScope
    return f"codemarkup.search.SearchByScope.1 { concatenateFields(key) }", SearchSearchByScope

  @staticmethod
  def angle_query(*, searchcase: Optional[Tuple[()]] = None, name: Optional[str] = None, scope: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "CodemarkupSearchSearchByScope":
    raise Exception("this function can only be called from @angle_query")


