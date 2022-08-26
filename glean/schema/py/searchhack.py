# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.searchhack.types import (
    HackQueryToScope,
    HackSearchInEnum,
    HackSearchInContext,
    HackSearchInNamespace,
    HackSearchByName,
    HackSearchInContainerOrEnum,
    HackSearchInContainer,
)


class SearchHackQueryToScope(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hack.QueryToScope.7 {{ }}", HackQueryToScope
    return f"search.hack.QueryToScope.7 { concatenateFields(key) }", HackQueryToScope

  @staticmethod
  def angle_query(*, query: Optional[Tuple[()]] = None, scopeName: Optional[Tuple[()]] = None, scopeNamespace: Optional[Tuple[()]] = None) -> "SearchHackQueryToScope":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hack.SearchInEnum.7 {{ }}", HackSearchInEnum
    return f"search.hack.SearchInEnum.7 { concatenateFields(key) }", HackSearchInEnum

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, enumName: Optional[Tuple[()]] = None, enumNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hack.SearchInContext.7 {{ }}", HackSearchInContext
    return f"search.hack.SearchInContext.7 { concatenateFields(key) }", HackSearchInContext

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, contextName: Optional[Tuple[()]] = None, contextNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContext":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInNamespace(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hack.SearchInNamespace.7 {{ }}", HackSearchInNamespace
    return f"search.hack.SearchInNamespace.7 { concatenateFields(key) }", HackSearchInNamespace

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, namespace_: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInNamespace":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hack.SearchByName.7 {{ }}", HackSearchByName
    return f"search.hack.SearchByName.7 { concatenateFields(key) }", HackSearchByName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchByName":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainerOrEnum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hack.SearchInContainerOrEnum.7 {{ }}", HackSearchInContainerOrEnum
    return f"search.hack.SearchInContainerOrEnum.7 { concatenateFields(key) }", HackSearchInContainerOrEnum

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, contextName: Optional[Tuple[()]] = None, contextNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContainerOrEnum":
    raise Exception("this function can only be called from @angle_query")

class SearchHackSearchInContainer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.hack.SearchInContainer.7 {{ }}", HackSearchInContainer
    return f"search.hack.SearchInContainer.7 { concatenateFields(key) }", HackSearchInContainer

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, containerName: Optional[Tuple[()]] = None, containerNamespace: Optional[Tuple[()]] = None, decl: Optional[Tuple[()]] = None) -> "SearchHackSearchInContainer":
    raise Exception("this function can only be called from @angle_query")


