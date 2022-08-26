# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.searchbuck.types import (
    BuckSearchDefinition,
    BuckSearchFile,
    BuckSearchByFQN,
)


class SearchBuckSearchDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.buck.SearchDefinition.1 {{ }}", BuckSearchDefinition
    return f"search.buck.SearchDefinition.1 { concatenateFields(key) }", BuckSearchDefinition

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchBuckSearchDefinition":
    raise Exception("this function can only be called from @angle_query")

class SearchBuckSearchFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.buck.SearchFile.1 {{ }}", BuckSearchFile
    return f"search.buck.SearchFile.1 { concatenateFields(key) }", BuckSearchFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "SearchBuckSearchFile":
    raise Exception("this function can only be called from @angle_query")

class SearchBuckSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"search.buck.SearchByFQN.1 {{ }}", BuckSearchByFQN
    return f"search.buck.SearchByFQN.1 { concatenateFields(key) }", BuckSearchByFQN

  @staticmethod
  def angle_query(*, subdir: Optional[Tuple[()]] = None, path: Optional[str] = None, name: Optional[str] = None, entity: Optional[Tuple[()]] = None) -> "SearchBuckSearchByFQN":
    raise Exception("this function can only be called from @angle_query")


