# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.searchbuck.types import (
    BuckSearchDefinition,
    BuckSearchFile,
    BuckSearchByFQN,
)


class SearchBuckSearchDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.buck.SearchDefinition.1 { { } }", BuckSearchDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchBuckSearchDefinition":
    raise Exception("this function can only be called from @angle_query")

class SearchBuckSearchFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.buck.SearchFile.1 { { } }", BuckSearchFile

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchBuckSearchFile":
    raise Exception("this function can only be called from @angle_query")

class SearchBuckSearchByFQN(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"search.buck.SearchByFQN.1 { { } }", BuckSearchByFQN

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SearchBuckSearchByFQN":
    raise Exception("this function can only be called from @angle_query")


