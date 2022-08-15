# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class CodemarkupThriftThriftFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupThriftThriftFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupThriftThriftResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.thrift.ThriftResolveLocation.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupThriftThriftResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupThriftThriftEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.thrift.ThriftEntityLocation.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupThriftThriftEntityLocation":
    raise Exception("this function can only be called from @angle_query")


