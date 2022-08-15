# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupThriftThriftFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupThriftThriftFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupThriftThriftResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.thrift.ThriftResolveLocation.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupThriftThriftResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupThriftThriftEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.thrift.ThriftEntityLocation.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupThriftThriftEntityLocation":
    raise Exception("this function can only be called from @angle_query")


