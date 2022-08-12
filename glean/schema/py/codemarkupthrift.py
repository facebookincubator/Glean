# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupThriftThriftFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupThriftThriftFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupThriftThriftResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.thrift.ThriftResolveLocation.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupThriftThriftResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupThriftThriftEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.thrift.ThriftEntityLocation.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupThriftThriftEntityLocation":
    raise Exception("this function can only be called from @angle_query")


