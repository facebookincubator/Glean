# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.codemarkupthrift.types import (
    ThriftThriftFileEntityXRefLocations,
    ThriftThriftResolveLocation,
    ThriftThriftEntityLocation,
)


class CodemarkupThriftThriftFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 {{ }}", ThriftThriftFileEntityXRefLocations
    return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 { concatenateFields(key) }", ThriftThriftFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupThriftThriftFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupThriftThriftResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.thrift.ThriftResolveLocation.4 {{ }}", ThriftThriftResolveLocation
    return f"codemarkup.thrift.ThriftResolveLocation.4 { concatenateFields(key) }", ThriftThriftResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupThriftThriftResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupThriftThriftEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.thrift.ThriftEntityLocation.4 {{ }}", ThriftThriftEntityLocation
    return f"codemarkup.thrift.ThriftEntityLocation.4 { concatenateFields(key) }", ThriftThriftEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupThriftThriftEntityLocation":
    raise Exception("this function can only be called from @angle_query")


