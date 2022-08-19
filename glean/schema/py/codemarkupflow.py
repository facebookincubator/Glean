# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkupflow.types import (
    FlowFlowEntityLocation,
    FlowFlowResolveLocation,
    FlowFlowFileReferenceEntityXRefLocations,
    FlowFlowFileImportDeclEntityXRefLocations,
    FlowFlowFileEntityXRefLocations,
    FlowFlowEntityUses,
)


class CodemarkupFlowFlowEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowEntityLocation.2 { { } }", FlowFlowEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFlowFlowEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowResolveLocation.2 { { } }", FlowFlowResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFlowFlowResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileReferenceEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 { { } }", FlowFlowFileReferenceEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFlowFlowFileReferenceEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileImportDeclEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 { { } }", FlowFlowFileImportDeclEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFlowFlowFileImportDeclEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileEntityXRefLocations.2 { { } }", FlowFlowFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFlowFlowFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowEntityUses.2 { { } }", FlowFlowEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFlowFlowEntityUses":
    raise Exception("this function can only be called from @angle_query")


