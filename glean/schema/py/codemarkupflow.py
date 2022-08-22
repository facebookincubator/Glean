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
    return f"codemarkup.flow.FlowEntityLocation.2 {{ entity = _, location = _ }}", FlowFlowEntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "CodemarkupFlowFlowEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowResolveLocation.2 {{ location = _, entity = _ }}", FlowFlowResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "CodemarkupFlowFlowResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileReferenceEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", FlowFlowFileReferenceEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupFlowFlowFileReferenceEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileImportDeclEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", FlowFlowFileImportDeclEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupFlowFlowFileImportDeclEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", FlowFlowFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupFlowFlowFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowEntityUses.2 {{ target = _, file = _, span = _ }}", FlowFlowEntityUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "CodemarkupFlowFlowEntityUses":
    raise Exception("this function can only be called from @angle_query")


