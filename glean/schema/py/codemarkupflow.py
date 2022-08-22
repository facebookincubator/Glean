# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowResolveLocation.2 {{ location = _, entity = _ }}", FlowFlowResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileReferenceEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", FlowFlowFileReferenceEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileReferenceEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileImportDeclEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", FlowFlowFileImportDeclEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileImportDeclEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", FlowFlowFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowEntityUses.2 {{ target = _, file = _, span = _ }}", FlowFlowEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowEntityUses":
    raise Exception("this function can only be called from @angle_query")


