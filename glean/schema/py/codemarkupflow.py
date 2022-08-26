# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.flow.FlowEntityLocation.2 {{ }}", FlowFlowEntityLocation
    return f"codemarkup.flow.FlowEntityLocation.2 { concatenateFields(key) }", FlowFlowEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.flow.FlowResolveLocation.2 {{ }}", FlowFlowResolveLocation
    return f"codemarkup.flow.FlowResolveLocation.2 { concatenateFields(key) }", FlowFlowResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileReferenceEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 {{ }}", FlowFlowFileReferenceEntityXRefLocations
    return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 { concatenateFields(key) }", FlowFlowFileReferenceEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileReferenceEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileImportDeclEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 {{ }}", FlowFlowFileImportDeclEntityXRefLocations
    return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 { concatenateFields(key) }", FlowFlowFileImportDeclEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileImportDeclEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.flow.FlowFileEntityXRefLocations.2 {{ }}", FlowFlowFileEntityXRefLocations
    return f"codemarkup.flow.FlowFileEntityXRefLocations.2 { concatenateFields(key) }", FlowFlowFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.flow.FlowEntityUses.2 {{ }}", FlowFlowEntityUses
    return f"codemarkup.flow.FlowEntityUses.2 { concatenateFields(key) }", FlowFlowEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowEntityUses":
    raise Exception("this function can only be called from @angle_query")


