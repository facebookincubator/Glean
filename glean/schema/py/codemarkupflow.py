# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupFlowFlowEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.flow.FlowEntityLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupFlowFlowEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFlowFlowResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.flow.FlowResolveLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupFlowFlowResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFlowFlowFileReferenceEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupFlowFlowFileReferenceEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFlowFlowFileImportDeclEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupFlowFlowFileImportDeclEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFlowFlowFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.flow.FlowFileEntityXRefLocations.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupFlowFlowFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFlowFlowEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.flow.FlowEntityUses.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupFlowFlowEntityUses":
    raise Exception("this function can only be called from @angle_query")


