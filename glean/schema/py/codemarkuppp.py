# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupPpPpEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpEntityInfo.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PPEntityLocation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPpResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpResolveLocation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPpEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpEntityKind.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpEntityKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPpFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpFileEntityXRefLocations.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPpEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpEntityTraceXRefLocations.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPpResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpResolveTraceLocation.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPpFileEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpFileEntityTraceXRefLocations.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpFileEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupPpPpFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codemarkup.pp.PpFileEntityTraceLocations.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupPpPpFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")


