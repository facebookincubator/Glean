# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSDocmarkupDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.DocAttr.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDocmarkupDocAttr":
    raise Exception("this function can only be called from @angle_query")

class GSDocmarkupEntityComments(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityComments.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDocmarkupEntityComments":
    raise Exception("this function can only be called from @angle_query")

class GSDocmarkupEntityDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityDocAttr.14 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDocmarkupEntityDocAttr":
    raise Exception("this function can only be called from @angle_query")

class GSDocmarkupEntityByDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityByDocAttrKey.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDocmarkupEntityByDocAttrKey":
    raise Exception("this function can only be called from @angle_query")

class GSDocmarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityAnnotations.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDocmarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class GSDocmarkupDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.DocAttrKey.14 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDocmarkupDocAttrKey":
    raise Exception("this function can only be called from @angle_query")


