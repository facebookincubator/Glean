# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class DocmarkupDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.DocAttr.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityComments(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityComments.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupEntityComments":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityDocAttr.14 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "DocmarkupEntityDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityByDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityByDocAttrKey.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupEntityByDocAttrKey":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.EntityAnnotations.14 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"docmarkup.DocAttrKey.14 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "DocmarkupDocAttrKey":
    raise Exception("this function can only be called from @angle_query")


