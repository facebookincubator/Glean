# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.docmarkup.types import (
    DocAttr,
    EntityComments,
    EntityDocAttr,
    EntityByDocAttrKey,
    EntityAnnotations,
    DocAttrKey,
)


class DocmarkupDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.DocAttr.14 { { } }", DocAttr

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityComments(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityComments.14 { { } }", EntityComments

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupEntityComments":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityDocAttr.14 { json.dumps(key) }", EntityDocAttr

  @staticmethod
  def angle_query(*, name: str) -> "DocmarkupEntityDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityByDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityByDocAttrKey.14 { { } }", EntityByDocAttrKey

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupEntityByDocAttrKey":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityAnnotations.14 { { } }", EntityAnnotations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DocmarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.DocAttrKey.14 { json.dumps(key) }", DocAttrKey

  @staticmethod
  def angle_query(*, name: str) -> "DocmarkupDocAttrKey":
    raise Exception("this function can only be called from @angle_query")


