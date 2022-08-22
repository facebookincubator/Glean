# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    return f"docmarkup.DocAttr.14 {{ key = _, value = _ }}", DocAttr

  @staticmethod
  def angle_query(*, key: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "DocmarkupDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityComments(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityComments.14 {{ entity = _, file = _, span = _ }}", EntityComments

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "DocmarkupEntityComments":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityDocAttr.14 {json.dumps(key)}", EntityDocAttr

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "DocmarkupEntityDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityByDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityByDocAttrKey.14 {{ key = _, entity = _ }}", EntityByDocAttrKey

  @staticmethod
  def angle_query(*, key: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "DocmarkupEntityByDocAttrKey":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.EntityAnnotations.14 {{ entity = _, annotations = _ }}", EntityAnnotations

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None) -> "DocmarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"docmarkup.DocAttrKey.14 {json.dumps(key)}", DocAttrKey

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "DocmarkupDocAttrKey":
    raise Exception("this function can only be called from @angle_query")


