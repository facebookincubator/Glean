# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"docmarkup.DocAttr.14 {{ }}", DocAttr
    return f"docmarkup.DocAttr.14 { concatenateFields(key) }", DocAttr

  @staticmethod
  def angle_query(*, key: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "DocmarkupDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityComments(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"docmarkup.EntityComments.14 {{ }}", EntityComments
    return f"docmarkup.EntityComments.14 { concatenateFields(key) }", EntityComments

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "DocmarkupEntityComments":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"docmarkup.EntityDocAttr.14 {{ }}", EntityDocAttr
    return f"docmarkup.EntityDocAttr.14 {key}", EntityDocAttr

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "DocmarkupEntityDocAttr":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityByDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"docmarkup.EntityByDocAttrKey.14 {{ }}", EntityByDocAttrKey
    return f"docmarkup.EntityByDocAttrKey.14 { concatenateFields(key) }", EntityByDocAttrKey

  @staticmethod
  def angle_query(*, key: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "DocmarkupEntityByDocAttrKey":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"docmarkup.EntityAnnotations.14 {{ }}", EntityAnnotations
    return f"docmarkup.EntityAnnotations.14 { concatenateFields(key) }", EntityAnnotations

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, annotations: Optional[Tuple[()]] = None) -> "DocmarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class DocmarkupDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"docmarkup.DocAttrKey.14 {{ }}", DocAttrKey
    return f"docmarkup.DocAttrKey.14 {key}", DocAttrKey

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "DocmarkupDocAttrKey":
    raise Exception("this function can only be called from @angle_query")


