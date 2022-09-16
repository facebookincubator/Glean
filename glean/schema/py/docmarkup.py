# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.code import *
from glean.schema.py.src import *


from glean.schema.docmarkup.types import (
    DocAttr,
    EntityComments,
    EntityDocAttr,
    EntityByDocAttrKey,
    EntityAnnotations,
    DocAttrKey,
    GeneralAnnotations,
)


class DocmarkupDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, key, 'key'), angle_for(__env, value, 'value')]))
    return f"docmarkup.DocAttr.14 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DocAttr

  @staticmethod
  def angle_query(*, key: Optional["DocmarkupDocAttrKey"] = None, value: Optional["DocmarkupDocAttrValue"] = None) -> "DocmarkupDocAttr":
    raise Exception("this function can only be called from @angle_query")



class DocmarkupEntityComments(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"docmarkup.EntityComments.14 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityComments

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "DocmarkupEntityComments":
    raise Exception("this function can only be called from @angle_query")



class DocmarkupEntityDocAttr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"docmarkup.EntityDocAttr.14 { query_fields if query_fields else '_' }", EntityDocAttr

  @staticmethod
  def angle_query(*, arg: Optional["CodeEntity"] = None) -> "DocmarkupEntityDocAttr":
    raise Exception("this function can only be called from @angle_query")



class DocmarkupEntityByDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, key, 'key'), angle_for(__env, entity, 'entity')]))
    return f"docmarkup.EntityByDocAttrKey.14 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityByDocAttrKey

  @staticmethod
  def angle_query(*, key: Optional["DocmarkupDocAttrKey"] = None, entity: Optional["CodeEntity"] = None) -> "DocmarkupEntityByDocAttrKey":
    raise Exception("this function can only be called from @angle_query")



class DocmarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, annotations: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, annotations, 'annotations')]))
    return f"docmarkup.EntityAnnotations.14 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityAnnotations

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, annotations: Optional["DocmarkupGeneralAnnotations"] = None) -> "DocmarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")



class DocmarkupDocAttrKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"docmarkup.DocAttrKey.14 { query_fields if query_fields else '_' }", DocAttrKey

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "DocmarkupDocAttrKey":
    raise Exception("this function can only be called from @angle_query")





class DocmarkupGeneralAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], doc: ast.Expr, hack: ast.Expr, java: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, doc, 'doc'), angle_for(__env, hack, 'hack'), angle_for(__env, java, 'java')]))
    return f"docmarkup.GeneralAnnotations.14 { ('{ ' + query_fields + ' }') if query_fields else '_' }", GeneralAnnotations

  @staticmethod
  def angle_query_doc(*, doc: Optional["DocmarkupDocAttrs"] = None) -> "DocmarkupGeneralAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hack(*, hack: Optional[List["HackUserAttribute"]] = None) -> "DocmarkupGeneralAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_java(*, java: Optional[List["JavaAnnotation"]] = None) -> "DocmarkupGeneralAnnotations":
    raise Exception("this function can only be called from @angle_query")






DocmarkupDocAttrs = List["DocmarkupDocAttr"]

DocmarkupDocAttrValue = str
