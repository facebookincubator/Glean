# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codethrift import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.src import *


from glean.schema.codemarkup_thrift.types import (
    ThriftFileEntityXRefLocations,
    ThriftResolveLocation,
    ThriftEntityLocation,
)


class CodemarkupThriftThriftFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ThriftFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeThriftEntity"] = None) -> "CodemarkupThriftThriftFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupThriftThriftResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.thrift.ThriftResolveLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ThriftResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeThriftEntity"] = None) -> "CodemarkupThriftThriftResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupThriftThriftEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.thrift.ThriftEntityLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ThriftEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeThriftEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupThriftThriftEntityLocation":
    raise Exception("this function can only be called from @angle_query")






