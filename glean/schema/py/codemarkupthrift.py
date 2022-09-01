# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.codemarkupthrift.types import (
    thriftThriftFileEntityXRefLocations,
    thriftThriftResolveLocation,
    thriftThriftEntityLocation,
)


class CodemarkupThriftThriftFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.thrift.ThriftFileEntityXRefLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", thriftThriftFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupThriftThriftFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupThriftThriftResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.thrift.ThriftResolveLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", thriftThriftResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupThriftThriftResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupThriftThriftEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.thrift.ThriftEntityLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", thriftThriftEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupThriftThriftEntityLocation":
    raise Exception("this function can only be called from @angle_query")




