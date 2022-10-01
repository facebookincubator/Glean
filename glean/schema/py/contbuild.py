# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.buck import *


from glean.schema.contbuild.types import (
    ContbuildName,
    ContbuildLocator,
    ContbuildFbpkg,
)


class ContbuildContbuildName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"contbuild.ContbuildName.1 { query_fields if query_fields else '_' }", ContbuildName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "ContbuildContbuildName":
    raise Exception("this function can only be called from @angle_query")



class ContbuildContbuildLocator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, name, 'name')]))
    return f"contbuild.ContbuildLocator.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContbuildLocator

  @staticmethod
  def angle_query(*, target: Optional["BuckLocator"] = None, name: Optional["ContbuildContbuildName"] = None) -> "ContbuildContbuildLocator":
    raise Exception("this function can only be called from @angle_query")



class ContbuildContbuildFbpkg(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fbpkg_name: ast.Expr, contbuild_name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, fbpkg_name, 'fbpkg_name'), angle_for(__env, contbuild_name, 'contbuild_name')]))
    return f"contbuild.ContbuildFbpkg.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContbuildFbpkg

  @staticmethod
  def angle_query(*, fbpkg_name: Optional[str] = None, contbuild_name: Optional["ContbuildContbuildName"] = None) -> "ContbuildContbuildFbpkg":
    raise Exception("this function can only be called from @angle_query")






