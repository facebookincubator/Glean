# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.coderust import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.src import *


from glean.schema.codemarkup_rust.types import (
    RustEntityLocation,
    RustResolveLocation,
    RustFileEntityXRefLocations,
    RustEntityUses,
)


class CodemarkupRustRustEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.rust.RustEntityLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RustEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeRustEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupRustRustEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupRustRustResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.rust.RustResolveLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RustResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeRustEntity"] = None) -> "CodemarkupRustRustResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupRustRustFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.rust.RustFileEntityXRefLocations.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RustFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeRustEntity"] = None) -> "CodemarkupRustRustFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupRustRustEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.rust.RustEntityUses.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RustEntityUses

  @staticmethod
  def angle_query(*, target: Optional["CodeRustEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupRustRustEntityUses":
    raise Exception("this function can only be called from @angle_query")






