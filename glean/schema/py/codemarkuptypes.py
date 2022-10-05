# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.src import *


from glean.schema.codemarkup_types.types import (
    XRefLocation,
    Visibility,
    SymbolKind,
    SymbolInfo,
    RangeSpan,
    Location,
)




class CodemarkupTypesXRefLocation(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')]))
    return f"codemarkup.types.XRefLocation.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefLocation

  @staticmethod
  def angle_query(*, target: Optional["CodemarkupTypesLocation"] = None, source: Optional["CodemarkupTypesRangeSpan"] = None) -> "CodemarkupTypesXRefLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupTypesVisibility(Enum):
  Public = 0
  Protected = 1
  Private = 2

class CodemarkupTypesSymbolKind(Enum):
  Package = 0
  Type = 1
  Value = 2
  File = 3
  Module = 4
  Namespace = 5
  Class_ = 6
  Method = 7
  Property = 8
  Field = 9
  Constructor = 10
  Enum_ = 11
  Interface = 12
  Function = 13
  Variable = 14
  Constant = 15
  String = 16
  Number = 17
  Boolean = 18
  Array = 19
  Object_ = 20
  Key = 21
  Null = 22
  Enumerator = 23
  Struct = 24
  Event = 25
  Operator = 26
  TypeParameter = 27
  Union = 28
  Macro = 29
  Trait = 30

class CodemarkupTypesSymbolInfo(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr, isAbstract: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind'), angle_for(__env, isAbstract, 'isAbstract')]))
    return f"codemarkup.types.SymbolInfo.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SymbolInfo

  @staticmethod
  def angle_query(*, kind: Optional["CodemarkupTypesSymbolKind"] = None, isAbstract: Optional[bool] = None) -> "CodemarkupTypesSymbolInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupTypesRangeSpan(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], span: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, span, 'span'), angle_for(__env, range, 'range')]))
    return f"codemarkup.types.RangeSpan.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RangeSpan

  @staticmethod
  def angle_query_span(*, span: Optional["SrcByteSpan"] = None) -> "CodemarkupTypesRangeSpan":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_range(*, range: Optional["SrcRange"] = None) -> "CodemarkupTypesRangeSpan":
    raise Exception("this function can only be called from @angle_query")




class CodemarkupTypesLocation(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, file: ast.Expr, location: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, location, 'location'), angle_for(__env, span, 'span')]))
    return f"codemarkup.types.Location.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Location

  @staticmethod
  def angle_query(*, name: Optional[str] = None, file: Optional["SrcFile"] = None, location: Optional["CodemarkupTypesRangeSpan"] = None, span: Optional[Union[Just["SrcByteSpan"], Just[None]]] = None) -> "CodemarkupTypesLocation":
    raise Exception("this function can only be called from @angle_query")




