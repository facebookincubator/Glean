# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codemarkuptypes.types import (
    typesXRefLocation,
    typesVisibility,
    typesSymbolKind,
    typesSymbolInfo,
    typesRangeSpan,
    typesLocation,
)




class CodemarkupTypesXRefLocation(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.types.XRefLocation.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, source, 'source')])) or '_' } }}", typesXRefLocation

  @staticmethod
  def angle_query(*, target: Optional["CodemarkupTypesLocation"] = None, source: Optional["CodemarkupTypesRangeSpan"] = None) -> "CodemarkupTypesXRefLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupTypesVisibility(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.types.Visibility.1 { angle_for(__env, arg, None) or '_' }", typesVisibility

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "CodemarkupTypesVisibility":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupTypesSymbolKind(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.types.SymbolKind.1 { angle_for(__env, arg, None) or '_' }", typesSymbolKind

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "CodemarkupTypesSymbolKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupTypesSymbolInfo(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr, isAbstract: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.types.SymbolInfo.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind'), angle_for(__env, isAbstract, 'isAbstract')])) or '_' } }}", typesSymbolInfo

  @staticmethod
  def angle_query(*, kind: Optional["CodemarkupTypesSymbolKind"] = None, isAbstract: Optional[bool] = None) -> "CodemarkupTypesSymbolInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupTypesRangeSpan(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], span: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.types.RangeSpan.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, span, 'span'), angle_for(__env, range, 'range')])) or '_' } }}", typesRangeSpan

  @staticmethod
  def angle_query_span(*, span: "SrcByteSpan") -> "CodemarkupTypesRangeSpan":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_range(*, range: "SrcRange") -> "CodemarkupTypesRangeSpan":
    raise Exception("this function can only be called from @angle_query")




class CodemarkupTypesLocation(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, file: ast.Expr, location: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.types.Location.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, location, 'location'), angle_for(__env, span, 'span')])) or '_' } }}", typesLocation

  @staticmethod
  def angle_query(*, name: Optional[str] = None, file: Optional["SrcFile"] = None, location: Optional["CodemarkupTypesRangeSpan"] = None, span: Optional[Union[Just["SrcByteSpan"], Just[None]]] = None) -> "CodemarkupTypesLocation":
    raise Exception("this function can only be called from @angle_query")




