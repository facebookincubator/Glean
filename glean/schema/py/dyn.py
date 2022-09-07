# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.src import *


from glean.schema.dyn.types import (
    ObserverIdentifier,
    EntityDynamicReference,
    EntityUsage,
    Environment,
    Observer,
    Usage,
)


class DynObserverIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"dyn.ObserverIdentifier.6 { angle_for(__env, arg, None) or '_' }", ObserverIdentifier

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "DynObserverIdentifier":
    raise Exception("this function can only be called from @angle_query")



class DynEntityDynamicReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], usage: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"dyn.EntityDynamicReference.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, usage, 'usage'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", EntityDynamicReference

  @staticmethod
  def angle_query(*, usage: Optional["DynEntityUsage"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "DynEntityDynamicReference":
    raise Exception("this function can only be called from @angle_query")



class DynEntityUsage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, observer: ast.Expr, usage: ast.Expr, environment: ast.Expr) -> Tuple[str, Struct]:
    return f"dyn.EntityUsage.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, observer, 'observer'), angle_for(__env, usage, 'usage'), angle_for(__env, environment, 'environment')])) or '_' } }}", EntityUsage

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, observer: Optional["DynObserver"] = None, usage: Optional["DynUsage"] = None, environment: Optional[Union[Just["DynEnvironment"], Just[None]]] = None) -> "DynEntityUsage":
    raise Exception("this function can only be called from @angle_query")



class DynEnvironment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"dyn.Environment.6 { angle_for(__env, arg, None) or '_' }", Environment

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "DynEnvironment":
    raise Exception("this function can only be called from @angle_query")





class DynObserver(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], symbol: ast.Expr, other: ast.Expr) -> Tuple[str, Struct]:
    return f"dyn.Observer.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, symbol, 'symbol'), angle_for(__env, other, 'other')])) or '_' } }}", Observer

  @staticmethod
  def angle_query_symbol(*, symbol: "DynObserverIdentifier") -> "DynObserver":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_other(*, other: "DynObserverIdentifier") -> "DynObserver":
    raise Exception("this function can only be called from @angle_query")




class DynUsage(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"dyn.Usage.6 { angle_for(__env, arg, None) or '_' }", Usage

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "DynUsage":
    raise Exception("this function can only be called from @angle_query")




