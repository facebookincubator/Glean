# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.src import *


from glean.schema.codemarkuperlang.types import (
    erlangErlangEntityInfo,
    erlangErlangEntityLocation,
    erlangErlangResolveLocation,
    erlangErlangFileEntityXRefLocations,
    erlangErlangEntityUses,
    erlangErlangEntityKind,
)


class CodemarkupErlangErlangEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangEntityInfo.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')])) or '_' } }}", erlangErlangEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional["CodeErlangEntity"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupErlangErlangEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupErlangErlangEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangEntityLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", erlangErlangEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeErlangEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupErlangErlangEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupErlangErlangResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangResolveLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", erlangErlangResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeErlangEntity"] = None) -> "CodemarkupErlangErlangResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupErlangErlangFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangFileEntityXRefLocations.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", erlangErlangFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeErlangEntity"] = None) -> "CodemarkupErlangErlangFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupErlangErlangEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangEntityUses.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", erlangErlangEntityUses

  @staticmethod
  def angle_query(*, target: Optional["CodeErlangEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupErlangErlangEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupErlangErlangEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.erlang.ErlangEntityKind.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')])) or '_' } }}", erlangErlangEntityKind

  @staticmethod
  def angle_query(*, entity: Optional["CodeErlangEntity"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupErlangErlangEntityKind":
    raise Exception("this function can only be called from @angle_query")






