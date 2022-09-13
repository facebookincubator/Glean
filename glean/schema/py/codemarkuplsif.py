# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codelsif import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.lsif import *
from glean.schema.py.src import *


from glean.schema.codemarkup_lsif.types import (
    LsifEntityLocation,
    LsifResolveLocation,
    LsifEntityUses,
    LsifFileEntityXRefLocations,
    EntityInfo,
    LsifKindToKind,
)


class CodemarkupLsifLsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifEntityLocation.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", LsifEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeLsifEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupLsifLsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupLsifLsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifResolveLocation.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", LsifResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeLsifEntity"] = None) -> "CodemarkupLsifLsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupLsifLsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifEntityUses.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')])) or '_' } }}", LsifEntityUses

  @staticmethod
  def angle_query(*, target: Optional["CodeLsifEntity"] = None, file: Optional["SrcFile"] = None, range: Optional["SrcRange"] = None) -> "CodemarkupLsifLsifEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupLsifLsifFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifFileEntityXRefLocations.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", LsifFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeLsifEntity"] = None) -> "CodemarkupLsifLsifFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupLsifEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.EntityInfo.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')])) or '_' } }}", EntityInfo

  @staticmethod
  def angle_query(*, entity: Optional["CodeLsifEntity"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupLsifEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupLsifLsifKindToKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lsif: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifKindToKind.3 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, lsif, 'lsif'), angle_for(__env, kind, 'kind')])) or '_' } }}", LsifKindToKind

  @staticmethod
  def angle_query(*, lsif: Optional["LsifSymbolKind"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupLsifLsifKindToKind":
    raise Exception("this function can only be called from @angle_query")






