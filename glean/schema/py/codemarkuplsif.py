# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.codemarkuplsif.types import (
    lsifLsifEntityLocation,
    lsifLsifResolveLocation,
    lsifLsifEntityUses,
    lsifLsifFileEntityXRefLocations,
    lsifEntityInfo,
    lsifLsifKindToKind,
)


class CodemarkupLsifLsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifEntityLocation.3 {{ entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", lsifLsifEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifResolveLocation.3 {{ location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", lsifLsifResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifEntityUses.3 {{ target = {angle_for(__env, target)}, file = {angle_for(__env, file)}, range = {angle_for(__env, range)} }}", lsifLsifEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, range: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifFileEntityXRefLocations.3 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", lsifLsifFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.EntityInfo.3 {{ entity = {angle_for(__env, entity)}, info = {angle_for(__env, info)} }}", lsifEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupLsifEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifKindToKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lsif: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifKindToKind.3 {{ lsif = {angle_for(__env, lsif)}, kind = {angle_for(__env, kind)} }}", lsifLsifKindToKind

  @staticmethod
  def angle_query(*, lsif: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifKindToKind":
    raise Exception("this function can only be called from @angle_query")


