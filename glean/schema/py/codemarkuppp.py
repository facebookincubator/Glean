# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.cxx1 import *
from glean.schema.py.src import *


from glean.schema.codemarkuppp.types import (
    ppPpEntityInfo,
    ppPPEntityLocation,
    ppPpResolveLocation,
    ppPpIncludeXRefLocations,
    ppPpEntityKind,
    ppPpFileEntityXRefLocations,
    ppPpEntityTraceXRefLocations,
    ppPpResolveTraceLocation,
    ppPpFileEntityTraceXRefLocations,
    ppPpFileEntityTraceLocations,
)


class CodemarkupPpPpEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityInfo.3 {{ entity = {angle_for(__env, entity)}, info = {angle_for(__env, info)} }}", ppPpEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupPpPpEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PPEntityLocation.3 {{ entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", ppPPEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupPpPPEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveLocation.3 {{ location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", ppPpResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpIncludeXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, range: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpIncludeXRefLocations.3 {{ trace = {angle_for(__env, trace)}, range = {angle_for(__env, range)}, target = {angle_for(__env, target)} }}", ppPpIncludeXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1PPTrace"] = None, range: Optional[Tuple[()]] = None, target: Optional["SrcFile"] = None) -> "CodemarkupPpPpIncludeXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityKind.3 {{ entity = {angle_for(__env, entity)}, kind = {angle_for(__env, kind)} }}", ppPpEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupPpPpEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityXRefLocations.3 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", ppPpFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpEntityTraceXRefLocations.3 {{ trace = {angle_for(__env, trace)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", ppPpEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1Trace"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpResolveTraceLocation.3 {{ trace = {angle_for(__env, trace)}, location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", ppPpResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1Trace"] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceXRefLocations.3 {{ file = {angle_for(__env, file)}, trace = {angle_for(__env, trace)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", ppPpFileEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpFileEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupPpPpFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.pp.PpFileEntityTraceLocations.3 {{ file = {angle_for(__env, file)}, trace = {angle_for(__env, trace)}, location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", ppPpFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupPpPpFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")


