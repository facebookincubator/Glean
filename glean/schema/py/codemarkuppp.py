# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.codepp import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.cxx1 import *
from glean.schema.py.src import *


from glean.schema.codemarkup_pp.types import (
    PpEntityInfo,
    PPEntityLocation,
    PpResolveLocation,
    PpIncludeXRefLocations,
    PpEntityKind,
    PpFileEntityXRefLocations,
    PpEntityTraceXRefLocations,
    PpResolveTraceLocation,
    PpFileEntityTraceXRefLocations,
    PpFileEntityTraceLocations,
)


class CodemarkupPpPpEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')]))
    return f"codemarkup.pp.PpEntityInfo.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional["CodePpEntity"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupPpPpEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPPEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.pp.PPEntityLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PPEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodePpEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupPpPPEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.pp.PpResolveLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodePpEntity"] = None) -> "CodemarkupPpPpResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpIncludeXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, range: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, range, 'range'), angle_for(__env, target, 'target')]))
    return f"codemarkup.pp.PpIncludeXRefLocations.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpIncludeXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1PPTrace"] = None, range: Optional["SrcRange"] = None, target: Optional["SrcFile"] = None) -> "CodemarkupPpPpIncludeXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')]))
    return f"codemarkup.pp.PpEntityKind.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpEntityKind

  @staticmethod
  def angle_query(*, entity: Optional["CodePpEntity"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupPpPpEntityKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.pp.PpFileEntityXRefLocations.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodePpEntity"] = None) -> "CodemarkupPpPpFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.pp.PpEntityTraceXRefLocations.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1Trace"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodePpEntity"] = None) -> "CodemarkupPpPpEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.pp.PpResolveTraceLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1Trace"] = None, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodePpEntity"] = None) -> "CodemarkupPpPpResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpFileEntityTraceXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.pp.PpFileEntityTraceXRefLocations.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpFileEntityTraceXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodePpEntity"] = None) -> "CodemarkupPpPpFileEntityTraceXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupPpPpFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.pp.PpFileEntityTraceLocations.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PpFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodePpEntity"] = None) -> "CodemarkupPpPpFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")






