# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codebuck import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.src import *


from glean.schema.codemarkup_buck.types import (
    BuckEntityLocation,
    BuckResolveLocation,
    BuckFileEntityXRefLocations,
    BuckEntityUses,
)


class CodemarkupBuckBuckEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.buck.BuckEntityLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", BuckEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeBuckEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupBuckBuckEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupBuckBuckResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.buck.BuckResolveLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", BuckResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeBuckEntity"] = None) -> "CodemarkupBuckBuckResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupBuckBuckFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.buck.BuckFileEntityXRefLocations.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", BuckFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeBuckEntity"] = None) -> "CodemarkupBuckBuckFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupBuckBuckEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.buck.BuckEntityUses.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", BuckEntityUses

  @staticmethod
  def angle_query(*, target: Optional["CodeBuckEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupBuckBuckEntityUses":
    raise Exception("this function can only be called from @angle_query")






