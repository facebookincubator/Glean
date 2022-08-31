# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.codemarkupflow.types import (
    flowFlowEntityLocation,
    flowFlowResolveLocation,
    flowFlowFileReferenceEntityXRefLocations,
    flowFlowFileImportDeclEntityXRefLocations,
    flowFlowFileEntityXRefLocations,
    flowFlowEntityUses,
)


class CodemarkupFlowFlowEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowEntityLocation.2 {{ entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", flowFlowEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowResolveLocation.2 {{ location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", flowFlowResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileReferenceEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", flowFlowFileReferenceEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileReferenceEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileImportDeclEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", flowFlowFileImportDeclEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileImportDeclEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowFileEntityXRefLocations.2 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", flowFlowFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFlowFlowEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.flow.FlowEntityUses.2 {{ target = {angle_for(__env, target)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", flowFlowEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupFlowFlowEntityUses":
    raise Exception("this function can only be called from @angle_query")


