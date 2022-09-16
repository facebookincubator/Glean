# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.codeflow import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.flow import *
from glean.schema.py.src import *


from glean.schema.codemarkup_flow.types import (
    FlowEntityLocation,
    FlowResolveLocation,
    FlowFileReferenceEntityXRefLocations,
    FlowEntityDocumentation,
    FlowFileImportDeclEntityXRefLocations,
    FlowFileEntityXRefLocations,
    FlowEntityUses,
    FlowDocumentationSpan,
    FlowDeclarationDocumentation,
)


class CodemarkupFlowFlowEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.flow.FlowEntityLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeFlowEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupFlowFlowEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.flow.FlowResolveLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeFlowEntity"] = None) -> "CodemarkupFlowFlowResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowFileReferenceEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.flow.FlowFileReferenceEntityXRefLocations.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowFileReferenceEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeFlowEntity"] = None) -> "CodemarkupFlowFlowFileReferenceEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowEntityDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.flow.FlowEntityDocumentation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowEntityDocumentation

  @staticmethod
  def angle_query(*, entity: Optional["CodeFlowEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupFlowFlowEntityDocumentation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowFileImportDeclEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.flow.FlowFileImportDeclEntityXRefLocations.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowFileImportDeclEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeFlowEntity"] = None) -> "CodemarkupFlowFlowFileImportDeclEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.flow.FlowFileEntityXRefLocations.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeFlowEntity"] = None) -> "CodemarkupFlowFlowFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.flow.FlowEntityUses.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowEntityUses

  @staticmethod
  def angle_query(*, target: Optional["CodeFlowEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupFlowFlowEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowDocumentationSpan(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], doc: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, doc, 'doc'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.flow.FlowDocumentationSpan.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowDocumentationSpan

  @staticmethod
  def angle_query(*, doc: Optional["FlowDocumentation"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupFlowFlowDocumentationSpan":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFlowFlowDeclarationDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.flow.FlowDeclarationDocumentation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FlowDeclarationDocumentation

  @staticmethod
  def angle_query(*, decl: Optional["FlowSomeDeclaration"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupFlowFlowDeclarationDocumentation":
    raise Exception("this function can only be called from @angle_query")






