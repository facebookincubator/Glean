# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codecxx import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.cxx1 import *
from glean.schema.py.src import *


from glean.schema.codemarkup_cxx.types import (
    CxxFileEntityXRefLocations,
    CxxContainsChildEntity,
    CxxEntityUses,
    CxxEntityKind,
    CxxDeclarationContainsChild,
    CxxResolveLocation,
    CxxDefToDeclFamilyXRefTargetLocation,
    CxxEntityDocumentation,
    CxxEntityLocation,
    CxxVisibility,
    CxxFileEntityXMapVariableXRefDeclLocations,
    CxxDeclToDefXRefTargetLocation,
    CxxAnnotation,
    CxxDefinitionContainsChild,
    CxxFileEntityFixedXRefLocations,
    CxxResolveDeclarationToEntity,
    CxxXRefTargetLocation,
    CxxFileEntityTraceFixedXRefLocations,
    CxxEntityInfo,
    CxxFileEntityTraceLocations,
    CxxNamespaceDeclarationContainsChild,
    CxxDeclInfo,
    CxxResolveTraceLocation,
    CxxFileEntityXMapFixedXRefLocations,
    CxxFileEntityTraceDeclToDefXRefLocations,
    CxxDeclKind,
    CxxFileEntityXMapVariableXRefDeclToDefs,
)


class CodemarkupCxxCxxFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxFileEntityXRefLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')]))
    return f"codemarkup.cxx.CxxContainsChildEntity.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional["CodeCxxEntity"] = None, child: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.cxx.CxxEntityUses.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxEntityUses

  @staticmethod
  def angle_query(*, target: Optional["CodeCxxEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupCxxCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')]))
    return f"codemarkup.cxx.CxxEntityKind.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxEntityKind

  @staticmethod
  def angle_query(*, entity: Optional["CodeCxxEntity"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupCxxCxxEntityKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDeclarationContainsChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')]))
    return f"codemarkup.cxx.CxxDeclarationContainsChild.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxDeclarationContainsChild

  @staticmethod
  def angle_query(*, parent: Optional["Cxx1Declaration"] = None, child: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxDeclarationContainsChild":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxResolveLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.cxx.CxxDefToDeclFamilyXRefTargetLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxDefToDeclFamilyXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, entity: Optional["CodeCxxEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityDocumentation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.cxx.CxxEntityDocumentation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxEntityDocumentation

  @staticmethod
  def angle_query(*, entity: Optional["CodeCxxEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupCxxCxxEntityDocumentation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.cxx.CxxEntityLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeCxxEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupCxxCxxEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, visibility: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, visibility, 'visibility')]))
    return f"codemarkup.cxx.CxxVisibility.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxVisibility

  @staticmethod
  def angle_query(*, entity: Optional["CodeCxxEntity"] = None, visibility: Optional["CodemarkupTypesVisibility"] = None) -> "CodemarkupCxxCxxVisibility":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, source: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, source, 'source'), angle_for(__env, location, 'location')]))
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityXMapVariableXRefDeclLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, source: Optional["Cxx1Declaration"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDeclToDefXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.cxx.CxxDeclToDefXRefTargetLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxDeclToDefXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, entity: Optional["CodeCxxEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupCxxCxxDeclToDefXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, anns: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, anns, 'anns')]))
    return f"codemarkup.cxx.CxxAnnotation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxAnnotation

  @staticmethod
  def angle_query(*, entity: Optional["CodeCxxEntity"] = None, anns: Optional["CodeCxxAnnotations"] = None) -> "CodemarkupCxxCxxAnnotation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDefinitionContainsChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')]))
    return f"codemarkup.cxx.CxxDefinitionContainsChild.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxDefinitionContainsChild

  @staticmethod
  def angle_query(*, parent: Optional["CodeCxxDefinition"] = None, child: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxDefinitionContainsChild":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxFileEntityFixedXRefLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxFileEntityFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxResolveDeclarationToEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxResolveDeclarationToEntity.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxResolveDeclarationToEntity

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxResolveDeclarationToEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.cxx.CxxXRefTargetLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxXRefTargetLocation

  @staticmethod
  def angle_query(*, target: Optional["Cxx1XRefTarget"] = None, entity: Optional["CodeCxxEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupCxxCxxXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityTraceFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxFileEntityTraceFixedXRefLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityTraceFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1FileXRefMap"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxFileEntityTraceFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')]))
    return f"codemarkup.cxx.CxxEntityInfo.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional["CodeCxxEntity"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupCxxCxxEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxFileEntityTraceLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxNamespaceDeclarationContainsChild(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')]))
    return f"codemarkup.cxx.CxxNamespaceDeclarationContainsChild.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxNamespaceDeclarationContainsChild

  @staticmethod
  def angle_query(*, parent: Optional["Cxx1NamespaceDeclaration"] = None, child: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxNamespaceDeclarationContainsChild":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, info, 'info')]))
    return f"codemarkup.cxx.CxxDeclInfo.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxDeclInfo

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupCxxCxxDeclInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxResolveTraceLocation.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1Trace"] = None, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityXMapFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxFileEntityXMapFixedXRefLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityXMapFixedXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxFileEntityXMapFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.cxx.CxxFileEntityTraceDeclToDefXRefLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityTraceDeclToDefXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeCxxEntity"] = None) -> "CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDeclKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, kind, 'kind')]))
    return f"codemarkup.cxx.CxxDeclKind.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxDeclKind

  @staticmethod
  def angle_query(*, decl: Optional["Cxx1Declaration"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupCxxCxxDeclKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, source: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, source, 'source'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclToDefs.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", CxxFileEntityXMapVariableXRefDeclToDefs

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, source: Optional["Cxx1Declaration"] = None, entity: Optional["CodeCxxEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs":
    raise Exception("this function can only be called from @angle_query")






