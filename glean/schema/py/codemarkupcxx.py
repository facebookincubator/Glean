# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.cxx1 import *
from glean.schema.py.src import *


from glean.schema.codemarkupcxx.types import (
    cxxCxxFileEntityXRefLocations,
    cxxCxxContainsChildEntity,
    cxxCxxEntityUses,
    cxxCxxEntityKind,
    cxxCxxResolveLocation,
    cxxCxxDefToDeclFamilyXRefTargetLocation,
    cxxCxxEntityLocation,
    cxxCxxVisibility,
    cxxCxxFileEntityXMapVariableXRefDeclLocations,
    cxxCxxDeclToDefXRefTargetLocation,
    cxxCxxAnnotation,
    cxxCxxFileEntityFixedXRefLocations,
    cxxCxxResolveDeclarationToEntity,
    cxxCxxXRefTargetLocation,
    cxxCxxFileEntityTraceFixedXRefLocations,
    cxxCxxEntityInfo,
    cxxCxxFileEntityTraceLocations,
    cxxCxxDeclInfo,
    cxxCxxResolveTraceLocation,
    cxxCxxFileEntityXMapFixedXRefLocations,
    cxxCxxFileEntityTraceDeclToDefXRefLocations,
    cxxCxxDeclKind,
    cxxCxxFileEntityXMapVariableXRefDeclToDefs,
)


class CodemarkupCxxCxxFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXRefLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxContainsChildEntity.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')])) or '_' } }}", cxxCxxContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityUses.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", cxxCxxEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityKind.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')])) or '_' } }}", cxxCxxEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDefToDeclFamilyXRefTargetLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", cxxCxxDefToDeclFamilyXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", cxxCxxEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, visibility: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxVisibility.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, visibility, 'visibility')])) or '_' } }}", cxxCxxVisibility

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxVisibility":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, source: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, source, 'source'), angle_for(__env, location, 'location')])) or '_' } }}", cxxCxxFileEntityXMapVariableXRefDeclLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, source: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDeclToDefXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclToDefXRefTargetLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", cxxCxxDeclToDefXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclToDefXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, anns: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxAnnotation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, anns, 'anns')])) or '_' } }}", cxxCxxAnnotation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, anns: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxAnnotation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityFixedXRefLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxFileEntityFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxResolveDeclarationToEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveDeclarationToEntity.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxResolveDeclarationToEntity

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveDeclarationToEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxXRefTargetLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", cxxCxxXRefTargetLocation

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityTraceFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceFixedXRefLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxFileEntityTraceFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1FileXRefMap"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityInfo.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')])) or '_' } }}", cxxCxxEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclInfo.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, info, 'info')])) or '_' } }}", cxxCxxDeclInfo

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveTraceLocation.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1Trace"] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityXMapFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapFixedXRefLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxFileEntityXMapFixedXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceDeclToDefXRefLocations.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, trace, 'trace'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", cxxCxxFileEntityTraceDeclToDefXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxDeclKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclKind.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, kind, 'kind')])) or '_' } }}", cxxCxxDeclKind

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, source: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclToDefs.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, trace, 'trace'), angle_for(__env, source, 'source'), angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", cxxCxxFileEntityXMapVariableXRefDeclToDefs

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, source: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs":
    raise Exception("this function can only be called from @angle_query")




