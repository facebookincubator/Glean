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
    return f"codemarkup.cxx.CxxFileEntityXRefLocations.4 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", cxxCxxFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxContainsChildEntity.4 {{ parent = {angle_for(__env, parent)}, child = {angle_for(__env, child)} }}", cxxCxxContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityUses.4 {{ target = {angle_for(__env, target)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", cxxCxxEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityKind.4 {{ entity = {angle_for(__env, entity)}, kind = {angle_for(__env, kind)} }}", cxxCxxEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveLocation.4 {{ location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", cxxCxxResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDefToDeclFamilyXRefTargetLocation.4 {{ decl = {angle_for(__env, decl)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", cxxCxxDefToDeclFamilyXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityLocation.4 {{ entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", cxxCxxEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, visibility: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxVisibility.4 {{ entity = {angle_for(__env, entity)}, visibility = {angle_for(__env, visibility)} }}", cxxCxxVisibility

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxVisibility":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, source: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclLocations.4 {{ trace = {angle_for(__env, trace)}, source = {angle_for(__env, source)}, location = {angle_for(__env, location)} }}", cxxCxxFileEntityXMapVariableXRefDeclLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, source: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclToDefXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclToDefXRefTargetLocation.4 {{ decl = {angle_for(__env, decl)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", cxxCxxDeclToDefXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclToDefXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, anns: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxAnnotation.4 {{ entity = {angle_for(__env, entity)}, anns = {angle_for(__env, anns)} }}", cxxCxxAnnotation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, anns: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxAnnotation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityFixedXRefLocations.4 {{ file = {angle_for(__env, file)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", cxxCxxFileEntityFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveDeclarationToEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveDeclarationToEntity.4 {{ decl = {angle_for(__env, decl)}, entity = {angle_for(__env, entity)} }}", cxxCxxResolveDeclarationToEntity

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveDeclarationToEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxXRefTargetLocation.4 {{ target = {angle_for(__env, target)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", cxxCxxXRefTargetLocation

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceFixedXRefLocations.4 {{ file = {angle_for(__env, file)}, trace = {angle_for(__env, trace)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", cxxCxxFileEntityTraceFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1FileXRefMap"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityInfo.4 {{ entity = {angle_for(__env, entity)}, info = {angle_for(__env, info)} }}", cxxCxxEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceLocations.4 {{ file = {angle_for(__env, file)}, trace = {angle_for(__env, trace)}, location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", cxxCxxFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclInfo.4 {{ decl = {angle_for(__env, decl)}, info = {angle_for(__env, info)} }}", cxxCxxDeclInfo

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveTraceLocation.4 {{ trace = {angle_for(__env, trace)}, location = {angle_for(__env, location)}, entity = {angle_for(__env, entity)} }}", cxxCxxResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1Trace"] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapFixedXRefLocations.4 {{ trace = {angle_for(__env, trace)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", cxxCxxFileEntityXMapFixedXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, trace: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceDeclToDefXRefLocations.4 {{ file = {angle_for(__env, file)}, trace = {angle_for(__env, trace)}, xref = {angle_for(__env, xref)}, entity = {angle_for(__env, entity)} }}", cxxCxxFileEntityTraceDeclToDefXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, trace: Optional["Cxx1Trace"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclKind.4 {{ decl = {angle_for(__env, decl)}, kind = {angle_for(__env, kind)} }}", cxxCxxDeclKind

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], trace: ast.Expr, source: ast.Expr, entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclToDefs.4 {{ trace = {angle_for(__env, trace)}, source = {angle_for(__env, source)}, entity = {angle_for(__env, entity)}, location = {angle_for(__env, location)} }}", cxxCxxFileEntityXMapVariableXRefDeclToDefs

  @staticmethod
  def angle_query(*, trace: Optional["Cxx1FileXRefs"] = None, source: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs":
    raise Exception("this function can only be called from @angle_query")


