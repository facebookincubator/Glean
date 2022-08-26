# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.codemarkupcxx.types import (
    CxxCxxFileEntityXRefLocations,
    CxxCxxContainsChildEntity,
    CxxCxxEntityUses,
    CxxCxxEntityKind,
    CxxCxxResolveLocation,
    CxxCxxDefToDeclFamilyXRefTargetLocation,
    CxxCxxEntityLocation,
    CxxCxxVisibility,
    CxxCxxFileEntityXMapVariableXRefDeclLocations,
    CxxCxxDeclToDefXRefTargetLocation,
    CxxCxxAnnotation,
    CxxCxxFileEntityFixedXRefLocations,
    CxxCxxResolveDeclarationToEntity,
    CxxCxxXRefTargetLocation,
    CxxCxxFileEntityTraceFixedXRefLocations,
    CxxCxxEntityInfo,
    CxxCxxFileEntityTraceLocations,
    CxxCxxDeclInfo,
    CxxCxxResolveTraceLocation,
    CxxCxxFileEntityXMapFixedXRefLocations,
    CxxCxxFileEntityTraceDeclToDefXRefLocations,
    CxxCxxDeclKind,
    CxxCxxFileEntityXMapVariableXRefDeclToDefs,
)


class CodemarkupCxxCxxFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityXRefLocations.4 {{ }}", CxxCxxFileEntityXRefLocations
    return f"codemarkup.cxx.CxxFileEntityXRefLocations.4 { concatenateFields(key) }", CxxCxxFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxContainsChildEntity.4 {{ }}", CxxCxxContainsChildEntity
    return f"codemarkup.cxx.CxxContainsChildEntity.4 { concatenateFields(key) }", CxxCxxContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxEntityUses.4 {{ }}", CxxCxxEntityUses
    return f"codemarkup.cxx.CxxEntityUses.4 { concatenateFields(key) }", CxxCxxEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxEntityKind.4 {{ }}", CxxCxxEntityKind
    return f"codemarkup.cxx.CxxEntityKind.4 { concatenateFields(key) }", CxxCxxEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxResolveLocation.4 {{ }}", CxxCxxResolveLocation
    return f"codemarkup.cxx.CxxResolveLocation.4 { concatenateFields(key) }", CxxCxxResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxDefToDeclFamilyXRefTargetLocation.4 {{ }}", CxxCxxDefToDeclFamilyXRefTargetLocation
    return f"codemarkup.cxx.CxxDefToDeclFamilyXRefTargetLocation.4 { concatenateFields(key) }", CxxCxxDefToDeclFamilyXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxEntityLocation.4 {{ }}", CxxCxxEntityLocation
    return f"codemarkup.cxx.CxxEntityLocation.4 { concatenateFields(key) }", CxxCxxEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxVisibility.4 {{ }}", CxxCxxVisibility
    return f"codemarkup.cxx.CxxVisibility.4 { concatenateFields(key) }", CxxCxxVisibility

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxVisibility":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclLocations.4 {{ }}", CxxCxxFileEntityXMapVariableXRefDeclLocations
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclLocations.4 { concatenateFields(key) }", CxxCxxFileEntityXMapVariableXRefDeclLocations

  @staticmethod
  def angle_query(*, trace: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclToDefXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxDeclToDefXRefTargetLocation.4 {{ }}", CxxCxxDeclToDefXRefTargetLocation
    return f"codemarkup.cxx.CxxDeclToDefXRefTargetLocation.4 { concatenateFields(key) }", CxxCxxDeclToDefXRefTargetLocation

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclToDefXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxAnnotation.4 {{ }}", CxxCxxAnnotation
    return f"codemarkup.cxx.CxxAnnotation.4 { concatenateFields(key) }", CxxCxxAnnotation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, anns: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxAnnotation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityFixedXRefLocations.4 {{ }}", CxxCxxFileEntityFixedXRefLocations
    return f"codemarkup.cxx.CxxFileEntityFixedXRefLocations.4 { concatenateFields(key) }", CxxCxxFileEntityFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveDeclarationToEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxResolveDeclarationToEntity.4 {{ }}", CxxCxxResolveDeclarationToEntity
    return f"codemarkup.cxx.CxxResolveDeclarationToEntity.4 { concatenateFields(key) }", CxxCxxResolveDeclarationToEntity

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveDeclarationToEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxXRefTargetLocation.4 {{ }}", CxxCxxXRefTargetLocation
    return f"codemarkup.cxx.CxxXRefTargetLocation.4 { concatenateFields(key) }", CxxCxxXRefTargetLocation

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityTraceFixedXRefLocations.4 {{ }}", CxxCxxFileEntityTraceFixedXRefLocations
    return f"codemarkup.cxx.CxxFileEntityTraceFixedXRefLocations.4 { concatenateFields(key) }", CxxCxxFileEntityTraceFixedXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxEntityInfo.4 {{ }}", CxxCxxEntityInfo
    return f"codemarkup.cxx.CxxEntityInfo.4 { concatenateFields(key) }", CxxCxxEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityTraceLocations.4 {{ }}", CxxCxxFileEntityTraceLocations
    return f"codemarkup.cxx.CxxFileEntityTraceLocations.4 { concatenateFields(key) }", CxxCxxFileEntityTraceLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxDeclInfo.4 {{ }}", CxxCxxDeclInfo
    return f"codemarkup.cxx.CxxDeclInfo.4 { concatenateFields(key) }", CxxCxxDeclInfo

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxResolveTraceLocation.4 {{ }}", CxxCxxResolveTraceLocation
    return f"codemarkup.cxx.CxxResolveTraceLocation.4 { concatenateFields(key) }", CxxCxxResolveTraceLocation

  @staticmethod
  def angle_query(*, trace: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityXMapFixedXRefLocations.4 {{ }}", CxxCxxFileEntityXMapFixedXRefLocations
    return f"codemarkup.cxx.CxxFileEntityXMapFixedXRefLocations.4 { concatenateFields(key) }", CxxCxxFileEntityXMapFixedXRefLocations

  @staticmethod
  def angle_query(*, trace: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityTraceDeclToDefXRefLocations.4 {{ }}", CxxCxxFileEntityTraceDeclToDefXRefLocations
    return f"codemarkup.cxx.CxxFileEntityTraceDeclToDefXRefLocations.4 { concatenateFields(key) }", CxxCxxFileEntityTraceDeclToDefXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, trace: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxDeclKind.4 {{ }}", CxxCxxDeclKind
    return f"codemarkup.cxx.CxxDeclKind.4 { concatenateFields(key) }", CxxCxxDeclKind

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxDeclKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclToDefs.4 {{ }}", CxxCxxFileEntityXMapVariableXRefDeclToDefs
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclToDefs.4 { concatenateFields(key) }", CxxCxxFileEntityXMapVariableXRefDeclToDefs

  @staticmethod
  def angle_query(*, trace: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs":
    raise Exception("this function can only be called from @angle_query")


