# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXRefLocations.4 { { } }", CxxCxxFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxContainsChildEntity.4 { { } }", CxxCxxContainsChildEntity

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityUses.4 { { } }", CxxCxxEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityKind.4 { { } }", CxxCxxEntityKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveLocation.4 { { } }", CxxCxxResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDefToDeclFamilyXRefTargetLocation.4 { { } }", CxxCxxDefToDeclFamilyXRefTargetLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxDefToDeclFamilyXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityLocation.4 { { } }", CxxCxxEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxVisibility.4 { { } }", CxxCxxVisibility

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxVisibility":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclLocations.4 { { } }", CxxCxxFileEntityXMapVariableXRefDeclLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclToDefXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclToDefXRefTargetLocation.4 { { } }", CxxCxxDeclToDefXRefTargetLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxDeclToDefXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxAnnotation.4 { { } }", CxxCxxAnnotation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxAnnotation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityFixedXRefLocations.4 { { } }", CxxCxxFileEntityFixedXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveDeclarationToEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveDeclarationToEntity.4 { { } }", CxxCxxResolveDeclarationToEntity

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxResolveDeclarationToEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxXRefTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxXRefTargetLocation.4 { { } }", CxxCxxXRefTargetLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxXRefTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceFixedXRefLocations.4 { { } }", CxxCxxFileEntityTraceFixedXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityTraceFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxEntityInfo.4 { { } }", CxxCxxEntityInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceLocations.4 { { } }", CxxCxxFileEntityTraceLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityTraceLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclInfo.4 { { } }", CxxCxxDeclInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxDeclInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxResolveTraceLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxResolveTraceLocation.4 { { } }", CxxCxxResolveTraceLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxResolveTraceLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapFixedXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapFixedXRefLocations.4 { { } }", CxxCxxFileEntityXMapFixedXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityXMapFixedXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityTraceDeclToDefXRefLocations.4 { { } }", CxxCxxFileEntityTraceDeclToDefXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityTraceDeclToDefXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxDeclKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxDeclKind.4 { { } }", CxxCxxDeclKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxDeclKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.cxx.CxxFileEntityXMapVariableXRefDeclToDefs.4 { { } }", CxxCxxFileEntityXMapVariableXRefDeclToDefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupCxxCxxFileEntityXMapVariableXRefDeclToDefs":
    raise Exception("this function can only be called from @angle_query")


