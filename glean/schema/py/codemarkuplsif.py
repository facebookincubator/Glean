# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkuplsif.types import (
    LsifLsifEntityLocation,
    LsifLsifResolveLocation,
    LsifLsifEntityUses,
    LsifLsifFileEntityXRefLocations,
    LsifEntityInfo,
    LsifLsifKindToKind,
)


class CodemarkupLsifLsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifEntityLocation.3 { { } }", LsifLsifEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupLsifLsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifResolveLocation.3 { { } }", LsifLsifResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupLsifLsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifEntityUses.3 { { } }", LsifLsifEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupLsifLsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifFileEntityXRefLocations.3 { { } }", LsifLsifFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupLsifLsifFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.EntityInfo.3 { { } }", LsifEntityInfo

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupLsifEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifKindToKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifKindToKind.3 { { } }", LsifLsifKindToKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupLsifLsifKindToKind":
    raise Exception("this function can only be called from @angle_query")


