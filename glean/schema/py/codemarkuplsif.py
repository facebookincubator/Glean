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
    return f"codemarkup.lsif.LsifEntityLocation.3 {{ entity = _, location = _ }}", LsifLsifEntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "CodemarkupLsifLsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifResolveLocation.3 {{ location = _, entity = _ }}", LsifLsifResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "CodemarkupLsifLsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifEntityUses.3 {{ target = _, file = _, range = _ }}", LsifLsifEntityUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], range: Tuple[()]) -> "CodemarkupLsifLsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifFileEntityXRefLocations.3 {{ file = _, xref = _, entity = _ }}", LsifLsifFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupLsifLsifFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.EntityInfo.3 {{ entity = _, info = _ }}", LsifEntityInfo

  @staticmethod
  def angle_query(*, entity: Tuple[()], info: Tuple[()]) -> "CodemarkupLsifEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifKindToKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.lsif.LsifKindToKind.3 {{ lsif = _, kind = _ }}", LsifLsifKindToKind

  @staticmethod
  def angle_query(*, lsif: Tuple[()], kind: Tuple[()]) -> "CodemarkupLsifLsifKindToKind":
    raise Exception("this function can only be called from @angle_query")


