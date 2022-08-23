# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"codemarkup.lsif.LsifEntityLocation.3 {{ }}", LsifLsifEntityLocation
    return f"codemarkup.lsif.LsifEntityLocation.3 {{ entity = _, location = _ }}", LsifLsifEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.lsif.LsifResolveLocation.3 {{ }}", LsifLsifResolveLocation
    return f"codemarkup.lsif.LsifResolveLocation.3 {{ location = _, entity = _ }}", LsifLsifResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.lsif.LsifEntityUses.3 {{ }}", LsifLsifEntityUses
    return f"codemarkup.lsif.LsifEntityUses.3 {{ target = _, file = _, range = _ }}", LsifLsifEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.lsif.LsifFileEntityXRefLocations.3 {{ }}", LsifLsifFileEntityXRefLocations
    return f"codemarkup.lsif.LsifFileEntityXRefLocations.3 {{ file = _, xref = _, entity = _ }}", LsifLsifFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.lsif.EntityInfo.3 {{ }}", LsifEntityInfo
    return f"codemarkup.lsif.EntityInfo.3 {{ entity = _, info = _ }}", LsifEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupLsifEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupLsifLsifKindToKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.lsif.LsifKindToKind.3 {{ }}", LsifLsifKindToKind
    return f"codemarkup.lsif.LsifKindToKind.3 {{ lsif = _, kind = _ }}", LsifLsifKindToKind

  @staticmethod
  def angle_query(*, lsif: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupLsifLsifKindToKind":
    raise Exception("this function can only be called from @angle_query")


