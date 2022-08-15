# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupLsifLsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.lsif.LsifEntityLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupLsifLsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupLsifLsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.lsif.LsifResolveLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupLsifLsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupLsifLsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.lsif.LsifEntityUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupLsifLsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupLsifLsifFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.lsif.LsifFileEntityXRefLocations.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupLsifLsifFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupLsifEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.lsif.EntityInfo.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupLsifEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupLsifLsifKindToKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.lsif.LsifKindToKind.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupLsifLsifKindToKind":
    raise Exception("this function can only be called from @angle_query")


