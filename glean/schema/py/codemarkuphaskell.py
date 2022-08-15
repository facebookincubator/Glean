# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupHaskellHaskellEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.haskell.HaskellEntityLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupHaskellHaskellEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupHaskellHaskellResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.haskell.HaskellResolveLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupHaskellHaskellResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupHaskellHaskellFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.haskell.HaskellFileEntityXRefLocations.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupHaskellHaskellFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupHaskellHaskellEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.haskell.HaskellEntityUses.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupHaskellHaskellEntityUses":
    raise Exception("this function can only be called from @angle_query")


