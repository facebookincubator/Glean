# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkuphaskell.types import (
    HaskellHaskellEntityLocation,
    HaskellHaskellResolveLocation,
    HaskellHaskellFileEntityXRefLocations,
    HaskellHaskellEntityUses,
)


class CodemarkupHaskellHaskellEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellEntityLocation.2 { { } }", HaskellHaskellEntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHaskellHaskellEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHaskellHaskellResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellResolveLocation.2 { { } }", HaskellHaskellResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHaskellHaskellResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHaskellHaskellFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellFileEntityXRefLocations.2 { { } }", HaskellHaskellFileEntityXRefLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHaskellHaskellFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHaskellHaskellEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellEntityUses.2 { { } }", HaskellHaskellEntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupHaskellHaskellEntityUses":
    raise Exception("this function can only be called from @angle_query")


