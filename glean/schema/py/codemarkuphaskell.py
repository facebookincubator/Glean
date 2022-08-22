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
    return f"codemarkup.haskell.HaskellEntityLocation.2 {{ entity = _, location = _ }}", HaskellHaskellEntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "CodemarkupHaskellHaskellEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHaskellHaskellResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellResolveLocation.2 {{ location = _, entity = _ }}", HaskellHaskellResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "CodemarkupHaskellHaskellResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHaskellHaskellFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellFileEntityXRefLocations.2 {{ file = _, xref = _, entity = _ }}", HaskellHaskellFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupHaskellHaskellFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupHaskellHaskellEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.haskell.HaskellEntityUses.2 {{ target = _, file = _, span = _ }}", HaskellHaskellEntityUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "CodemarkupHaskellHaskellEntityUses":
    raise Exception("this function can only be called from @angle_query")


