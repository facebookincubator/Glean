# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.dyn.types import (
    ObserverIdentifier,
    EntityDynamicReference,
    EntityUsage,
    Environment,
)


class DynObserverIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"dyn.ObserverIdentifier.6 { json.dumps(key) }", ObserverIdentifier

  @staticmethod
  def angle_query(*, name: str) -> "DynObserverIdentifier":
    raise Exception("this function can only be called from @angle_query")

class DynEntityDynamicReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"dyn.EntityDynamicReference.6 { { } }", EntityDynamicReference

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DynEntityDynamicReference":
    raise Exception("this function can only be called from @angle_query")

class DynEntityUsage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"dyn.EntityUsage.6 { { } }", EntityUsage

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DynEntityUsage":
    raise Exception("this function can only be called from @angle_query")

class DynEnvironment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"dyn.Environment.6 { json.dumps(key) }", Environment

  @staticmethod
  def angle_query(*, name: str) -> "DynEnvironment":
    raise Exception("this function can only be called from @angle_query")


