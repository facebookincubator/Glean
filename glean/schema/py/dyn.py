# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class DynObserverIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.ObserverIdentifier.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "DynObserverIdentifier":
    raise Exception("this function can only be called from @angle_query")

class DynEntityDynamicReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.EntityDynamicReference.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DynEntityDynamicReference":
    raise Exception("this function can only be called from @angle_query")

class DynEntityUsage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.EntityUsage.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DynEntityUsage":
    raise Exception("this function can only be called from @angle_query")

class DynEnvironment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.Environment.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "DynEnvironment":
    raise Exception("this function can only be called from @angle_query")


