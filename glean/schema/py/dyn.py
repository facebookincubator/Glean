# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSDynObserverIdentifier(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.ObserverIdentifier.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDynObserverIdentifier":
    raise Exception("this function can only be called from @angle_query")

class GSDynEntityDynamicReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.EntityDynamicReference.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDynEntityDynamicReference":
    raise Exception("this function can only be called from @angle_query")

class GSDynEntityUsage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.EntityUsage.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDynEntityUsage":
    raise Exception("this function can only be called from @angle_query")

class GSDynEnvironment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"dyn.Environment.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDynEnvironment":
    raise Exception("this function can only be called from @angle_query")


