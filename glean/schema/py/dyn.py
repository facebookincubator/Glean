# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    if key is None:
      return f"dyn.ObserverIdentifier.6 {{ }}", ObserverIdentifier
    return f"dyn.ObserverIdentifier.6 {json.dumps(key)}", ObserverIdentifier

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "DynObserverIdentifier":
    raise Exception("this function can only be called from @angle_query")

class DynEntityDynamicReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"dyn.EntityDynamicReference.6 {{ }}", EntityDynamicReference
    return f"dyn.EntityDynamicReference.6 {{ usage = _, file = _, span = _ }}", EntityDynamicReference

  @staticmethod
  def angle_query(*, usage: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "DynEntityDynamicReference":
    raise Exception("this function can only be called from @angle_query")

class DynEntityUsage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"dyn.EntityUsage.6 {{ }}", EntityUsage
    return f"dyn.EntityUsage.6 {{ entity = _, observer = _, usage = _, environment = _ }}", EntityUsage

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, observer: Optional[Tuple[()]] = None, usage: Optional[Tuple[()]] = None, environment: Optional[Tuple[()]] = None) -> "DynEntityUsage":
    raise Exception("this function can only be called from @angle_query")

class DynEnvironment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"dyn.Environment.6 {{ }}", Environment
    return f"dyn.Environment.6 {json.dumps(key)}", Environment

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "DynEnvironment":
    raise Exception("this function can only be called from @angle_query")


