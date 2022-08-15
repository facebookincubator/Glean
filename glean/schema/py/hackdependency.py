# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class HackdependencyInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hackdependency.inheritance.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HackdependencyInheritance":
    raise Exception("this function can only be called from @angle_query")

class HackdependencyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"hackdependency.name.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HackdependencyName":
    raise Exception("this function can only be called from @angle_query")


