# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSHackdependencyInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hackdependency.inheritance.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackdependencyInheritance":
    raise Exception("this function can only be called from @angle_query")

class GSHackdependencyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"hackdependency.name.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSHackdependencyName":
    raise Exception("this function can only be called from @angle_query")


