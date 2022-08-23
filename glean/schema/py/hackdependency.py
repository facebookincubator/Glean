# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.hackdependency.types import (
    Inheritance,
    Name,
)


class HackdependencyInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hackdependency.inheritance.1 {{ }}", Inheritance
    return f"hackdependency.inheritance.1 {{ parent = _, child = _ }}", Inheritance

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "HackdependencyInheritance":
    raise Exception("this function can only be called from @angle_query")

class HackdependencyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hackdependency.name.1 {{ }}", Name
    return f"hackdependency.name.1 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackdependencyName":
    raise Exception("this function can only be called from @angle_query")


