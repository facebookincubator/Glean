# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
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
    return f"hackdependency.inheritance.1 {{ parent = _, child = _ }}", Inheritance

  @staticmethod
  def angle_query(*, parent: Tuple[()], child: Tuple[()]) -> "HackdependencyInheritance":
    raise Exception("this function can only be called from @angle_query")

class HackdependencyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"hackdependency.name.1 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: str) -> "HackdependencyName":
    raise Exception("this function can only be called from @angle_query")


