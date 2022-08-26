# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.hackdependency.types import (
    Inheritance,
    Name,
)


class HackdependencyInheritance(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hackdependency.inheritance.1 {{ }}", Inheritance
    return f"hackdependency.inheritance.1 { concatenateFields(key) }", Inheritance

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "HackdependencyInheritance":
    raise Exception("this function can only be called from @angle_query")

class HackdependencyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"hackdependency.name.1 {{ }}", Name
    return f"hackdependency.name.1 {key}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HackdependencyName":
    raise Exception("this function can only be called from @angle_query")


