# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.codemarkuprust.types import (
    RustRustEntityLocation,
    RustRustResolveLocation,
    RustRustFileEntityXRefLocations,
    RustRustEntityUses,
)


class CodemarkupRustRustEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.rust.RustEntityLocation.2 {{ }}", RustRustEntityLocation
    return f"codemarkup.rust.RustEntityLocation.2 { concatenateFields(key) }", RustRustEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupRustRustEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupRustRustResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.rust.RustResolveLocation.2 {{ }}", RustRustResolveLocation
    return f"codemarkup.rust.RustResolveLocation.2 { concatenateFields(key) }", RustRustResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupRustRustResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupRustRustFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.rust.RustFileEntityXRefLocations.2 {{ }}", RustRustFileEntityXRefLocations
    return f"codemarkup.rust.RustFileEntityXRefLocations.2 { concatenateFields(key) }", RustRustFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupRustRustFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupRustRustEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codemarkup.rust.RustEntityUses.2 {{ }}", RustRustEntityUses
    return f"codemarkup.rust.RustEntityUses.2 { concatenateFields(key) }", RustRustEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupRustRustEntityUses":
    raise Exception("this function can only be called from @angle_query")


