# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.codexref.types import (
    OutgoingXRefs,
    SymbolName,
    IncomingXRefs,
)


class CodexrefOutgoingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codexref.OutgoingXRefs.6 {{ }}", OutgoingXRefs
    return f"codexref.OutgoingXRefs.6 { concatenateFields(key) }", OutgoingXRefs

  @staticmethod
  def angle_query(*, file: Optional[str] = None, shard: Optional[Tuple[()]] = None) -> "CodexrefOutgoingXRefs":
    raise Exception("this function can only be called from @angle_query")

class CodexrefSymbolName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codexref.SymbolName.6 {{ }}", SymbolName
    return f"codexref.SymbolName.6 {key}", SymbolName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "CodexrefSymbolName":
    raise Exception("this function can only be called from @angle_query")

class CodexrefIncomingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"codexref.IncomingXRefs.6 {{ }}", IncomingXRefs
    return f"codexref.IncomingXRefs.6 { concatenateFields(key) }", IncomingXRefs

  @staticmethod
  def angle_query(*, file: Optional[str] = None, shard: Optional[Tuple[()]] = None) -> "CodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")


