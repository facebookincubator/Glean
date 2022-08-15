# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class CodexrefOutgoingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codexref.OutgoingXRefs.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodexrefOutgoingXRefs":
    raise Exception("this function can only be called from @angle_query")

class CodexrefSymbolName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codexref.SymbolName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "CodexrefSymbolName":
    raise Exception("this function can only be called from @angle_query")

class CodexrefIncomingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codexref.IncomingXRefs.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")


