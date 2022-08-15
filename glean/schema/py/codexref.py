# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodexrefOutgoingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codexref.OutgoingXRefs.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodexrefOutgoingXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSCodexrefSymbolName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codexref.SymbolName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodexrefSymbolName":
    raise Exception("this function can only be called from @angle_query")

class GSCodexrefIncomingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codexref.IncomingXRefs.6 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")


