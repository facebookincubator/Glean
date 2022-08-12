# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodexrefOutgoingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codexref.OutgoingXRefs.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodexrefOutgoingXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSCodexrefSymbolName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codexref.SymbolName.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodexrefSymbolName":
    raise Exception("this function can only be called from @angle_query")

class GSCodexrefIncomingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"codexref.IncomingXRefs.6 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")


