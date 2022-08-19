# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codexref.types import (
    OutgoingXRefs,
    SymbolName,
    IncomingXRefs,
)


class CodexrefOutgoingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codexref.OutgoingXRefs.6 { { } }", OutgoingXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodexrefOutgoingXRefs":
    raise Exception("this function can only be called from @angle_query")

class CodexrefSymbolName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codexref.SymbolName.6 { json.dumps(key) }", SymbolName

  @staticmethod
  def angle_query(*, name: str) -> "CodexrefSymbolName":
    raise Exception("this function can only be called from @angle_query")

class CodexrefIncomingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codexref.IncomingXRefs.6 { { } }", IncomingXRefs

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")


