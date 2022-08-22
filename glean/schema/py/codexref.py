# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    return f"codexref.OutgoingXRefs.6 {{ file = _, shard = _ }}", OutgoingXRefs

  @staticmethod
  def angle_query(*, file: Optional[str] = None, shard: Optional[Tuple[()]] = None) -> "CodexrefOutgoingXRefs":
    raise Exception("this function can only be called from @angle_query")

class CodexrefSymbolName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codexref.SymbolName.6 {json.dumps(key)}", SymbolName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "CodexrefSymbolName":
    raise Exception("this function can only be called from @angle_query")

class CodexrefIncomingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codexref.IncomingXRefs.6 {{ file = _, shard = _ }}", IncomingXRefs

  @staticmethod
  def angle_query(*, file: Optional[str] = None, shard: Optional[Tuple[()]] = None) -> "CodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")


