# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.codexref.types import (
    OutgoingXRefs,
    SymbolName,
    IncomingXRefs,
)


class CodexrefOutgoingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, shard: ast.Expr) -> Tuple[str, Struct]:
    return f"codexref.OutgoingXRefs.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, shard, 'shard')])) or '_' } }}", OutgoingXRefs

  @staticmethod
  def angle_query(*, file: Optional[str] = None, shard: Optional[Tuple[()]] = None) -> "CodexrefOutgoingXRefs":
    raise Exception("this function can only be called from @angle_query")



class CodexrefSymbolName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"codexref.SymbolName.6 { angle_for(__env, arg, None) or '_' }", SymbolName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "CodexrefSymbolName":
    raise Exception("this function can only be called from @angle_query")



class CodexrefIncomingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, shard: ast.Expr) -> Tuple[str, Struct]:
    return f"codexref.IncomingXRefs.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, shard, 'shard')])) or '_' } }}", IncomingXRefs

  @staticmethod
  def angle_query(*, file: Optional[str] = None, shard: Optional[Tuple[()]] = None) -> "CodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")




