# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codexref.types import (
    OutgoingXRefs,
    SymbolName,
    IncomingXRefs,
    Shard,
    Location,
    XRefDatum,
)


class CodexrefOutgoingXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, shard: ast.Expr) -> Tuple[str, Struct]:
    return f"codexref.OutgoingXRefs.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, shard, 'shard')])) or '_' } }}", OutgoingXRefs

  @staticmethod
  def angle_query(*, file: Optional[str] = None, shard: Optional["CodexrefShard"] = None) -> "CodexrefOutgoingXRefs":
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
  def angle_query(*, file: Optional[str] = None, shard: Optional["CodexrefShard"] = None) -> "CodexrefIncomingXRefs":
    raise Exception("this function can only be called from @angle_query")





class CodexrefShard(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"codexref.Shard.6 { angle_for(__env, arg, None) or '_' }", Shard

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "CodexrefShard":
    raise Exception("this function can only be called from @angle_query")



class CodexrefLocation(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], start: ast.Expr, line: ast.Expr) -> Tuple[str, Struct]:
    return f"codexref.Location.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, start, 'start'), angle_for(__env, line, 'line')])) or '_' } }}", Location

  @staticmethod
  def angle_query_start(*, start: int) -> "CodexrefLocation":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_line(*, line: int) -> "CodexrefLocation":
    raise Exception("this function can only be called from @angle_query")




class CodexrefXRefDatum(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, symbol_kind: ast.Expr, symbol_name: ast.Expr, src_location: ast.Expr, dst_location: ast.Expr) -> Tuple[str, Struct]:
    return f"codexref.XRefDatum.6 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, symbol_kind, 'symbol_kind'), angle_for(__env, symbol_name, 'symbol_name'), angle_for(__env, src_location, 'src_location'), angle_for(__env, dst_location, 'dst_location')])) or '_' } }}", XRefDatum

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, symbol_kind: Optional["CodemarkupTypesSymbolKind"] = None, symbol_name: Optional["CodexrefSymbolName"] = None, src_location: Optional["CodexrefLocation"] = None, dst_location: Optional["CodexrefLocation"] = None) -> "CodexrefXRefDatum":
    raise Exception("this function can only be called from @angle_query")




