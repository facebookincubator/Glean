# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.src.types import (
    IndexFailure,
    ByteSpanContains,
    File,
    FileLanguage,
    FileLines,
)


class SrcIndexFailure(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, reason: ast.Expr, details: ast.Expr) -> Tuple[str, Struct]:
    return f"src.IndexFailure.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, reason, 'reason'), angle_for(__env, details, 'details')])) or '_' } }}", IndexFailure

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, reason: Optional[Tuple[()]] = None, details: Optional[str] = None) -> "SrcIndexFailure":
    raise Exception("this function can only be called from @angle_query")



class SrcByteSpanContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], byteSpan: ast.Expr, contains: ast.Expr) -> Tuple[str, Struct]:
    return f"src.ByteSpanContains.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, byteSpan, 'byteSpan'), angle_for(__env, contains, 'contains')])) or '_' } }}", ByteSpanContains

  @staticmethod
  def angle_query(*, byteSpan: Optional[Tuple[()]] = None, contains: Optional[Tuple[()]] = None) -> "SrcByteSpanContains":
    raise Exception("this function can only be called from @angle_query")



class SrcFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"src.File.1 { angle_for(__env, arg, None) or '_' }", File

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "SrcFile":
    raise Exception("this function can only be called from @angle_query")



class SrcFileLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"src.FileLanguage.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, language, 'language')])) or '_' } }}", FileLanguage

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, language: Optional[Tuple[()]] = None) -> "SrcFileLanguage":
    raise Exception("this function can only be called from @angle_query")



class SrcFileLines(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, lengths: ast.Expr, endsInNewline: ast.Expr, hasUnicodeOrTabs: ast.Expr) -> Tuple[str, Struct]:
    return f"src.FileLines.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, lengths, 'lengths'), angle_for(__env, endsInNewline, 'endsInNewline'), angle_for(__env, hasUnicodeOrTabs, 'hasUnicodeOrTabs')])) or '_' } }}", FileLines

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, lengths: Optional[Tuple[()]] = None, endsInNewline: Optional[bool] = None, hasUnicodeOrTabs: Optional[bool] = None) -> "SrcFileLines":
    raise Exception("this function can only be called from @angle_query")




