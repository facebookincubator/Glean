# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R


from glean.schema.src.types import (
    IndexFailure,
    ByteSpanContains,
    File,
    FileLanguage,
    FileLines,
    RelByteSpan,
    ByteRange,
    IndexFailureReason,
    Loc,
    ByteSpan,
    Range,
    Language,
    FileLocation,
)


class SrcIndexFailure(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, reason: ast.Expr, details: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, reason, 'reason'), angle_for(__env, details, 'details')]))
    return f"src.IndexFailure.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", IndexFailure

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, reason: Optional["SrcIndexFailureReason"] = None, details: Optional[str] = None) -> "SrcIndexFailure":
    raise Exception("this function can only be called from @angle_query")



class SrcByteSpanContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], byteSpan: ast.Expr, contains: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, byteSpan, 'byteSpan'), angle_for(__env, contains, 'contains')]))
    return f"src.ByteSpanContains.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ByteSpanContains

  @staticmethod
  def angle_query(*, byteSpan: Optional["SrcByteSpan"] = None, contains: Optional["SrcByteSpan"] = None) -> "SrcByteSpanContains":
    raise Exception("this function can only be called from @angle_query")



class SrcFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"src.File.1 { query_fields if query_fields else '_' }", File

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "SrcFile":
    raise Exception("this function can only be called from @angle_query")



class SrcFileLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, language, 'language')]))
    return f"src.FileLanguage.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileLanguage

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, language: Optional["SrcLanguage"] = None) -> "SrcFileLanguage":
    raise Exception("this function can only be called from @angle_query")



class SrcFileLines(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, lengths: ast.Expr, endsInNewline: ast.Expr, hasUnicodeOrTabs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, lengths, 'lengths'), angle_for(__env, endsInNewline, 'endsInNewline'), angle_for(__env, hasUnicodeOrTabs, 'hasUnicodeOrTabs')]))
    return f"src.FileLines.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileLines

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, lengths: Optional[List[int]] = None, endsInNewline: Optional[bool] = None, hasUnicodeOrTabs: Optional[bool] = None) -> "SrcFileLines":
    raise Exception("this function can only be called from @angle_query")





class SrcRelByteSpan(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], offset: ast.Expr, length: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, offset, 'offset'), angle_for(__env, length, 'length')]))
    return f"src.RelByteSpan.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RelByteSpan

  @staticmethod
  def angle_query(*, offset: Optional[int] = None, length: Optional[int] = None) -> "SrcRelByteSpan":
    raise Exception("this function can only be called from @angle_query")



class SrcByteRange(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], begin: ast.Expr, end: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, begin, 'begin'), angle_for(__env, end, 'end')]))
    return f"src.ByteRange.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ByteRange

  @staticmethod
  def angle_query(*, begin: Optional[int] = None, end: Optional[int] = None) -> "SrcByteRange":
    raise Exception("this function can only be called from @angle_query")



class SrcIndexFailureReason(Enum):
  CompileError = 0

class SrcLoc(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, line: ast.Expr, column: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, line, 'line'), angle_for(__env, column, 'column')]))
    return f"src.Loc.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Loc

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, line: Optional[int] = None, column: Optional[int] = None) -> "SrcLoc":
    raise Exception("this function can only be called from @angle_query")



class SrcByteSpan(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], start: ast.Expr, length: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, start, 'start'), angle_for(__env, length, 'length')]))
    return f"src.ByteSpan.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ByteSpan

  @staticmethod
  def angle_query(*, start: Optional[int] = None, length: Optional[int] = None) -> "SrcByteSpan":
    raise Exception("this function can only be called from @angle_query")



class SrcRange(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, lineBegin: ast.Expr, columnBegin: ast.Expr, lineEnd: ast.Expr, columnEnd: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, lineBegin, 'lineBegin'), angle_for(__env, columnBegin, 'columnBegin'), angle_for(__env, lineEnd, 'lineEnd'), angle_for(__env, columnEnd, 'columnEnd')]))
    return f"src.Range.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Range

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, lineBegin: Optional[int] = None, columnBegin: Optional[int] = None, lineEnd: Optional[int] = None, columnEnd: Optional[int] = None) -> "SrcRange":
    raise Exception("this function can only be called from @angle_query")



class SrcLanguage(Enum):
  Buck = 0
  C = 1
  Cpp = 2
  Hack = 3
  Haskell = 4
  ObjC = 5
  ObjCpp = 6
  Python = 7
  Thrift = 8
  Java = 9
  GraphQL = 10

class SrcFileLocation(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"src.FileLocation.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "SrcFileLocation":
    raise Exception("this function can only be called from @angle_query")





SrcByteSpans = List["SrcRelByteSpan"]
