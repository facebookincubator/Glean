# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.src.types import (
    IndexFailure,
    ByteSpanContains,
    File,
    FileLanguage,
    FileLines,
)


class SrcIndexFailure(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"src.IndexFailure.1 { { } }", IndexFailure

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcIndexFailure":
    raise Exception("this function can only be called from @angle_query")

class SrcByteSpanContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"src.ByteSpanContains.1 { { } }", ByteSpanContains

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcByteSpanContains":
    raise Exception("this function can only be called from @angle_query")

class SrcFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"src.File.1 { json.dumps(key) }", File

  @staticmethod
  def angle_query(*, name: str) -> "SrcFile":
    raise Exception("this function can only be called from @angle_query")

class SrcFileLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"src.FileLanguage.1 { { } }", FileLanguage

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcFileLanguage":
    raise Exception("this function can only be called from @angle_query")

class SrcFileLines(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"src.FileLines.1 { { } }", FileLines

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcFileLines":
    raise Exception("this function can only be called from @angle_query")


