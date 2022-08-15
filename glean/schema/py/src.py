# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class SrcIndexFailure(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.IndexFailure.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcIndexFailure":
    raise Exception("this function can only be called from @angle_query")

class SrcByteSpanContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.ByteSpanContains.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcByteSpanContains":
    raise Exception("this function can only be called from @angle_query")

class SrcFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.File.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "SrcFile":
    raise Exception("this function can only be called from @angle_query")

class SrcFileLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.FileLanguage.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcFileLanguage":
    raise Exception("this function can only be called from @angle_query")

class SrcFileLines(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.FileLines.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "SrcFileLines":
    raise Exception("this function can only be called from @angle_query")


