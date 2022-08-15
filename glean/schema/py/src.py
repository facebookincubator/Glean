# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSSrcIndexFailure(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.IndexFailure.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSrcIndexFailure":
    raise Exception("this function can only be called from @angle_query")

class GSSrcByteSpanContains(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.ByteSpanContains.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSrcByteSpanContains":
    raise Exception("this function can only be called from @angle_query")

class GSSrcFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.File.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSSrcFile":
    raise Exception("this function can only be called from @angle_query")

class GSSrcFileLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.FileLanguage.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSrcFileLanguage":
    raise Exception("this function can only be called from @angle_query")

class GSSrcFileLines(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"src.FileLines.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSSrcFileLines":
    raise Exception("this function can only be called from @angle_query")


