# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.gencode.types import (
    GenCodeSignature,
    GenCodeBySource,
    GenCodeCommand,
    GenCodeClass,
    GenCode,
)


class GencodeGenCodeSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"gencode.GenCodeSignature.1 { json.dumps(key) }", GenCodeSignature

  @staticmethod
  def angle_query(*, name: str) -> "GencodeGenCodeSignature":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeBySource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"gencode.GenCodeBySource.1 { { } }", GenCodeBySource

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GencodeGenCodeBySource":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeCommand(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"gencode.GenCodeCommand.1 { json.dumps(key) }", GenCodeCommand

  @staticmethod
  def angle_query(*, name: str) -> "GencodeGenCodeCommand":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"gencode.GenCodeClass.1 { json.dumps(key) }", GenCodeClass

  @staticmethod
  def angle_query(*, name: str) -> "GencodeGenCodeClass":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"gencode.GenCode.1 { { } }", GenCode

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GencodeGenCode":
    raise Exception("this function can only be called from @angle_query")


