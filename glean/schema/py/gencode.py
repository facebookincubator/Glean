# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSGencodeGenCodeSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"gencode.GenCodeSignature.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGencodeGenCodeSignature":
    raise Exception("this function can only be called from @angle_query")

class GSGencodeGenCodeBySource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"gencode.GenCodeBySource.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGencodeGenCodeBySource":
    raise Exception("this function can only be called from @angle_query")

class GSGencodeGenCodeCommand(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"gencode.GenCodeCommand.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGencodeGenCodeCommand":
    raise Exception("this function can only be called from @angle_query")

class GSGencodeGenCodeClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"gencode.GenCodeClass.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGencodeGenCodeClass":
    raise Exception("this function can only be called from @angle_query")

class GSGencodeGenCode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"gencode.GenCode.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGencodeGenCode":
    raise Exception("this function can only be called from @angle_query")


