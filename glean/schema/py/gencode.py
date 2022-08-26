# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.gencode.types import (
    GenCodeSignature,
    GenCodeBySource,
    GenCodeCommand,
    GenCodeClass,
    GenCode,
)


class GencodeGenCodeSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"gencode.GenCodeSignature.1 {{ }}", GenCodeSignature
    return f"gencode.GenCodeSignature.1 {key}", GenCodeSignature

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeSignature":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeBySource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"gencode.GenCodeBySource.1 {{ }}", GenCodeBySource
    return f"gencode.GenCodeBySource.1 { concatenateFields(key) }", GenCodeBySource

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, gencode: Optional[Tuple[()]] = None) -> "GencodeGenCodeBySource":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeCommand(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"gencode.GenCodeCommand.1 {{ }}", GenCodeCommand
    return f"gencode.GenCodeCommand.1 {key}", GenCodeCommand

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeCommand":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"gencode.GenCodeClass.1 {{ }}", GenCodeClass
    return f"gencode.GenCodeClass.1 {key}", GenCodeClass

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeClass":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"gencode.GenCode.1 {{ }}", GenCode
    return f"gencode.GenCode.1 { concatenateFields(key) }", GenCode

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, variant: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, command: Optional[Tuple[()]] = None, class_: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None) -> "GencodeGenCode":
    raise Exception("this function can only be called from @angle_query")


