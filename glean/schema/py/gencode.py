# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.gencode.types import (
    GenCodeSignature,
    GenCodeBySource,
    GenCodeCommand,
    GenCodeClass,
    GenCode,
)


class GencodeGenCodeSignature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCodeSignature.1 {angle_for(__env, arg)}", GenCodeSignature

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeSignature":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeBySource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, gencode: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCodeBySource.1 {{ source = {angle_for(__env, source)}, gencode = {angle_for(__env, gencode)} }}", GenCodeBySource

  @staticmethod
  def angle_query(*, source: Optional["SrcFile"] = None, gencode: Optional["SrcFile"] = None) -> "GencodeGenCodeBySource":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeCommand(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCodeCommand.1 {angle_for(__env, arg)}", GenCodeCommand

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeCommand":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCodeClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCodeClass.1 {angle_for(__env, arg)}", GenCodeClass

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeClass":
    raise Exception("this function can only be called from @angle_query")

class GencodeGenCode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, variant: ast.Expr, source: ast.Expr, command: ast.Expr, class_: ast.Expr, signature: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCode.1 {{ file = {angle_for(__env, file)}, variant = {angle_for(__env, variant)}, source = {angle_for(__env, source)}, command = {angle_for(__env, command)}, class_ = {angle_for(__env, class_)}, signature = {angle_for(__env, signature)} }}", GenCode

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, variant: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, command: Optional[Tuple[()]] = None, class_: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None) -> "GencodeGenCode":
    raise Exception("this function can only be called from @angle_query")


