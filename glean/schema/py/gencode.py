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
    return f"gencode.GenCodeSignature.1 { angle_for(__env, arg, None) or '_' }", GenCodeSignature

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeSignature":
    raise Exception("this function can only be called from @angle_query")



class GencodeGenCodeBySource(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, gencode: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCodeBySource.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, gencode, 'gencode')])) or '_' } }}", GenCodeBySource

  @staticmethod
  def angle_query(*, source: Optional["SrcFile"] = None, gencode: Optional["SrcFile"] = None) -> "GencodeGenCodeBySource":
    raise Exception("this function can only be called from @angle_query")



class GencodeGenCodeCommand(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCodeCommand.1 { angle_for(__env, arg, None) or '_' }", GenCodeCommand

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeCommand":
    raise Exception("this function can only be called from @angle_query")



class GencodeGenCodeClass(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCodeClass.1 { angle_for(__env, arg, None) or '_' }", GenCodeClass

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GencodeGenCodeClass":
    raise Exception("this function can only be called from @angle_query")



class GencodeGenCode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, variant: ast.Expr, source: ast.Expr, command: ast.Expr, class_: ast.Expr, signature: ast.Expr) -> Tuple[str, Struct]:
    return f"gencode.GenCode.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, variant, 'variant'), angle_for(__env, source, 'source'), angle_for(__env, command, 'command'), angle_for(__env, class_, 'class_'), angle_for(__env, signature, 'signature')])) or '_' } }}", GenCode

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, variant: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, command: Optional[Tuple[()]] = None, class_: Optional[Tuple[()]] = None, signature: Optional[Tuple[()]] = None) -> "GencodeGenCode":
    raise Exception("this function can only be called from @angle_query")




