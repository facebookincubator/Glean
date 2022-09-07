# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codelsif.types import (
    lsifEntity,
)




class CodeLsifEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], erlang: ast.Expr, fsharp: ast.Expr, go: ast.Expr, haskell: ast.Expr, java: ast.Expr, kotlin: ast.Expr, ocaml: ast.Expr, python: ast.Expr, rust: ast.Expr, scala: ast.Expr, swift: ast.Expr, typescript: ast.Expr) -> Tuple[str, Struct]:
    return f"code.lsif.Entity.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, erlang, 'erlang'), angle_for(__env, fsharp, 'fsharp'), angle_for(__env, go, 'go'), angle_for(__env, haskell, 'haskell'), angle_for(__env, java, 'java'), angle_for(__env, kotlin, 'kotlin'), angle_for(__env, ocaml, 'ocaml'), angle_for(__env, python, 'python'), angle_for(__env, rust, 'rust'), angle_for(__env, scala, 'scala'), angle_for(__env, swift, 'swift'), angle_for(__env, typescript, 'typescript')])) or '_' } }}", lsifEntity

  @staticmethod
  def angle_query_erlang(*, erlang: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_fsharp(*, fsharp: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_go(*, go: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_haskell(*, haskell: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_java(*, java: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_kotlin(*, kotlin: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_ocaml(*, ocaml: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_python(*, python: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_rust(*, rust: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_scala(*, scala: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_swift(*, swift: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typescript(*, typescript: "LsifSomeEntity") -> "CodeLsifEntity":
    raise Exception("this function can only be called from @angle_query")





