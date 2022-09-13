# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codebuck import *
from glean.schema.py.codecxx import *
from glean.schema.py.codeerlang import *
from glean.schema.py.codeflow import *
from glean.schema.py.codehack import *
from glean.schema.py.codehs import *
from glean.schema.py.codejava import *
from glean.schema.py.codelsif import *
from glean.schema.py.codepp import *
from glean.schema.py.codepython import *
from glean.schema.py.coderust import *
from glean.schema.py.codethrift import *


from glean.schema.code.types import (
    EntityLanguageLSIF,
    EntityLanguage,
    Language,
    Annotations,
    Entity,
)


class CodeEntityLanguageLSIF(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"code.EntityLanguageLSIF.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, language, 'language')])) or '_' } }}", EntityLanguageLSIF

  @staticmethod
  def angle_query(*, entity: Optional["CodeLsifEntity"] = None, language: Optional["CodeLanguage"] = None) -> "CodeEntityLanguageLSIF":
    raise Exception("this function can only be called from @angle_query")



class CodeEntityLanguage(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"code.EntityLanguage.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, language, 'language')])) or '_' } }}", EntityLanguage

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, language: Optional["CodeLanguage"] = None) -> "CodeEntityLanguage":
    raise Exception("this function can only be called from @angle_query")





class CodeLanguage(Enum):
  Cpp = 0
  PreProcessor = 1
  Java = 2
  Haskell = 3
  Python = 4
  Hack = 5
  JavaScript = 6
  Rust = 7
  Thrift = 8
  Buck = 9
  Erlang = 10
  FSharp = 11
  Go = 12
  Kotlin = 13
  OCaml = 14
  Scala = 15
  Swift = 16
  TypeScript = 17

class CodeAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cxx: ast.Expr, java: ast.Expr, python: ast.Expr, hack: ast.Expr, thrift: ast.Expr) -> Tuple[str, Struct]:
    return f"code.Annotations.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, cxx, 'cxx'), angle_for(__env, java, 'java'), angle_for(__env, python, 'python'), angle_for(__env, hack, 'hack'), angle_for(__env, thrift, 'thrift')])) or '_' } }}", Annotations

  @staticmethod
  def angle_query_cxx(*, cxx: Optional["CodeCxxAnnotations"] = None) -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_java(*, java: Optional["CodeJavaAnnotations"] = None) -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_python(*, python: Optional["CodePythonAnnotations"] = None) -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hack(*, hack: Optional["CodeHackAnnotations"] = None) -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_thrift(*, thrift: Optional["CodeThriftAnnotations"] = None) -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")




class CodeEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cxx: ast.Expr, pp: ast.Expr, java: ast.Expr, hs: ast.Expr, python: ast.Expr, hack: ast.Expr, flow: ast.Expr, rust: ast.Expr, thrift: ast.Expr, buck: ast.Expr, erlang: ast.Expr, lsif: ast.Expr) -> Tuple[str, Struct]:
    return f"code.Entity.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, cxx, 'cxx'), angle_for(__env, pp, 'pp'), angle_for(__env, java, 'java'), angle_for(__env, hs, 'hs'), angle_for(__env, python, 'python'), angle_for(__env, hack, 'hack'), angle_for(__env, flow, 'flow'), angle_for(__env, rust, 'rust'), angle_for(__env, thrift, 'thrift'), angle_for(__env, buck, 'buck'), angle_for(__env, erlang, 'erlang'), angle_for(__env, lsif, 'lsif')])) or '_' } }}", Entity

  @staticmethod
  def angle_query_cxx(*, cxx: Optional["CodeCxxEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_pp(*, pp: Optional["CodePpEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_java(*, java: Optional["CodeJavaEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hs(*, hs: Optional["CodeHsEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_python(*, python: Optional["CodePythonEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hack(*, hack: Optional["CodeHackEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_flow(*, flow: Optional["CodeFlowEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_rust(*, rust: Optional["CodeRustEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_thrift(*, thrift: Optional["CodeThriftEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_buck(*, buck: Optional["CodeBuckEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_erlang(*, erlang: Optional["CodeErlangEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_lsif(*, lsif: Optional["CodeLsifEntity"] = None) -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")





