# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


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





class CodeLanguage(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"code.Language.24 { angle_for(__env, arg, None) or '_' }", Language

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "CodeLanguage":
    raise Exception("this function can only be called from @angle_query")



class CodeAnnotations(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cxx: ast.Expr, java: ast.Expr, python: ast.Expr, hack: ast.Expr, thrift: ast.Expr) -> Tuple[str, Struct]:
    return f"code.Annotations.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, cxx, 'cxx'), angle_for(__env, java, 'java'), angle_for(__env, python, 'python'), angle_for(__env, hack, 'hack'), angle_for(__env, thrift, 'thrift')])) or '_' } }}", Annotations

  @staticmethod
  def angle_query_cxx(*, cxx: "CodeCxxAnnotations") -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_java(*, java: "CodeJavaAnnotations") -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_python(*, python: "CodePythonAnnotations") -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hack(*, hack: "CodeHackAnnotations") -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_thrift(*, thrift: "CodeThriftAnnotations") -> "CodeAnnotations":
    raise Exception("this function can only be called from @angle_query")




class CodeEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cxx: ast.Expr, pp: ast.Expr, java: ast.Expr, hs: ast.Expr, python: ast.Expr, hack: ast.Expr, flow: ast.Expr, rust: ast.Expr, thrift: ast.Expr, buck: ast.Expr, erlang: ast.Expr, lsif: ast.Expr) -> Tuple[str, Struct]:
    return f"code.Entity.24 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, cxx, 'cxx'), angle_for(__env, pp, 'pp'), angle_for(__env, java, 'java'), angle_for(__env, hs, 'hs'), angle_for(__env, python, 'python'), angle_for(__env, hack, 'hack'), angle_for(__env, flow, 'flow'), angle_for(__env, rust, 'rust'), angle_for(__env, thrift, 'thrift'), angle_for(__env, buck, 'buck'), angle_for(__env, erlang, 'erlang'), angle_for(__env, lsif, 'lsif')])) or '_' } }}", Entity

  @staticmethod
  def angle_query_cxx(*, cxx: "CodeCxxEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_pp(*, pp: "CodePpEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_java(*, java: "CodeJavaEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hs(*, hs: "CodeHsEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_python(*, python: "CodePythonEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hack(*, hack: "CodeHackEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_flow(*, flow: "CodeFlowEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_rust(*, rust: "CodeRustEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_thrift(*, thrift: "CodeThriftEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_buck(*, buck: "CodeBuckEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_erlang(*, erlang: "CodeErlangEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_lsif(*, lsif: "CodeLsifEntity") -> "CodeEntity":
    raise Exception("this function can only be called from @angle_query")





