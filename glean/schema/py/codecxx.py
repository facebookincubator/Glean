# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just


from glean.schema.codecxx.types import (
    cxxDeclToDef,
)


class CodeCxxDeclToDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    return f"code.cxx.DeclToDef.4 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, defn, 'defn')])) or '_' } }}", cxxDeclToDef

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "CodeCxxDeclToDef":
    raise Exception("this function can only be called from @angle_query")




