# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate


from glean.schema.codeerlang.types import (
    erlangEntity,
)




class CodeErlangEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr) -> Tuple[str, Struct]:
    return f"code.erlang.Entity.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl')])) or '_' } }}", erlangEntity

  @staticmethod
  def angle_query_decl(*, decl: "ErlangDeclaration") -> "CodeErlangEntity":
    raise Exception("this function can only be called from @angle_query")





