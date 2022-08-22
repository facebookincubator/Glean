# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codecxx.types import (
    CxxDeclToDef,
)


class CodeCxxDeclToDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"code.cxx.DeclToDef.4 {{ decl = _, defn = _ }}", CxxDeclToDef

  @staticmethod
  def angle_query(*, decl: Tuple[()], defn: Tuple[()]) -> "CodeCxxDeclToDef":
    raise Exception("this function can only be called from @angle_query")


