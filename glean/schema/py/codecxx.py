# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


from glean.schema.codecxx.types import (
    CxxDeclToDef,
)


class CodeCxxDeclToDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"code.cxx.DeclToDef.4 {{ }}", CxxDeclToDef
    return f"code.cxx.DeclToDef.4 { concatenateFields(key) }", CxxDeclToDef

  @staticmethod
  def angle_query(*, decl: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "CodeCxxDeclToDef":
    raise Exception("this function can only be called from @angle_query")


