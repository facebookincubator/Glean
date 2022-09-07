# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.testinfra import *


from glean.schema.lionhead.types import (
    CoveredHarness,
    FbId,
)


class LionheadCoveredHarness(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], harnessId: ast.Expr, root: ast.Expr) -> Tuple[str, Struct]:
    return f"lionhead.CoveredHarness.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, harnessId, 'harnessId'), angle_for(__env, root, 'root')])) or '_' } }}", CoveredHarness

  @staticmethod
  def angle_query(*, harnessId: Optional["LionheadFbId"] = None, root: Optional["TestinfraCoveredFolder"] = None) -> "LionheadCoveredHarness":
    raise Exception("this function can only be called from @angle_query")



class LionheadFbId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"lionhead.FbId.1 { angle_for(__env, arg, None) or '_' }", FbId

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "LionheadFbId":
    raise Exception("this function can only be called from @angle_query")






