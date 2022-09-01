# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.perfhack.types import (
    hackLoopCounts,
    hackFunctionData,
    hackReturnPercentages,
)


class PerfHackLoopCounts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, loop_counts: ast.Expr) -> Tuple[str, Struct]:
    return f"perf.hack.LoopCounts.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, loop_counts, 'loop_counts')])) or '_' } }}", hackLoopCounts

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, loop_counts: Optional[List[Tuple[()]]] = None) -> "PerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")



class PerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], function_definition: ast.Expr, function_name: ast.Expr, full_name: ast.Expr, calls_per_request: ast.Expr, inclusive_gcpu: ast.Expr, exclusive_gcpu: ast.Expr, total_samples: ast.Expr, callers: ast.Expr, products: ast.Expr) -> Tuple[str, Struct]:
    return f"perf.hack.FunctionData.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, function_definition, 'function_definition'), angle_for(__env, function_name, 'function_name'), angle_for(__env, full_name, 'full_name'), angle_for(__env, calls_per_request, 'calls_per_request'), angle_for(__env, inclusive_gcpu, 'inclusive_gcpu'), angle_for(__env, exclusive_gcpu, 'exclusive_gcpu'), angle_for(__env, total_samples, 'total_samples'), angle_for(__env, callers, 'callers'), angle_for(__env, products, 'products')])) or '_' } }}", hackFunctionData

  @staticmethod
  def angle_query(*, function_definition: Optional[Tuple[()]] = None, function_name: Optional[Tuple[()]] = None, full_name: Optional[Tuple[()]] = None, calls_per_request: Optional[Tuple[()]] = None, inclusive_gcpu: Optional[Tuple[()]] = None, exclusive_gcpu: Optional[Tuple[()]] = None, total_samples: Optional[int] = None, callers: Optional[List[Tuple[()]]] = None, products: Optional[List[Tuple[()]]] = None) -> "PerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")



class PerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, full_name: ast.Expr, total_sample_count: ast.Expr, return_counts: ast.Expr) -> Tuple[str, Struct]:
    return f"perf.hack.ReturnPercentages.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, full_name, 'full_name'), angle_for(__env, total_sample_count, 'total_sample_count'), angle_for(__env, return_counts, 'return_counts')])) or '_' } }}", hackReturnPercentages

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, full_name: Optional[str] = None, total_sample_count: Optional[int] = None, return_counts: Optional[List[Tuple[()]]] = None) -> "PerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")




