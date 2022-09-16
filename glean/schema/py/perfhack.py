# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.src import *


from glean.schema.perf_hack.types import (
    LoopCounts,
    FunctionData,
    ReturnPercentages,
    FunctionProducts,
    LoopCount,
    FunctionCallers,
    FileLine,
    ReturnCount,
)


class PerfHackLoopCounts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, loop_counts: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, loop_counts, 'loop_counts')]))
    return f"perf.hack.LoopCounts.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LoopCounts

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, loop_counts: Optional[List["PerfHackLoopCount"]] = None) -> "PerfHackLoopCounts":
    raise Exception("this function can only be called from @angle_query")



class PerfHackFunctionData(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], function_definition: ast.Expr, function_name: ast.Expr, full_name: ast.Expr, calls_per_request: ast.Expr, inclusive_gcpu: ast.Expr, exclusive_gcpu: ast.Expr, total_samples: ast.Expr, callers: ast.Expr, products: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, function_definition, 'function_definition'), angle_for(__env, function_name, 'function_name'), angle_for(__env, full_name, 'full_name'), angle_for(__env, calls_per_request, 'calls_per_request'), angle_for(__env, inclusive_gcpu, 'inclusive_gcpu'), angle_for(__env, exclusive_gcpu, 'exclusive_gcpu'), angle_for(__env, total_samples, 'total_samples'), angle_for(__env, callers, 'callers'), angle_for(__env, products, 'products')]))
    return f"perf.hack.FunctionData.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FunctionData

  @staticmethod
  def angle_query(*, function_definition: Optional["PerfHackFileLine"] = None, function_name: Optional["PerfHackFunctionName"] = None, full_name: Optional["PerfHackFullName"] = None, calls_per_request: Optional["PerfHackScaledMillion"] = None, inclusive_gcpu: Optional["PerfHackScaledMillion"] = None, exclusive_gcpu: Optional["PerfHackScaledMillion"] = None, total_samples: Optional[int] = None, callers: Optional[List["PerfHackFunctionCallers"]] = None, products: Optional[List["PerfHackFunctionProducts"]] = None) -> "PerfHackFunctionData":
    raise Exception("this function can only be called from @angle_query")



class PerfHackReturnPercentages(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, full_name: ast.Expr, total_sample_count: ast.Expr, return_counts: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, full_name, 'full_name'), angle_for(__env, total_sample_count, 'total_sample_count'), angle_for(__env, return_counts, 'return_counts')]))
    return f"perf.hack.ReturnPercentages.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ReturnPercentages

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, full_name: Optional[str] = None, total_sample_count: Optional[int] = None, return_counts: Optional[List["PerfHackReturnCount"]] = None) -> "PerfHackReturnPercentages":
    raise Exception("this function can only be called from @angle_query")





class PerfHackFunctionProducts(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], product_name: ast.Expr, product_call_percent: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, product_name, 'product_name'), angle_for(__env, product_call_percent, 'product_call_percent')]))
    return f"perf.hack.FunctionProducts.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FunctionProducts

  @staticmethod
  def angle_query(*, product_name: Optional[str] = None, product_call_percent: Optional["PerfHackScaledMillion"] = None) -> "PerfHackFunctionProducts":
    raise Exception("this function can only be called from @angle_query")



class PerfHackLoopCount(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], line_number: ast.Expr, num_iterations: ast.Expr, count: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, line_number, 'line_number'), angle_for(__env, num_iterations, 'num_iterations'), angle_for(__env, count, 'count')]))
    return f"perf.hack.LoopCount.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LoopCount

  @staticmethod
  def angle_query(*, line_number: Optional[int] = None, num_iterations: Optional[int] = None, count: Optional[int] = None) -> "PerfHackLoopCount":
    raise Exception("this function can only be called from @angle_query")



class PerfHackFunctionCallers(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], caller_name: ast.Expr, caller_location: ast.Expr, caller_call_percent: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, caller_name, 'caller_name'), angle_for(__env, caller_location, 'caller_location'), angle_for(__env, caller_call_percent, 'caller_call_percent')]))
    return f"perf.hack.FunctionCallers.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FunctionCallers

  @staticmethod
  def angle_query(*, caller_name: Optional[str] = None, caller_location: Optional["PerfHackFileLine"] = None, caller_call_percent: Optional["PerfHackScaledMillion"] = None) -> "PerfHackFunctionCallers":
    raise Exception("this function can only be called from @angle_query")



class PerfHackFileLine(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, line: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, line, 'line')]))
    return f"perf.hack.FileLine.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileLine

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, line: Optional[int] = None) -> "PerfHackFileLine":
    raise Exception("this function can only be called from @angle_query")



class PerfHackReturnCount(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], line_number: ast.Expr, exit_sample_count: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, line_number, 'line_number'), angle_for(__env, exit_sample_count, 'exit_sample_count')]))
    return f"perf.hack.ReturnCount.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ReturnCount

  @staticmethod
  def angle_query(*, line_number: Optional[int] = None, exit_sample_count: Optional[int] = None) -> "PerfHackReturnCount":
    raise Exception("this function can only be called from @angle_query")





PerfHackScaledMillion = int

PerfHackFullName = str

PerfHackFunctionName = str
