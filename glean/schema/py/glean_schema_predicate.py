# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Dict, Tuple, TypeVar, Type, Optional
from thrift.py3 import Struct
import json
import inspect
import ast

R = TypeVar("R", int, str, bool, Tuple[()])

class GleanSchemaPredicate:
  @staticmethod
  def angle_query(*, arg: str) -> "GleanSchemaPredicate":
    raise Exception("this function can only be called from @angle_query")

def angle_for(__env: Dict[str, R], key: ast.Expr, field_name: Optional[str]) -> str:
  if key is None:
    return f''
  if isinstance(key, ast.Name):
    return _make_attribute(field_name, json.dumps(__env[key.id]))
  elif isinstance(key, ast.Constant):
    return _make_attribute(field_name, json.dumps(key.value))
  elif isinstance(key, ast.Call):
    nested_call_arg = callGleanSchemaPredicateQuery(key, {}, "__target__", __env)["__target__"]
    return _make_attribute(field_name, nested_call_arg[0])
  raise NotImplementedError(f"Query key type not implemented")

def _make_attribute(field_name: Optional[str], value: str) -> str:
  if field_name:
    return f'{field_name} = {value}'
  return value

def _class_name_to_py_query(class_name: str, __env: Dict[str, R], method_args: Dict[str, ast.Expr]) -> Tuple[str, Type[Struct]]:
  # Before this function call, we check that the node is an ast.Name (part of an ast.Call).
  # It will always evaluate to a class name.

  # Using globals() to get the class_name does not work because it needs all the user classes imported
  # in this module. Instead: we use the frame info to access the global namespace of the user's code.
  # The variable no_frames_to_user_code is dependent on the number of nested functions calls from the user code.
  num_frames_to_user_code = 3
  frame = 0
  angle_query_args = inspect.getfullargspec(
    inspect.stack()[num_frames_to_user_code][frame]
    .f_globals[class_name]
    .build_angle
  ).args[1:]
  fields = (method_args.get(k) for k in angle_query_args)
  return (
    inspect.stack()[num_frames_to_user_code][frame]
    .f_globals[class_name]
    .build_angle(__env, *tuple(fields))
  )

def callGleanSchemaPredicateQuery(function_call: ast.Call, variables: Dict[str, Tuple[str, Type[Struct]]], target: str, __env: Dict[str, R]) -> Dict[str, Tuple[str, Type[Struct]]]:
  method_args = function_call.keywords
  method_args_ = {}
  for method_arg in method_args:
    query_arg_name = method_arg.arg
    arg_val = method_arg.value
    method_args_[query_arg_name] = arg_val
  function_attribute = function_call.func
  if isinstance(function_attribute, ast.Attribute):
    class_name = function_attribute.value
    if isinstance(class_name, ast.Name):
      class_name = class_name.id
      variables[target] = _class_name_to_py_query(class_name, __env, method_args_)
  return variables

