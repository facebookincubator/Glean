# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Dict, Generic, Tuple, TypeVar, Type, Optional
from thrift.py3 import Struct
import json
import inspect
import ast

R = TypeVar("R", int, str, bool, bytes)
T = TypeVar("T")

class GleanSchemaPredicate:
  @staticmethod
  def angle_query(*, arg: str) -> "GleanSchemaPredicate":
    raise Exception("this function can only be called from @angle_query")

class InnerGleanSchemaPredicate:
  @staticmethod
  def angle_query(*, arg: str) -> "InnerGleanSchemaPredicate":
    raise Exception("this function can only be called as a parameter of a GleanSchemaPredicate")

def angle_for(__env: Dict[str, R], key: ast.Expr, field_name: Optional[str]) -> str:
  if key is None:
    return f''
  if isinstance(key, ast.Name):
    return _make_attribute(field_name, _make_value(__env[key.id]))
  elif isinstance(key, ast.Constant):
    return _make_attribute(field_name, _make_value(key.value))
  elif isinstance(key, ast.Call):
    if (isinstance(key.func, ast.Subscript) and key.func.value.id == 'Just') or (isinstance(key.func, ast.Name) and key.func.id == 'Just'):
      if key.args and isinstance(key.args[0], ast.Call):
        value = f'{{ just = {angle_for(__env, key.args[0], None)} }}'
      else:
        value = 'nothing'
      return _make_attribute(field_name, value)
    nested_call_arg = callGleanSchemaPredicateQuery(key, {}, "__target__", __env)["__target__"]
    # eliminate the name of GleanPredicate from inner call
    nested_call_arg = " ".join(nested_call_arg[0].split(" ")[1:])
    return _make_attribute(field_name, nested_call_arg)
  elif isinstance(key, ast.List):
    elems = map(lambda el: angle_for(__env, el, None), key.elts)
    value = f'[{ ", ".join(elems) }]'
    return _make_attribute(field_name, value)
  elif isinstance(key, ast.Attribute):
    return _make_attribute(field_name, key.attr)
  raise NotImplementedError(f"Query key type not implemented")

def _make_attribute(field_name: Optional[str], value: str) -> str:
  if field_name:
    return f'{field_name} = {value}'
  return value

def _make_value(value: R) -> str:
  if isinstance(value, bytes):
    value = [int(byte) for byte in value]
  return json.dumps(value)

def _class_name_to_py_query(class_name: str, __env: Dict[str, R], method_args: Dict[str, ast.Expr]) -> Tuple[str, Type[Struct]]:
  # Before this function call, we check that the node is an ast.Name (part of an ast.Call).
  # It will always evaluate to a class name.

  # Using globals() to get the class_name does not work because it needs all the user classes imported
  # in this module. Instead: we use the frame info to access the global namespace of the user's code.
  # The variable no_frames_to_user_code is dependent on the number of nested functions calls from the user code.
  num_frames_to_user_code = 3
  frame = 0
  ctr = 0
  while True:
    try:
      globals_ = inspect.stack()[num_frames_to_user_code + ctr][frame].f_globals
      angle_query_args = inspect.getfullargspec(globals_[class_name].build_angle).args[1:]
      break
    except KeyError as e:
      # inner calls to GleanSchemaPredicates need extra number of frames
      ctr = ctr + 1
  fields = (method_args.get(k) for k in angle_query_args)
  return (globals_[class_name].build_angle(__env, *tuple(fields)))

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

class Just(Generic[T]):
  just: T = None
  def __init__(self, just: T = None) -> None:
    self.just = just
  def get(self) -> Optional[T]:
    return self.just

