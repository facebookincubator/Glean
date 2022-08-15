# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSDeadcodeGraphInverseEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphInverseEdge.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDeadcodeGraphInverseEdge":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphNode.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDeadcodeGraphNode":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphEntityByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphEntityByFile.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDeadcodeGraphEntityByFile":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphEntity.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDeadcodeGraphEntity":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphEdge.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDeadcodeGraphEdge":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphNodeByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphNodeByEntity.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSDeadcodeGraphNodeByEntity":
    raise Exception("this function can only be called from @angle_query")


