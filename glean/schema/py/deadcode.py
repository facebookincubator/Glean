# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class DeadcodeGraphInverseEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphInverseEdge.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DeadcodeGraphInverseEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphNode.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DeadcodeGraphNode":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntityByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphEntityByFile.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DeadcodeGraphEntityByFile":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphEntity.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DeadcodeGraphEntity":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphEdge.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DeadcodeGraphEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNodeByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"deadcode.GraphNodeByEntity.7 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "DeadcodeGraphNodeByEntity":
    raise Exception("this function can only be called from @angle_query")


