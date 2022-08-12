# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSDeadcodeGraphInverseEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"deadcode.GraphInverseEdge.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDeadcodeGraphInverseEdge":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"deadcode.GraphNode.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDeadcodeGraphNode":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphEntityByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"deadcode.GraphEntityByFile.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDeadcodeGraphEntityByFile":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"deadcode.GraphEntity.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDeadcodeGraphEntity":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"deadcode.GraphEdge.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDeadcodeGraphEdge":
    raise Exception("this function can only be called from @angle_query")

class GSDeadcodeGraphNodeByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"deadcode.GraphNodeByEntity.7 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSDeadcodeGraphNodeByEntity":
    raise Exception("this function can only be called from @angle_query")


