# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.deadcode.types import (
    GraphInverseEdge,
    GraphNode,
    GraphEntityByFile,
    GraphEntity,
    GraphEdge,
    GraphNodeByEntity,
)


class DeadcodeGraphInverseEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"deadcode.GraphInverseEdge.7 {{ to = _, from = _ }}", GraphInverseEdge

  @staticmethod
  def angle_query(*, to: Optional[Tuple[()]] = None, from: Optional[Tuple[()]] = None) -> "DeadcodeGraphInverseEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"deadcode.GraphNode.7 {{ type = _, entities = _, out_degree = _, in_degree = _ }}", GraphNode

  @staticmethod
  def angle_query(*, type: Optional[Tuple[()]] = None, entities: Optional[Tuple[()]] = None, out_degree: Optional[int] = None, in_degree: Optional[int] = None) -> "DeadcodeGraphNode":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntityByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"deadcode.GraphEntityByFile.7 {{ file = _, graph_entity = _ }}", GraphEntityByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, graph_entity: Optional[Tuple[()]] = None) -> "DeadcodeGraphEntityByFile":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"deadcode.GraphEntity.7 {{ entity = _, file = _, span = _ }}", GraphEntity

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "DeadcodeGraphEntity":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"deadcode.GraphEdge.7 {{ from = _, to = _ }}", GraphEdge

  @staticmethod
  def angle_query(*, from: Optional[Tuple[()]] = None, to: Optional[Tuple[()]] = None) -> "DeadcodeGraphEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNodeByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"deadcode.GraphNodeByEntity.7 {{ entity = _, node = _ }}", GraphNodeByEntity

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, node: Optional[Tuple[()]] = None) -> "DeadcodeGraphNodeByEntity":
    raise Exception("this function can only be called from @angle_query")


