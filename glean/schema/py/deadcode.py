# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"deadcode.GraphInverseEdge.7 {{ }}", GraphInverseEdge
    return f"deadcode.GraphInverseEdge.7 { concatenateFields(key) }", GraphInverseEdge

  @staticmethod
  def angle_query(*, to: Optional[Tuple[()]] = None, _from: Optional[Tuple[()]] = None) -> "DeadcodeGraphInverseEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"deadcode.GraphNode.7 {{ }}", GraphNode
    return f"deadcode.GraphNode.7 { concatenateFields(key) }", GraphNode

  @staticmethod
  def angle_query(*, type: Optional[Tuple[()]] = None, entities: Optional[Tuple[()]] = None, out_degree: Optional[int] = None, in_degree: Optional[int] = None) -> "DeadcodeGraphNode":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntityByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"deadcode.GraphEntityByFile.7 {{ }}", GraphEntityByFile
    return f"deadcode.GraphEntityByFile.7 { concatenateFields(key) }", GraphEntityByFile

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, graph_entity: Optional[Tuple[()]] = None) -> "DeadcodeGraphEntityByFile":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"deadcode.GraphEntity.7 {{ }}", GraphEntity
    return f"deadcode.GraphEntity.7 { concatenateFields(key) }", GraphEntity

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "DeadcodeGraphEntity":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"deadcode.GraphEdge.7 {{ }}", GraphEdge
    return f"deadcode.GraphEdge.7 { concatenateFields(key) }", GraphEdge

  @staticmethod
  def angle_query(*, _from: Optional[Tuple[()]] = None, to: Optional[Tuple[()]] = None) -> "DeadcodeGraphEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNodeByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"deadcode.GraphNodeByEntity.7 {{ }}", GraphNodeByEntity
    return f"deadcode.GraphNodeByEntity.7 { concatenateFields(key) }", GraphNodeByEntity

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, node: Optional[Tuple[()]] = None) -> "DeadcodeGraphNodeByEntity":
    raise Exception("this function can only be called from @angle_query")


