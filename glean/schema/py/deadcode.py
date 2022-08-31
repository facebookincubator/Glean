# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], to: ast.Expr, _from: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphInverseEdge.7 {{ to = {angle_for(__env, to)}, from = {angle_for(__env, _from)} }}", GraphInverseEdge

  @staticmethod
  def angle_query(*, to: Optional["DeadcodeGraphNode"] = None, _from: Optional["DeadcodeGraphNode"] = None) -> "DeadcodeGraphInverseEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, entities: ast.Expr, out_degree: ast.Expr, in_degree: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphNode.7 {{ type = {angle_for(__env, type)}, entities = {angle_for(__env, entities)}, out_degree = {angle_for(__env, out_degree)}, in_degree = {angle_for(__env, in_degree)} }}", GraphNode

  @staticmethod
  def angle_query(*, type: Optional[Tuple[()]] = None, entities: Optional[Tuple[()]] = None, out_degree: Optional[int] = None, in_degree: Optional[int] = None) -> "DeadcodeGraphNode":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntityByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, graph_entity: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphEntityByFile.7 {{ file = {angle_for(__env, file)}, graph_entity = {angle_for(__env, graph_entity)} }}", GraphEntityByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, graph_entity: Optional["DeadcodeGraphEntity"] = None) -> "DeadcodeGraphEntityByFile":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphEntity.7 {{ entity = {angle_for(__env, entity)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", GraphEntity

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "DeadcodeGraphEntity":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], _from: ast.Expr, to: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphEdge.7 {{ from = {angle_for(__env, _from)}, to = {angle_for(__env, to)} }}", GraphEdge

  @staticmethod
  def angle_query(*, _from: Optional["DeadcodeGraphNode"] = None, to: Optional["DeadcodeGraphNode"] = None) -> "DeadcodeGraphEdge":
    raise Exception("this function can only be called from @angle_query")

class DeadcodeGraphNodeByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, node: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphNodeByEntity.7 {{ entity = {angle_for(__env, entity)}, node = {angle_for(__env, node)} }}", GraphNodeByEntity

  @staticmethod
  def angle_query(*, entity: Optional["DeadcodeGraphEntity"] = None, node: Optional["DeadcodeGraphNode"] = None) -> "DeadcodeGraphNodeByEntity":
    raise Exception("this function can only be called from @angle_query")


