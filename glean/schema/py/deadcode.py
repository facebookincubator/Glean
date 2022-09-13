# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.code import *
from glean.schema.py.src import *


from glean.schema.deadcode.types import (
    GraphInverseEdge,
    GraphNode,
    GraphEntityByFile,
    GraphEntity,
    GraphEdge,
    GraphNodeByEntity,
    GraphType,
)


class DeadcodeGraphInverseEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], to: ast.Expr, _from: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphInverseEdge.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, to, 'to'), angle_for(__env, _from, 'from')])) or '_' } }}", GraphInverseEdge

  @staticmethod
  def angle_query(*, to: Optional["DeadcodeGraphNode"] = None, _from: Optional["DeadcodeGraphNode"] = None) -> "DeadcodeGraphInverseEdge":
    raise Exception("this function can only be called from @angle_query")



class DeadcodeGraphNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, entities: ast.Expr, out_degree: ast.Expr, in_degree: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphNode.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, entities, 'entities'), angle_for(__env, out_degree, 'out_degree'), angle_for(__env, in_degree, 'in_degree')])) or '_' } }}", GraphNode

  @staticmethod
  def angle_query(*, type: Optional["DeadcodeGraphType"] = None, entities: Optional[List["DeadcodeGraphEntity"]] = None, out_degree: Optional[int] = None, in_degree: Optional[int] = None) -> "DeadcodeGraphNode":
    raise Exception("this function can only be called from @angle_query")



class DeadcodeGraphEntityByFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, graph_entity: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphEntityByFile.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, graph_entity, 'graph_entity')])) or '_' } }}", GraphEntityByFile

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, graph_entity: Optional["DeadcodeGraphEntity"] = None) -> "DeadcodeGraphEntityByFile":
    raise Exception("this function can only be called from @angle_query")



class DeadcodeGraphEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphEntity.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", GraphEntity

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "DeadcodeGraphEntity":
    raise Exception("this function can only be called from @angle_query")



class DeadcodeGraphEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], _from: ast.Expr, to: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphEdge.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, _from, 'from'), angle_for(__env, to, 'to')])) or '_' } }}", GraphEdge

  @staticmethod
  def angle_query(*, _from: Optional["DeadcodeGraphNode"] = None, to: Optional["DeadcodeGraphNode"] = None) -> "DeadcodeGraphEdge":
    raise Exception("this function can only be called from @angle_query")



class DeadcodeGraphNodeByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, node: ast.Expr) -> Tuple[str, Struct]:
    return f"deadcode.GraphNodeByEntity.7 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, node, 'node')])) or '_' } }}", GraphNodeByEntity

  @staticmethod
  def angle_query(*, entity: Optional["DeadcodeGraphEntity"] = None, node: Optional["DeadcodeGraphNode"] = None) -> "DeadcodeGraphNodeByEntity":
    raise Exception("this function can only be called from @angle_query")





class DeadcodeGraphType(Enum):
  Raw = 0
  Condensed = 1


