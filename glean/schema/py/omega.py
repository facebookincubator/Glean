# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.hack import *
from glean.schema.py.src import *


from glean.schema.omega.types import (
    DependencyList,
    Enum_,
    OmegaAction,
    OmegaPolicy,
    OmegaBlock,
    Oncall,
    Config,
    OmegaEndpoint,
    DependencyPathByEntity,
    Function_,
    TargetNodeLocations,
    Name,
    Method,
    OmegaEntityMetadata,
    Class_,
    OmegaExecutionNode,
    DependencyPath,
    OmegaEntity,
    GraphNode,
)


class OmegaDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, entities: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, node, 'node'), angle_for(__env, entities, 'entities')]))
    return f"omega.DependencyList.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DependencyList

  @staticmethod
  def angle_query(*, node: Optional["OmegaGraphNode"] = None, entities: Optional[List["OmegaOmegaEntity"]] = None) -> "OmegaDependencyList":
    raise Exception("this function can only be called from @angle_query")



class OmegaEnum_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"omega.Enum_.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Enum_

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaEnum_":
    raise Exception("this function can only be called from @angle_query")



class OmegaOmegaAction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_')]))
    return f"omega.OmegaAction.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OmegaAction

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaAction":
    raise Exception("this function can only be called from @angle_query")



class OmegaOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_')]))
    return f"omega.OmegaPolicy.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OmegaPolicy

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")



class OmegaOmegaBlock(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_')]))
    return f"omega.OmegaBlock.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OmegaBlock

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaBlock":
    raise Exception("this function can only be called from @angle_query")



class OmegaOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"omega.Oncall.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Oncall

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaOncall":
    raise Exception("this function can only be called from @angle_query")



class OmegaConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"omega.Config.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Config

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaConfig":
    raise Exception("this function can only be called from @angle_query")



class OmegaOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_')]))
    return f"omega.OmegaEndpoint.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OmegaEndpoint

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")



class OmegaDependencyPathByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, node: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, node, 'node'), angle_for(__env, shortestPath, 'shortestPath')]))
    return f"omega.DependencyPathByEntity.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DependencyPathByEntity

  @staticmethod
  def angle_query(*, entity: Optional["OmegaOmegaEntity"] = None, node: Optional["OmegaGraphNode"] = None, shortestPath: Optional[List["OmegaGraphNode"]] = None) -> "OmegaDependencyPathByEntity":
    raise Exception("this function can only be called from @angle_query")



class OmegaFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"omega.Function_.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Function_

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaFunction_":
    raise Exception("this function can only be called from @angle_query")



class OmegaTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr, pathToFile: ast.Expr, targetByteSpan: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, target, 'target'), angle_for(__env, pathToFile, 'pathToFile'), angle_for(__env, targetByteSpan, 'targetByteSpan')]))
    return f"omega.TargetNodeLocations.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetNodeLocations

  @staticmethod
  def angle_query(*, source: Optional["HackDeclaration"] = None, target: Optional["HackDeclaration"] = None, pathToFile: Optional["SrcFile"] = None, targetByteSpan: Optional["SrcByteSpan"] = None) -> "OmegaTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")



class OmegaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omega.Name.1 { query_fields if query_fields else '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaName":
    raise Exception("this function can only be called from @angle_query")



class OmegaMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"omega.Method.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Method

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaMethod":
    raise Exception("this function can only be called from @angle_query")



class OmegaOmegaEntityMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, oncall: ast.Expr, isAbstract: ast.Expr, isICE: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, oncall, 'oncall'), angle_for(__env, isAbstract, 'isAbstract'), angle_for(__env, isICE, 'isICE')]))
    return f"omega.OmegaEntityMetadata.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OmegaEntityMetadata

  @staticmethod
  def angle_query(*, entity: Optional["OmegaOmegaEntity"] = None, oncall: Optional[Union[Just["OmegaOncall"], Just[None]]] = None, isAbstract: Optional[bool] = None, isICE: Optional[bool] = None) -> "OmegaOmegaEntityMetadata":
    raise Exception("this function can only be called from @angle_query")



class OmegaClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name')]))
    return f"omega.Class_.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Class_

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaClass_":
    raise Exception("this function can only be called from @angle_query")



class OmegaOmegaExecutionNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_')]))
    return f"omega.OmegaExecutionNode.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OmegaExecutionNode

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaExecutionNode":
    raise Exception("this function can only be called from @angle_query")



class OmegaDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, entity: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, node, 'node'), angle_for(__env, entity, 'entity'), angle_for(__env, shortestPath, 'shortestPath')]))
    return f"omega.DependencyPath.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DependencyPath

  @staticmethod
  def angle_query(*, node: Optional["OmegaGraphNode"] = None, entity: Optional["OmegaOmegaEntity"] = None, shortestPath: Optional[List["OmegaGraphNode"]] = None) -> "OmegaDependencyPath":
    raise Exception("this function can only be called from @angle_query")





class OmegaOmegaEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], endpoint: ast.Expr, policy: ast.Expr, action: ast.Expr, block: ast.Expr, executionNode: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, endpoint, 'endpoint'), angle_for(__env, policy, 'policy'), angle_for(__env, action, 'action'), angle_for(__env, block, 'block'), angle_for(__env, executionNode, 'executionNode')]))
    return f"omega.OmegaEntity.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OmegaEntity

  @staticmethod
  def angle_query_endpoint(*, endpoint: "OmegaOmegaEndpoint") -> "OmegaOmegaEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_policy(*, policy: "OmegaOmegaPolicy") -> "OmegaOmegaEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_action(*, action: "OmegaOmegaAction") -> "OmegaOmegaEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_block(*, block: "OmegaOmegaBlock") -> "OmegaOmegaEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_executionNode(*, executionNode: "OmegaOmegaExecutionNode") -> "OmegaOmegaEntity":
    raise Exception("this function can only be called from @angle_query")




class OmegaGraphNode(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, function_: ast.Expr, method: ast.Expr, enum_: ast.Expr, config: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_'), angle_for(__env, function_, 'function_'), angle_for(__env, method, 'method'), angle_for(__env, enum_, 'enum_'), angle_for(__env, config, 'config')]))
    return f"omega.GraphNode.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", GraphNode

  @staticmethod
  def angle_query_class_(*, class_: "OmegaClass_") -> "OmegaGraphNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_function_(*, function_: "OmegaFunction_") -> "OmegaGraphNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_method(*, method: "OmegaMethod") -> "OmegaGraphNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enum_(*, enum_: "OmegaEnum_") -> "OmegaGraphNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_config(*, config: "OmegaConfig") -> "OmegaGraphNode":
    raise Exception("this function can only be called from @angle_query")





