# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.hack import *
from glean.schema.py.src import *


from glean.schema.omegaanalyser.types import (
    Method,
    PolicyOncall,
    OmegaEndpoint,
    DependencyPath,
    Class_,
    ClassStaticMethodReferences,
    OmegaPolicy,
    EndpointOncall,
    TargetNodeLocations,
    DependencyList,
    Function_,
    OncallName,
    Config,
    DependencyPathByCoreNode,
    Node,
)


class OmegaanalyserMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omegaanalyser.Method.4 { query_fields if query_fields else '_' }", Method

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserMethod":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserPolicyOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], policy: ast.Expr, oncall: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, policy, 'policy'), angle_for(__env, oncall, 'oncall')]))
    return f"omegaanalyser.PolicyOncall.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PolicyOncall

  @staticmethod
  def angle_query(*, policy: Optional["OmegaanalyserOmegaPolicy"] = None, oncall: Optional["OmegaanalyserOncallName"] = None) -> "OmegaanalyserPolicyOncall":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omegaanalyser.OmegaEndpoint.4 { query_fields if query_fields else '_' }", OmegaEndpoint

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, coreNode: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, node, 'node'), angle_for(__env, coreNode, 'coreNode'), angle_for(__env, shortestPath, 'shortestPath')]))
    return f"omegaanalyser.DependencyPath.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DependencyPath

  @staticmethod
  def angle_query(*, node: Optional["OmegaanalyserNode"] = None, coreNode: Optional["OmegaanalyserOmegaEndpoint"] = None, shortestPath: Optional[List["OmegaanalyserNode"]] = None) -> "OmegaanalyserDependencyPath":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omegaanalyser.Class_.4 { query_fields if query_fields else '_' }", Class_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserClass_":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserClassStaticMethodReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, targetClass: ast.Expr, classXRefUses: ast.Expr, staticMethodXRefUses: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, targetClass, 'targetClass'), angle_for(__env, classXRefUses, 'classXRefUses'), angle_for(__env, staticMethodXRefUses, 'staticMethodXRefUses')]))
    return f"omegaanalyser.ClassStaticMethodReferences.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ClassStaticMethodReferences

  @staticmethod
  def angle_query(*, source: Optional["HackDeclaration"] = None, targetClass: Optional["HackClassDeclaration"] = None, classXRefUses: Optional["SrcByteSpans"] = None, staticMethodXRefUses: Optional["SrcByteSpans"] = None) -> "OmegaanalyserClassStaticMethodReferences":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omegaanalyser.OmegaPolicy.4 { query_fields if query_fields else '_' }", OmegaPolicy

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserEndpointOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], endpoint: ast.Expr, oncall: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, endpoint, 'endpoint'), angle_for(__env, oncall, 'oncall')]))
    return f"omegaanalyser.EndpointOncall.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EndpointOncall

  @staticmethod
  def angle_query(*, endpoint: Optional["OmegaanalyserOmegaEndpoint"] = None, oncall: Optional["OmegaanalyserOncallName"] = None) -> "OmegaanalyserEndpointOncall":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr, pathToFile: ast.Expr, targetByteSpan: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, target, 'target'), angle_for(__env, pathToFile, 'pathToFile'), angle_for(__env, targetByteSpan, 'targetByteSpan')]))
    return f"omegaanalyser.TargetNodeLocations.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetNodeLocations

  @staticmethod
  def angle_query(*, source: Optional["HackDeclaration"] = None, target: Optional["HackDeclaration"] = None, pathToFile: Optional["SrcFile"] = None, targetByteSpan: Optional["SrcByteSpan"] = None) -> "OmegaanalyserTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, endpoints: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, node, 'node'), angle_for(__env, endpoints, 'endpoints')]))
    return f"omegaanalyser.DependencyList.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DependencyList

  @staticmethod
  def angle_query(*, node: Optional["OmegaanalyserNode"] = None, endpoints: Optional[List["OmegaanalyserOmegaEndpoint"]] = None) -> "OmegaanalyserDependencyList":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omegaanalyser.Function_.4 { query_fields if query_fields else '_' }", Function_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserFunction_":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserOncallName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omegaanalyser.OncallName.4 { query_fields if query_fields else '_' }", OncallName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOncallName":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"omegaanalyser.Config.4 { query_fields if query_fields else '_' }", Config

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserConfig":
    raise Exception("this function can only be called from @angle_query")



class OmegaanalyserDependencyPathByCoreNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], coreNode: ast.Expr, node: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, coreNode, 'coreNode'), angle_for(__env, node, 'node'), angle_for(__env, shortestPath, 'shortestPath')]))
    return f"omegaanalyser.DependencyPathByCoreNode.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DependencyPathByCoreNode

  @staticmethod
  def angle_query(*, coreNode: Optional["OmegaanalyserOmegaEndpoint"] = None, node: Optional["OmegaanalyserNode"] = None, shortestPath: Optional[List["OmegaanalyserNode"]] = None) -> "OmegaanalyserDependencyPathByCoreNode":
    raise Exception("this function can only be called from @angle_query")





class OmegaanalyserNode(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr, method: ast.Expr, function_: ast.Expr, config: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, class_, 'class_'), angle_for(__env, method, 'method'), angle_for(__env, function_, 'function_'), angle_for(__env, config, 'config')]))
    return f"omegaanalyser.Node.4 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Node

  @staticmethod
  def angle_query_class_(*, class_: Optional["OmegaanalyserClass_"] = None) -> "OmegaanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_method(*, method: Optional["OmegaanalyserMethod"] = None) -> "OmegaanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_function_(*, function_: Optional["OmegaanalyserFunction_"] = None) -> "OmegaanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_config(*, config: Optional["OmegaanalyserConfig"] = None) -> "OmegaanalyserNode":
    raise Exception("this function can only be called from @angle_query")





