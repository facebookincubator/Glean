# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
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
)


class OmegaanalyserMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.Method.4 {angle_for(__env, arg)}", Method

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserPolicyOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], policy: ast.Expr, oncall: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.PolicyOncall.4 {{ policy = {angle_for(__env, policy)}, oncall = {angle_for(__env, oncall)} }}", PolicyOncall

  @staticmethod
  def angle_query(*, policy: Optional["OmegaanalyserOmegaPolicy"] = None, oncall: Optional["OmegaanalyserOncallName"] = None) -> "OmegaanalyserPolicyOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.OmegaEndpoint.4 {angle_for(__env, arg)}", OmegaEndpoint

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, coreNode: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.DependencyPath.4 {{ node = {angle_for(__env, node)}, coreNode = {angle_for(__env, coreNode)}, shortestPath = {angle_for(__env, shortestPath)} }}", DependencyPath

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, coreNode: Optional["OmegaanalyserOmegaEndpoint"] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaanalyserDependencyPath":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.Class_.4 {angle_for(__env, arg)}", Class_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClassStaticMethodReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, targetClass: ast.Expr, classXRefUses: ast.Expr, staticMethodXRefUses: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.ClassStaticMethodReferences.4 {{ source = {angle_for(__env, source)}, targetClass = {angle_for(__env, targetClass)}, classXRefUses = {angle_for(__env, classXRefUses)}, staticMethodXRefUses = {angle_for(__env, staticMethodXRefUses)} }}", ClassStaticMethodReferences

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, targetClass: Optional["HackClassDeclaration"] = None, classXRefUses: Optional[Tuple[()]] = None, staticMethodXRefUses: Optional[Tuple[()]] = None) -> "OmegaanalyserClassStaticMethodReferences":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.OmegaPolicy.4 {angle_for(__env, arg)}", OmegaPolicy

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserEndpointOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], endpoint: ast.Expr, oncall: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.EndpointOncall.4 {{ endpoint = {angle_for(__env, endpoint)}, oncall = {angle_for(__env, oncall)} }}", EndpointOncall

  @staticmethod
  def angle_query(*, endpoint: Optional["OmegaanalyserOmegaEndpoint"] = None, oncall: Optional["OmegaanalyserOncallName"] = None) -> "OmegaanalyserEndpointOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr, pathToFile: ast.Expr, targetByteSpan: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.TargetNodeLocations.4 {{ source = {angle_for(__env, source)}, target = {angle_for(__env, target)}, pathToFile = {angle_for(__env, pathToFile)}, targetByteSpan = {angle_for(__env, targetByteSpan)} }}", TargetNodeLocations

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, pathToFile: Optional["SrcFile"] = None, targetByteSpan: Optional[Tuple[()]] = None) -> "OmegaanalyserTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, endpoints: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.DependencyList.4 {{ node = {angle_for(__env, node)}, endpoints = {angle_for(__env, endpoints)} }}", DependencyList

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, endpoints: Optional[Tuple[()]] = None) -> "OmegaanalyserDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.Function_.4 {angle_for(__env, arg)}", Function_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOncallName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.OncallName.4 {angle_for(__env, arg)}", OncallName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOncallName":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.Config.4 {angle_for(__env, arg)}", Config

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPathByCoreNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], coreNode: ast.Expr, node: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    return f"omegaanalyser.DependencyPathByCoreNode.4 {{ coreNode = {angle_for(__env, coreNode)}, node = {angle_for(__env, node)}, shortestPath = {angle_for(__env, shortestPath)} }}", DependencyPathByCoreNode

  @staticmethod
  def angle_query(*, coreNode: Optional["OmegaanalyserOmegaEndpoint"] = None, node: Optional[Tuple[()]] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaanalyserDependencyPathByCoreNode":
    raise Exception("this function can only be called from @angle_query")


