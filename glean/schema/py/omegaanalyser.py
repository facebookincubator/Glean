# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.Method.4 {{ }}", Method
    return f"omegaanalyser.Method.4 {key}", Method

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserPolicyOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.PolicyOncall.4 {{ }}", PolicyOncall
    return f"omegaanalyser.PolicyOncall.4 { concatenateFields(key) }", PolicyOncall

  @staticmethod
  def angle_query(*, policy: Optional[Tuple[()]] = None, oncall: Optional[Tuple[()]] = None) -> "OmegaanalyserPolicyOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.OmegaEndpoint.4 {{ }}", OmegaEndpoint
    return f"omegaanalyser.OmegaEndpoint.4 {key}", OmegaEndpoint

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.DependencyPath.4 {{ }}", DependencyPath
    return f"omegaanalyser.DependencyPath.4 { concatenateFields(key) }", DependencyPath

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, coreNode: Optional[Tuple[()]] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaanalyserDependencyPath":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.Class_.4 {{ }}", Class_
    return f"omegaanalyser.Class_.4 {key}", Class_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClassStaticMethodReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.ClassStaticMethodReferences.4 {{ }}", ClassStaticMethodReferences
    return f"omegaanalyser.ClassStaticMethodReferences.4 { concatenateFields(key) }", ClassStaticMethodReferences

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, targetClass: Optional[Tuple[()]] = None, classXRefUses: Optional[Tuple[()]] = None, staticMethodXRefUses: Optional[Tuple[()]] = None) -> "OmegaanalyserClassStaticMethodReferences":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.OmegaPolicy.4 {{ }}", OmegaPolicy
    return f"omegaanalyser.OmegaPolicy.4 {key}", OmegaPolicy

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserEndpointOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.EndpointOncall.4 {{ }}", EndpointOncall
    return f"omegaanalyser.EndpointOncall.4 { concatenateFields(key) }", EndpointOncall

  @staticmethod
  def angle_query(*, endpoint: Optional[Tuple[()]] = None, oncall: Optional[Tuple[()]] = None) -> "OmegaanalyserEndpointOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.TargetNodeLocations.4 {{ }}", TargetNodeLocations
    return f"omegaanalyser.TargetNodeLocations.4 { concatenateFields(key) }", TargetNodeLocations

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, pathToFile: Optional[Tuple[()]] = None, targetByteSpan: Optional[Tuple[()]] = None) -> "OmegaanalyserTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.DependencyList.4 {{ }}", DependencyList
    return f"omegaanalyser.DependencyList.4 { concatenateFields(key) }", DependencyList

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, endpoints: Optional[Tuple[()]] = None) -> "OmegaanalyserDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.Function_.4 {{ }}", Function_
    return f"omegaanalyser.Function_.4 {key}", Function_

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOncallName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.OncallName.4 {{ }}", OncallName
    return f"omegaanalyser.OncallName.4 {key}", OncallName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserOncallName":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.Config.4 {{ }}", Config
    return f"omegaanalyser.Config.4 {key}", Config

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaanalyserConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPathByCoreNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"omegaanalyser.DependencyPathByCoreNode.4 {{ }}", DependencyPathByCoreNode
    return f"omegaanalyser.DependencyPathByCoreNode.4 { concatenateFields(key) }", DependencyPathByCoreNode

  @staticmethod
  def angle_query(*, coreNode: Optional[Tuple[()]] = None, node: Optional[Tuple[()]] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaanalyserDependencyPathByCoreNode":
    raise Exception("this function can only be called from @angle_query")


