# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.Method.4 { json.dumps(key) }", Method

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserPolicyOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.PolicyOncall.4 { { } }", PolicyOncall

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserPolicyOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.OmegaEndpoint.4 { json.dumps(key) }", OmegaEndpoint

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.DependencyPath.4 { { } }", DependencyPath

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserDependencyPath":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.Class_.4 { json.dumps(key) }", Class_

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClassStaticMethodReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.ClassStaticMethodReferences.4 { { } }", ClassStaticMethodReferences

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserClassStaticMethodReferences":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.OmegaPolicy.4 { json.dumps(key) }", OmegaPolicy

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserEndpointOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.EndpointOncall.4 { { } }", EndpointOncall

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserEndpointOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.TargetNodeLocations.4 { { } }", TargetNodeLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.DependencyList.4 { { } }", DependencyList

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.Function_.4 { json.dumps(key) }", Function_

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOncallName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.OncallName.4 { json.dumps(key) }", OncallName

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserOncallName":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.Config.4 { json.dumps(key) }", Config

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPathByCoreNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omegaanalyser.DependencyPathByCoreNode.4 { { } }", DependencyPathByCoreNode

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserDependencyPathByCoreNode":
    raise Exception("this function can only be called from @angle_query")


