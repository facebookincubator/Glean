# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSOmegaanalyserMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Method.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSOmegaanalyserMethod":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserPolicyOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.PolicyOncall.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSOmegaanalyserPolicyOncall":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.OmegaEndpoint.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSOmegaanalyserOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.DependencyPath.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSOmegaanalyserDependencyPath":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Class_.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSOmegaanalyserClass_":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserClassStaticMethodReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.ClassStaticMethodReferences.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSOmegaanalyserClassStaticMethodReferences":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.OmegaPolicy.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSOmegaanalyserOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserEndpointOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.EndpointOncall.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSOmegaanalyserEndpointOncall":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.TargetNodeLocations.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSOmegaanalyserTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.DependencyList.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSOmegaanalyserDependencyList":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Function_.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSOmegaanalyserFunction_":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserOncallName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.OncallName.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSOmegaanalyserOncallName":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Config.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSOmegaanalyserConfig":
    raise Exception("this function can only be called from @angle_query")

class GSOmegaanalyserDependencyPathByCoreNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.DependencyPathByCoreNode.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSOmegaanalyserDependencyPathByCoreNode":
    raise Exception("this function can only be called from @angle_query")


