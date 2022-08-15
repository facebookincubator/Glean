# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class OmegaanalyserMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Method.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserPolicyOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.PolicyOncall.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserPolicyOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.OmegaEndpoint.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.DependencyPath.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserDependencyPath":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Class_.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserClassStaticMethodReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.ClassStaticMethodReferences.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserClassStaticMethodReferences":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.OmegaPolicy.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserEndpointOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.EndpointOncall.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserEndpointOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.TargetNodeLocations.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.DependencyList.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Function_.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserOncallName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.OncallName.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserOncallName":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.Config.4 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "OmegaanalyserConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaanalyserDependencyPathByCoreNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"omegaanalyser.DependencyPathByCoreNode.4 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaanalyserDependencyPathByCoreNode":
    raise Exception("this function can only be called from @angle_query")


