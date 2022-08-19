# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
)


class OmegaDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.DependencyList.1 { { } }", DependencyList

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaEnum_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Enum_.1 { { } }", Enum_

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaEnum_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaAction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaAction.1 { { } }", OmegaAction

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOmegaAction":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaPolicy.1 { { } }", OmegaPolicy

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaBlock(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaBlock.1 { { } }", OmegaBlock

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOmegaBlock":
    raise Exception("this function can only be called from @angle_query")

class OmegaOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Oncall.1 { { } }", Oncall

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Config.1 { { } }", Config

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaEndpoint.1 { { } }", OmegaEndpoint

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPathByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.DependencyPathByEntity.1 { { } }", DependencyPathByEntity

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaDependencyPathByEntity":
    raise Exception("this function can only be called from @angle_query")

class OmegaFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Function_.1 { { } }", Function_

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.TargetNodeLocations.1 { { } }", TargetNodeLocations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Name.1 { json.dumps(key) }", Name

  @staticmethod
  def angle_query(*, name: str) -> "OmegaName":
    raise Exception("this function can only be called from @angle_query")

class OmegaMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Method.1 { { } }", Method

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEntityMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaEntityMetadata.1 { { } }", OmegaEntityMetadata

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOmegaEntityMetadata":
    raise Exception("this function can only be called from @angle_query")

class OmegaClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Class_.1 { { } }", Class_

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaExecutionNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaExecutionNode.1 { { } }", OmegaExecutionNode

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOmegaExecutionNode":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.DependencyPath.1 { { } }", DependencyPath

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaDependencyPath":
    raise Exception("this function can only be called from @angle_query")


