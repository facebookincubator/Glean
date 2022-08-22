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
    return f"omega.DependencyList.1 {{ node = _, entities = _ }}", DependencyList

  @staticmethod
  def angle_query(*, node: Tuple[()], entities: Tuple[()]) -> "OmegaDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaEnum_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Enum_.1 {{ name = _ }}", Enum_

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaEnum_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaAction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaAction.1 {{ class_ = _ }}", OmegaAction

  @staticmethod
  def angle_query(*, class_: Tuple[()]) -> "OmegaOmegaAction":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaPolicy.1 {{ class_ = _ }}", OmegaPolicy

  @staticmethod
  def angle_query(*, class_: Tuple[()]) -> "OmegaOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaBlock(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaBlock.1 {{ class_ = _ }}", OmegaBlock

  @staticmethod
  def angle_query(*, class_: Tuple[()]) -> "OmegaOmegaBlock":
    raise Exception("this function can only be called from @angle_query")

class OmegaOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Oncall.1 {{ name = _ }}", Oncall

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Config.1 {{ name = _ }}", Config

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaEndpoint.1 {{ class_ = _ }}", OmegaEndpoint

  @staticmethod
  def angle_query(*, class_: Tuple[()]) -> "OmegaOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPathByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.DependencyPathByEntity.1 {{ entity = _, node = _, shortestPath = _ }}", DependencyPathByEntity

  @staticmethod
  def angle_query(*, entity: Tuple[()], node: Tuple[()], shortestPath: Tuple[()]) -> "OmegaDependencyPathByEntity":
    raise Exception("this function can only be called from @angle_query")

class OmegaFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Function_.1 {{ name = _ }}", Function_

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.TargetNodeLocations.1 {{ source = _, target = _, pathToFile = _, targetByteSpan = _ }}", TargetNodeLocations

  @staticmethod
  def angle_query(*, source: Tuple[()], target: Tuple[()], pathToFile: Tuple[()], targetByteSpan: Tuple[()]) -> "OmegaTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Name.1 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: str) -> "OmegaName":
    raise Exception("this function can only be called from @angle_query")

class OmegaMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Method.1 {{ name = _ }}", Method

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEntityMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaEntityMetadata.1 {{ entity = _, oncall = _, isAbstract = _, isICE = _ }}", OmegaEntityMetadata

  @staticmethod
  def angle_query(*, entity: Tuple[()], oncall: Tuple[()], isAbstract: bool, isICE: bool) -> "OmegaOmegaEntityMetadata":
    raise Exception("this function can only be called from @angle_query")

class OmegaClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Class_.1 {{ name = _ }}", Class_

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "OmegaClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaExecutionNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaExecutionNode.1 {{ class_ = _ }}", OmegaExecutionNode

  @staticmethod
  def angle_query(*, class_: Tuple[()]) -> "OmegaOmegaExecutionNode":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.DependencyPath.1 {{ node = _, entity = _, shortestPath = _ }}", DependencyPath

  @staticmethod
  def angle_query(*, node: Tuple[()], entity: Tuple[()], shortestPath: Tuple[()]) -> "OmegaDependencyPath":
    raise Exception("this function can only be called from @angle_query")


