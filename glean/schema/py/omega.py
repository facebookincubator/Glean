# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
  def angle_query(*, node: Optional[Tuple[()]] = None, entities: Optional[Tuple[()]] = None) -> "OmegaDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaEnum_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Enum_.1 {{ name = _ }}", Enum_

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "OmegaEnum_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaAction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaAction.1 {{ class_ = _ }}", OmegaAction

  @staticmethod
  def angle_query(*, class_: Optional[Tuple[()]] = None) -> "OmegaOmegaAction":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaPolicy.1 {{ class_ = _ }}", OmegaPolicy

  @staticmethod
  def angle_query(*, class_: Optional[Tuple[()]] = None) -> "OmegaOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaBlock(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaBlock.1 {{ class_ = _ }}", OmegaBlock

  @staticmethod
  def angle_query(*, class_: Optional[Tuple[()]] = None) -> "OmegaOmegaBlock":
    raise Exception("this function can only be called from @angle_query")

class OmegaOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Oncall.1 {{ name = _ }}", Oncall

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "OmegaOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Config.1 {{ name = _ }}", Config

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "OmegaConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaEndpoint.1 {{ class_ = _ }}", OmegaEndpoint

  @staticmethod
  def angle_query(*, class_: Optional[Tuple[()]] = None) -> "OmegaOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPathByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.DependencyPathByEntity.1 {{ entity = _, node = _, shortestPath = _ }}", DependencyPathByEntity

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, node: Optional[Tuple[()]] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaDependencyPathByEntity":
    raise Exception("this function can only be called from @angle_query")

class OmegaFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Function_.1 {{ name = _ }}", Function_

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "OmegaFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.TargetNodeLocations.1 {{ source = _, target = _, pathToFile = _, targetByteSpan = _ }}", TargetNodeLocations

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, pathToFile: Optional[Tuple[()]] = None, targetByteSpan: Optional[Tuple[()]] = None) -> "OmegaTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Name.1 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaName":
    raise Exception("this function can only be called from @angle_query")

class OmegaMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Method.1 {{ name = _ }}", Method

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "OmegaMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEntityMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaEntityMetadata.1 {{ entity = _, oncall = _, isAbstract = _, isICE = _ }}", OmegaEntityMetadata

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, oncall: Optional[Tuple[()]] = None, isAbstract: Optional[bool] = None, isICE: Optional[bool] = None) -> "OmegaOmegaEntityMetadata":
    raise Exception("this function can only be called from @angle_query")

class OmegaClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.Class_.1 {{ name = _ }}", Class_

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None) -> "OmegaClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaExecutionNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.OmegaExecutionNode.1 {{ class_ = _ }}", OmegaExecutionNode

  @staticmethod
  def angle_query(*, class_: Optional[Tuple[()]] = None) -> "OmegaOmegaExecutionNode":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"omega.DependencyPath.1 {{ node = _, entity = _, shortestPath = _ }}", DependencyPath

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaDependencyPath":
    raise Exception("this function can only be called from @angle_query")


