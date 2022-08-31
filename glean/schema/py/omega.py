# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
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
)


class OmegaDependencyList(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, entities: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.DependencyList.1 {{ node = {angle_for(__env, node)}, entities = {angle_for(__env, entities)} }}", DependencyList

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, entities: Optional[Tuple[()]] = None) -> "OmegaDependencyList":
    raise Exception("this function can only be called from @angle_query")

class OmegaEnum_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.Enum_.1 {{ name = {angle_for(__env, name)} }}", Enum_

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaEnum_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaAction(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.OmegaAction.1 {{ class_ = {angle_for(__env, class_)} }}", OmegaAction

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaAction":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.OmegaPolicy.1 {{ class_ = {angle_for(__env, class_)} }}", OmegaPolicy

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaPolicy":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaBlock(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.OmegaBlock.1 {{ class_ = {angle_for(__env, class_)} }}", OmegaBlock

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaBlock":
    raise Exception("this function can only be called from @angle_query")

class OmegaOncall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.Oncall.1 {{ name = {angle_for(__env, name)} }}", Oncall

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaOncall":
    raise Exception("this function can only be called from @angle_query")

class OmegaConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.Config.1 {{ name = {angle_for(__env, name)} }}", Config

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaConfig":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.OmegaEndpoint.1 {{ class_ = {angle_for(__env, class_)} }}", OmegaEndpoint

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaEndpoint":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPathByEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, node: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.DependencyPathByEntity.1 {{ entity = {angle_for(__env, entity)}, node = {angle_for(__env, node)}, shortestPath = {angle_for(__env, shortestPath)} }}", DependencyPathByEntity

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, node: Optional[Tuple[()]] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaDependencyPathByEntity":
    raise Exception("this function can only be called from @angle_query")

class OmegaFunction_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.Function_.1 {{ name = {angle_for(__env, name)} }}", Function_

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaFunction_":
    raise Exception("this function can only be called from @angle_query")

class OmegaTargetNodeLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, target: ast.Expr, pathToFile: ast.Expr, targetByteSpan: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.TargetNodeLocations.1 {{ source = {angle_for(__env, source)}, target = {angle_for(__env, target)}, pathToFile = {angle_for(__env, pathToFile)}, targetByteSpan = {angle_for(__env, targetByteSpan)} }}", TargetNodeLocations

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, pathToFile: Optional["SrcFile"] = None, targetByteSpan: Optional[Tuple[()]] = None) -> "OmegaTargetNodeLocations":
    raise Exception("this function can only be called from @angle_query")

class OmegaName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.Name.1 {angle_for(__env, arg)}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "OmegaName":
    raise Exception("this function can only be called from @angle_query")

class OmegaMethod(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.Method.1 {{ name = {angle_for(__env, name)} }}", Method

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaMethod":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaEntityMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, oncall: ast.Expr, isAbstract: ast.Expr, isICE: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.OmegaEntityMetadata.1 {{ entity = {angle_for(__env, entity)}, oncall = {angle_for(__env, oncall)}, isAbstract = {angle_for(__env, isAbstract)}, isICE = {angle_for(__env, isICE)} }}", OmegaEntityMetadata

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, oncall: Optional[Tuple[()]] = None, isAbstract: Optional[bool] = None, isICE: Optional[bool] = None) -> "OmegaOmegaEntityMetadata":
    raise Exception("this function can only be called from @angle_query")

class OmegaClass_(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.Class_.1 {{ name = {angle_for(__env, name)} }}", Class_

  @staticmethod
  def angle_query(*, name: Optional["OmegaName"] = None) -> "OmegaClass_":
    raise Exception("this function can only be called from @angle_query")

class OmegaOmegaExecutionNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], class_: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.OmegaExecutionNode.1 {{ class_ = {angle_for(__env, class_)} }}", OmegaExecutionNode

  @staticmethod
  def angle_query(*, class_: Optional["OmegaClass_"] = None) -> "OmegaOmegaExecutionNode":
    raise Exception("this function can only be called from @angle_query")

class OmegaDependencyPath(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, entity: ast.Expr, shortestPath: ast.Expr) -> Tuple[str, Struct]:
    return f"omega.DependencyPath.1 {{ node = {angle_for(__env, node)}, entity = {angle_for(__env, entity)}, shortestPath = {angle_for(__env, shortestPath)} }}", DependencyPath

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None, shortestPath: Optional[Tuple[()]] = None) -> "OmegaDependencyPath":
    raise Exception("this function can only be called from @angle_query")


