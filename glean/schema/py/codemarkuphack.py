# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just
from glean.schema.py.src import *


from glean.schema.codemarkuphack.types import (
    hackHackEntityInfo,
    hackHackEntityLocation,
    hackHackVisibility,
    hackHackAnnotation,
    hackHackResolveLocation,
    hackHackContainsChildEntity,
    hackHackFileEntityXRefLocations,
    hackHackEntityUses,
    hackHackEntityKind,
    hackHackFileEntityXRefSpans,
)


class CodemarkupHackHackEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityInfo.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')])) or '_' } }}", hackHackEntityInfo

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, info: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", hackHackEntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, visibility: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackVisibility.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, visibility, 'visibility')])) or '_' } }}", hackHackVisibility

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, visibility: Optional[Tuple[()]] = None) -> "CodemarkupHackHackVisibility":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackAnnotation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, anns: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackAnnotation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, anns, 'anns')])) or '_' } }}", hackHackAnnotation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, anns: Optional[Tuple[()]] = None) -> "CodemarkupHackHackAnnotation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackResolveLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", hackHackResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHackHackResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackContainsChildEntity.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')])) or '_' } }}", hackHackContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "CodemarkupHackHackContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackFileEntityXRefLocations.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')])) or '_' } }}", hackHackFileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHackHackFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityUses.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", hackHackEntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackEntityKind.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')])) or '_' } }}", hackHackEntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "CodemarkupHackHackEntityKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupHackHackFileEntityXRefSpans(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"codemarkup.hack.HackFileEntityXRefSpans.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, entity, 'entity')])) or '_' } }}", hackHackFileEntityXRefSpans

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "CodemarkupHackHackFileEntityXRefSpans":
    raise Exception("this function can only be called from @angle_query")




