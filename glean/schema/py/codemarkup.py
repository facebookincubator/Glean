# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.codemarkup.types import (
    FileEntityKinds,
    FileEntityXRefKinds,
    EntityAnnotations,
    SearchRelatedEntities,
    EntityVisibility,
    FileEntityInfos,
    FileEntityXRefInfos,
    EntityLocation,
    ExtendsChildEntity,
    EntityToAnnotations,
    EntityInfo,
    ExtendsParentEntity,
    ContainsChildEntity,
    FileEntityXRefLocations,
    FileEntityXRefSpans,
    EntityUses,
    FileEntityLocations,
    EntityReferences,
    EntityKind,
    ResolveLocation,
)


class CodemarkupFileEntityKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.FileEntityKinds.30 {{ file = _, entity = _, kind = _ }}", FileEntityKinds

  @staticmethod
  def angle_query(*, file: Tuple[()], entity: Tuple[()], kind: Tuple[()]) -> "CodemarkupFileEntityKinds":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.FileEntityXRefKinds.30 {{ file = _, entity = _, kind = _ }}", FileEntityXRefKinds

  @staticmethod
  def angle_query(*, file: Tuple[()], entity: Tuple[()], kind: Tuple[()]) -> "CodemarkupFileEntityXRefKinds":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityAnnotations.30 {json.dumps(key)}", EntityAnnotations

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "CodemarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchRelatedEntities(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.SearchRelatedEntities.30 {{ query = _, parent = _, child = _ }}", SearchRelatedEntities

  @staticmethod
  def angle_query(*, query: Tuple[()], parent: Tuple[()], child: Tuple[()]) -> "CodemarkupSearchRelatedEntities":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityVisibility.30 {json.dumps(key)}", EntityVisibility

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "CodemarkupEntityVisibility":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.FileEntityInfos.30 {{ file = _, entity = _, info = _ }}", FileEntityInfos

  @staticmethod
  def angle_query(*, file: Tuple[()], entity: Tuple[()], info: Tuple[()]) -> "CodemarkupFileEntityInfos":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.FileEntityXRefInfos.30 {{ file = _, entity = _, info = _ }}", FileEntityXRefInfos

  @staticmethod
  def angle_query(*, file: Tuple[()], entity: Tuple[()], info: Tuple[()]) -> "CodemarkupFileEntityXRefInfos":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityLocation.30 {{ entity = _, location = _ }}", EntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "CodemarkupEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupExtendsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.ExtendsChildEntity.30 {{ parent = _, child = _ }}", ExtendsChildEntity

  @staticmethod
  def angle_query(*, parent: Tuple[()], child: Tuple[()]) -> "CodemarkupExtendsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityToAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityToAnnotations.30 {{ entity = _, annotations = _ }}", EntityToAnnotations

  @staticmethod
  def angle_query(*, entity: Tuple[()], annotations: Tuple[()]) -> "CodemarkupEntityToAnnotations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityInfo.30 {{ entity = _, info = _ }}", EntityInfo

  @staticmethod
  def angle_query(*, entity: Tuple[()], info: Tuple[()]) -> "CodemarkupEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupExtendsParentEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.ExtendsParentEntity.30 {{ child = _, parent = _ }}", ExtendsParentEntity

  @staticmethod
  def angle_query(*, child: Tuple[()], parent: Tuple[()]) -> "CodemarkupExtendsParentEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.ContainsChildEntity.30 {{ parent = _, child = _ }}", ContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Tuple[()], child: Tuple[()]) -> "CodemarkupContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.FileEntityXRefLocations.30 {{ file = _, xref = _, entity = _ }}", FileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], xref: Tuple[()], entity: Tuple[()]) -> "CodemarkupFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefSpans(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.FileEntityXRefSpans.30 {{ file = _, span = _, entity = _ }}", FileEntityXRefSpans

  @staticmethod
  def angle_query(*, file: Tuple[()], span: Tuple[()], entity: Tuple[()]) -> "CodemarkupFileEntityXRefSpans":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityUses.30 {{ target = _, file = _, span = _ }}", EntityUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "CodemarkupEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.FileEntityLocations.30 {{ file = _, location = _, entity = _ }}", FileEntityLocations

  @staticmethod
  def angle_query(*, file: Tuple[()], location: Tuple[()], entity: Tuple[()]) -> "CodemarkupFileEntityLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityReferences.30 {{ target = _, file = _, range = _ }}", EntityReferences

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], range: Tuple[()]) -> "CodemarkupEntityReferences":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.EntityKind.30 {{ entity = _, kind = _ }}", EntityKind

  @staticmethod
  def angle_query(*, entity: Tuple[()], kind: Tuple[()]) -> "CodemarkupEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"codemarkup.ResolveLocation.30 {{ location = _, entity = _ }}", ResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "CodemarkupResolveLocation":
    raise Exception("this function can only be called from @angle_query")


