# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSCodemarkupFileEntityKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityKinds.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupFileEntityKinds":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFileEntityXRefKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefKinds.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupFileEntityXRefKinds":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityAnnotations.30 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupSearchRelatedEntities(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.SearchRelatedEntities.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupSearchRelatedEntities":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupEntityVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityVisibility.30 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSCodemarkupEntityVisibility":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFileEntityInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityInfos.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupFileEntityInfos":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFileEntityXRefInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefInfos.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupFileEntityXRefInfos":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityLocation.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupExtendsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ExtendsChildEntity.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupExtendsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityInfo.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupExtendsParentEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ExtendsParentEntity.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupExtendsParentEntity":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ContainsChildEntity.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefLocations.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFileEntityXRefSpans(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefSpans.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupFileEntityXRefSpans":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityUses.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupEntityUses":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupFileEntityLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityLocations.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupFileEntityLocations":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupEntityReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityReferences.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupEntityReferences":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityKind.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupEntityKind":
    raise Exception("this function can only be called from @angle_query")

class GSCodemarkupResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ResolveLocation.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSCodemarkupResolveLocation":
    raise Exception("this function can only be called from @angle_query")


