# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class CodemarkupFileEntityKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityKinds.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFileEntityKinds":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefKinds.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFileEntityXRefKinds":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityAnnotations.30 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "CodemarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupSearchRelatedEntities(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.SearchRelatedEntities.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupSearchRelatedEntities":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityVisibility.30 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "CodemarkupEntityVisibility":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityInfos.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFileEntityInfos":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefInfos.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFileEntityXRefInfos":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityLocation.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupExtendsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ExtendsChildEntity.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupExtendsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityInfo.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupEntityInfo":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupExtendsParentEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ExtendsParentEntity.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupExtendsParentEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ContainsChildEntity.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefLocations.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityXRefSpans(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityXRefSpans.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFileEntityXRefSpans":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityUses.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupEntityUses":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupFileEntityLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.FileEntityLocations.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupFileEntityLocations":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityReferences.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupEntityReferences":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.EntityKind.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupEntityKind":
    raise Exception("this function can only be called from @angle_query")

class CodemarkupResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"codemarkup.ResolveLocation.30 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "CodemarkupResolveLocation":
    raise Exception("this function can only be called from @angle_query")


