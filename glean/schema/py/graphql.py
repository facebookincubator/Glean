# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GraphqlDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Directive.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlDirective":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InputObjectTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.EnumTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.UnionTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Field.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlField":
    raise Exception("this function can only be called from @angle_query")

class GraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.ObjectTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Argument.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlArgument":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.DirectiveDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Fragment.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlFragment":
    raise Exception("this function can only be called from @angle_query")

class GraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.ScalarTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.VariableDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.DeclarationName.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.FileDeclarations.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.DeclarationLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.FieldDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InterfaceTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Query.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlQuery":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InputValueDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Value.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GraphqlValue":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InlineFragment.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")


