# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSGraphqlDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Directive.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlDirective":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InputObjectTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.EnumTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.UnionTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Field.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlField":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.ObjectTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Argument.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlArgument":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.DirectiveDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Fragment.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlFragment":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.ScalarTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.VariableDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.DeclarationName.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.FileDeclarations.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.DeclarationLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.FieldDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InterfaceTypeDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Query.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlQuery":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InputValueDef.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.Value.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlValue":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"graphql.InlineFragment.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSGraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")


