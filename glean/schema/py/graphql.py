# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSGraphqlDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.Directive.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlDirective":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.InputObjectTypeDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.EnumTypeDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.UnionTypeDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.Field.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlField":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.ObjectTypeDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.Argument.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlArgument":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.DirectiveDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.Fragment.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlFragment":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.ScalarTypeDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.VariableDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.DeclarationName.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.FileDeclarations.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.DeclarationLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.FieldDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.InterfaceTypeDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.Query.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlQuery":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.InputValueDef.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.Value.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlValue":
    raise Exception("this function can only be called from @angle_query")

class GSGraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"graphql.InlineFragment.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")


