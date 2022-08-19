# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.graphql.types import (
    Directive,
    InputObjectTypeDef,
    EnumTypeDef,
    UnionTypeDef,
    Field,
    ObjectTypeDef,
    Argument,
    DirectiveDef,
    Fragment,
    ScalarTypeDef,
    VariableDef,
    DeclarationName,
    FileDeclarations,
    DeclarationLocation,
    FieldDef,
    InterfaceTypeDef,
    Query,
    InputValueDef,
    Value,
    InlineFragment,
)


class GraphqlDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Directive.2 { { } }", Directive

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlDirective":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InputObjectTypeDef.2 { { } }", InputObjectTypeDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.EnumTypeDef.2 { { } }", EnumTypeDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.UnionTypeDef.2 { { } }", UnionTypeDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Field.2 { { } }", Field

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlField":
    raise Exception("this function can only be called from @angle_query")

class GraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.ObjectTypeDef.2 { { } }", ObjectTypeDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Argument.2 { { } }", Argument

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlArgument":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.DirectiveDef.2 { { } }", DirectiveDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Fragment.2 { { } }", Fragment

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlFragment":
    raise Exception("this function can only be called from @angle_query")

class GraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.ScalarTypeDef.2 { { } }", ScalarTypeDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.VariableDef.2 { { } }", VariableDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.DeclarationName.2 { json.dumps(key) }", DeclarationName

  @staticmethod
  def angle_query(*, name: str) -> "GraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.FileDeclarations.2 { { } }", FileDeclarations

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.DeclarationLocation.2 { { } }", DeclarationLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.FieldDef.2 { { } }", FieldDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InterfaceTypeDef.2 { { } }", InterfaceTypeDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Query.2 { { } }", Query

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlQuery":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InputValueDef.2 { { } }", InputValueDef

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Value.2 { json.dumps(key) }", Value

  @staticmethod
  def angle_query(*, name: str) -> "GraphqlValue":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InlineFragment.2 { { } }", InlineFragment

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")


