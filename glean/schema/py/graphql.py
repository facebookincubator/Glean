# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
    return f"graphql.Directive.2 {{ name = _, arguments = _ }}", Directive

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None) -> "GraphqlDirective":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InputObjectTypeDef.2 {{ name = _, fields = _, directives = _ }}", InputObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.EnumTypeDef.2 {{ name = _, values = _, directives = _ }}", EnumTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, values: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.UnionTypeDef.2 {{ name = _, types = _, directives = _ }}", UnionTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, types: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Field.2 {{ type = _, name = _, directives = _, selectionSet = _, arguments = _, alias = _, loc = _ }}", Field

  @staticmethod
  def angle_query(*, type: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None, alias: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlField":
    raise Exception("this function can only be called from @angle_query")

class GraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.ObjectTypeDef.2 {{ name = _, interfaces = _, fields = _, directives = _ }}", ObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, interfaces: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Argument.2 {{ name = _, value = _ }}", Argument

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "GraphqlArgument":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.DirectiveDef.2 {{ name = _, argumentDefs = _, locations = _ }}", DirectiveDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, argumentDefs: Optional[Tuple[()]] = None, locations: Optional[Tuple[()]] = None) -> "GraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Fragment.2 {{ name = _, typeCondition = _, variableDefs = _, directives = _, selectionSet = _, loc = _ }}", Fragment

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, typeCondition: Optional[Tuple[()]] = None, variableDefs: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlFragment":
    raise Exception("this function can only be called from @angle_query")

class GraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.ScalarTypeDef.2 {{ name = _, directives = _ }}", ScalarTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.VariableDef.2 {{ name = _, type = _, directives = _, defaultValue = _ }}", VariableDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.DeclarationName.2 {json.dumps(key)}", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.FileDeclarations.2 {{ file = _, span = _, declaration = _ }}", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "GraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.DeclarationLocation.2 {{ declaration = _, file = _, span = _ }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "GraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.FieldDef.2 {{ name = _, type = _, argumentDefs = _, directives = _ }}", FieldDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, argumentDefs: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InterfaceTypeDef.2 {{ name = _, fields = _, directives = _ }}", InterfaceTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Query.2 {{ name = _, directives = _, variableDefs = _, selectionSet = _, loc = _ }}", Query

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, variableDefs: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlQuery":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InputValueDef.2 {{ name = _, type = _, directives = _, defaultValue = _ }}", InputValueDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.Value.2 {json.dumps(key)}", Value

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GraphqlValue":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"graphql.InlineFragment.2 {{ inferredTypeCondition = _, directives = _, selectionSet = _, typeCondition = _ }}", InlineFragment

  @staticmethod
  def angle_query(*, inferredTypeCondition: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, typeCondition: Optional[Tuple[()]] = None) -> "GraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")


