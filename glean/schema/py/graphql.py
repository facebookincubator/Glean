# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.Directive.2 {{ }}", Directive
    return f"graphql.Directive.2 { concatenateFields(key) }", Directive

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None) -> "GraphqlDirective":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.InputObjectTypeDef.2 {{ }}", InputObjectTypeDef
    return f"graphql.InputObjectTypeDef.2 { concatenateFields(key) }", InputObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.EnumTypeDef.2 {{ }}", EnumTypeDef
    return f"graphql.EnumTypeDef.2 { concatenateFields(key) }", EnumTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, values: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.UnionTypeDef.2 {{ }}", UnionTypeDef
    return f"graphql.UnionTypeDef.2 { concatenateFields(key) }", UnionTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, types: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.Field.2 {{ }}", Field
    return f"graphql.Field.2 { concatenateFields(key) }", Field

  @staticmethod
  def angle_query(*, type: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None, alias: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlField":
    raise Exception("this function can only be called from @angle_query")

class GraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.ObjectTypeDef.2 {{ }}", ObjectTypeDef
    return f"graphql.ObjectTypeDef.2 { concatenateFields(key) }", ObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, interfaces: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.Argument.2 {{ }}", Argument
    return f"graphql.Argument.2 { concatenateFields(key) }", Argument

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "GraphqlArgument":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.DirectiveDef.2 {{ }}", DirectiveDef
    return f"graphql.DirectiveDef.2 { concatenateFields(key) }", DirectiveDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, argumentDefs: Optional[Tuple[()]] = None, locations: Optional[Tuple[()]] = None) -> "GraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.Fragment.2 {{ }}", Fragment
    return f"graphql.Fragment.2 { concatenateFields(key) }", Fragment

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, typeCondition: Optional[Tuple[()]] = None, variableDefs: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlFragment":
    raise Exception("this function can only be called from @angle_query")

class GraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.ScalarTypeDef.2 {{ }}", ScalarTypeDef
    return f"graphql.ScalarTypeDef.2 { concatenateFields(key) }", ScalarTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.VariableDef.2 {{ }}", VariableDef
    return f"graphql.VariableDef.2 { concatenateFields(key) }", VariableDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.DeclarationName.2 {{ }}", DeclarationName
    return f"graphql.DeclarationName.2 {key}", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.FileDeclarations.2 {{ }}", FileDeclarations
    return f"graphql.FileDeclarations.2 { concatenateFields(key) }", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "GraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.DeclarationLocation.2 {{ }}", DeclarationLocation
    return f"graphql.DeclarationLocation.2 { concatenateFields(key) }", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "GraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.FieldDef.2 {{ }}", FieldDef
    return f"graphql.FieldDef.2 { concatenateFields(key) }", FieldDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, argumentDefs: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.InterfaceTypeDef.2 {{ }}", InterfaceTypeDef
    return f"graphql.InterfaceTypeDef.2 { concatenateFields(key) }", InterfaceTypeDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.Query.2 {{ }}", Query
    return f"graphql.Query.2 { concatenateFields(key) }", Query

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, variableDefs: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlQuery":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.InputValueDef.2 {{ }}", InputValueDef
    return f"graphql.InputValueDef.2 { concatenateFields(key) }", InputValueDef

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, type: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.Value.2 {{ }}", Value
    return f"graphql.Value.2 {key}", Value

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GraphqlValue":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"graphql.InlineFragment.2 {{ }}", InlineFragment
    return f"graphql.InlineFragment.2 { concatenateFields(key) }", InlineFragment

  @staticmethod
  def angle_query(*, inferredTypeCondition: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, typeCondition: Optional[Tuple[()]] = None) -> "GraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")


