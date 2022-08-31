# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


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
  def build_angle(__env: Dict[str, R], name: ast.Expr, arguments: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Directive.2 {{ name = {angle_for(__env, name)}, arguments = {angle_for(__env, arguments)} }}", Directive

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, arguments: Optional[Tuple[()]] = None) -> "GraphqlDirective":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InputObjectTypeDef.2 {{ name = {angle_for(__env, name)}, fields = {angle_for(__env, fields)}, directives = {angle_for(__env, directives)} }}", InputObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, values: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.EnumTypeDef.2 {{ name = {angle_for(__env, name)}, values = {angle_for(__env, values)}, directives = {angle_for(__env, directives)} }}", EnumTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, values: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, types: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.UnionTypeDef.2 {{ name = {angle_for(__env, name)}, types = {angle_for(__env, types)}, directives = {angle_for(__env, directives)} }}", UnionTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, types: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, name: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, arguments: ast.Expr, alias: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Field.2 {{ type = {angle_for(__env, type)}, name = {angle_for(__env, name)}, directives = {angle_for(__env, directives)}, selectionSet = {angle_for(__env, selectionSet)}, arguments = {angle_for(__env, arguments)}, alias = {angle_for(__env, alias)}, loc = {angle_for(__env, loc)} }}", Field

  @staticmethod
  def angle_query(*, type: Optional["GraphqlValue"] = None, name: Optional["GraphqlValue"] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, arguments: Optional[Tuple[()]] = None, alias: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlField":
    raise Exception("this function can only be called from @angle_query")

class GraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, interfaces: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.ObjectTypeDef.2 {{ name = {angle_for(__env, name)}, interfaces = {angle_for(__env, interfaces)}, fields = {angle_for(__env, fields)}, directives = {angle_for(__env, directives)} }}", ObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, interfaces: Optional[Tuple[()]] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Argument.2 {{ name = {angle_for(__env, name)}, value = {angle_for(__env, value)} }}", Argument

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, value: Optional["GraphqlValue"] = None) -> "GraphqlArgument":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, argumentDefs: ast.Expr, locations: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.DirectiveDef.2 {{ name = {angle_for(__env, name)}, argumentDefs = {angle_for(__env, argumentDefs)}, locations = {angle_for(__env, locations)} }}", DirectiveDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, argumentDefs: Optional[Tuple[()]] = None, locations: Optional[Tuple[()]] = None) -> "GraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, typeCondition: ast.Expr, variableDefs: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Fragment.2 {{ name = {angle_for(__env, name)}, typeCondition = {angle_for(__env, typeCondition)}, variableDefs = {angle_for(__env, variableDefs)}, directives = {angle_for(__env, directives)}, selectionSet = {angle_for(__env, selectionSet)}, loc = {angle_for(__env, loc)} }}", Fragment

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, typeCondition: Optional["GraphqlValue"] = None, variableDefs: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlFragment":
    raise Exception("this function can only be called from @angle_query")

class GraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.ScalarTypeDef.2 {{ name = {angle_for(__env, name)}, directives = {angle_for(__env, directives)} }}", ScalarTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, directives: ast.Expr, defaultValue: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.VariableDef.2 {{ name = {angle_for(__env, name)}, type = {angle_for(__env, type)}, directives = {angle_for(__env, directives)}, defaultValue = {angle_for(__env, defaultValue)} }}", VariableDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, directives: Optional[Tuple[()]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.DeclarationName.2 {angle_for(__env, arg)}", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.FileDeclarations.2 {{ file = {angle_for(__env, file)}, span = {angle_for(__env, span)}, declaration = {angle_for(__env, declaration)} }}", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "GraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")

class GraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.DeclarationLocation.2 {{ declaration = {angle_for(__env, declaration)}, file = {angle_for(__env, file)}, span = {angle_for(__env, span)} }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "GraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")

class GraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, argumentDefs: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.FieldDef.2 {{ name = {angle_for(__env, name)}, type = {angle_for(__env, type)}, argumentDefs = {angle_for(__env, argumentDefs)}, directives = {angle_for(__env, directives)} }}", FieldDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, argumentDefs: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InterfaceTypeDef.2 {{ name = {angle_for(__env, name)}, fields = {angle_for(__env, fields)}, directives = {angle_for(__env, directives)} }}", InterfaceTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, fields: Optional[Tuple[()]] = None, directives: Optional[Tuple[()]] = None) -> "GraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, directives: ast.Expr, variableDefs: ast.Expr, selectionSet: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Query.2 {{ name = {angle_for(__env, name)}, directives = {angle_for(__env, directives)}, variableDefs = {angle_for(__env, variableDefs)}, selectionSet = {angle_for(__env, selectionSet)}, loc = {angle_for(__env, loc)} }}", Query

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, directives: Optional[Tuple[()]] = None, variableDefs: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlQuery":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, directives: ast.Expr, defaultValue: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InputValueDef.2 {{ name = {angle_for(__env, name)}, type = {angle_for(__env, type)}, directives = {angle_for(__env, directives)}, defaultValue = {angle_for(__env, defaultValue)} }}", InputValueDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, directives: Optional[Tuple[()]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")

class GraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Value.2 {angle_for(__env, arg)}", Value

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GraphqlValue":
    raise Exception("this function can only be called from @angle_query")

class GraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], inferredTypeCondition: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, typeCondition: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InlineFragment.2 {{ inferredTypeCondition = {angle_for(__env, inferredTypeCondition)}, directives = {angle_for(__env, directives)}, selectionSet = {angle_for(__env, selectionSet)}, typeCondition = {angle_for(__env, typeCondition)} }}", InlineFragment

  @staticmethod
  def angle_query(*, inferredTypeCondition: Optional["GraphqlValue"] = None, directives: Optional[Tuple[()]] = None, selectionSet: Optional[Tuple[()]] = None, typeCondition: Optional[Tuple[()]] = None) -> "GraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")


