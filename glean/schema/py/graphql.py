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
    return f"graphql.Directive.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, arguments, 'arguments')])) or '_' } }}", Directive

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, arguments: Optional[List["GraphqlArgument"]] = None) -> "GraphqlDirective":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InputObjectTypeDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fields, 'fields'), angle_for(__env, directives, 'directives')])) or '_' } }}", InputObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, fields: Optional[List["GraphqlInputValueDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, values: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.EnumTypeDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, values, 'values'), angle_for(__env, directives, 'directives')])) or '_' } }}", EnumTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, values: Optional[List["GraphqlValue"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, types: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.UnionTypeDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, types, 'types'), angle_for(__env, directives, 'directives')])) or '_' } }}", UnionTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, types: Optional[List["GraphqlValue"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, name: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, arguments: ast.Expr, alias: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Field.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, name, 'name'), angle_for(__env, directives, 'directives'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, arguments, 'arguments'), angle_for(__env, alias, 'alias'), angle_for(__env, loc, 'loc')])) or '_' } }}", Field

  @staticmethod
  def angle_query(*, type: Optional["GraphqlValue"] = None, name: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, selectionSet: Optional[Tuple[()]] = None, arguments: Optional[List["GraphqlArgument"]] = None, alias: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlField":
    raise Exception("this function can only be called from @angle_query")



class GraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, interfaces: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.ObjectTypeDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, interfaces, 'interfaces'), angle_for(__env, fields, 'fields'), angle_for(__env, directives, 'directives')])) or '_' } }}", ObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, interfaces: Optional[List["GraphqlValue"]] = None, fields: Optional[List["GraphqlFieldDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Argument.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, value, 'value')])) or '_' } }}", Argument

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, value: Optional["GraphqlValue"] = None) -> "GraphqlArgument":
    raise Exception("this function can only be called from @angle_query")



class GraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, argumentDefs: ast.Expr, locations: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.DirectiveDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, argumentDefs, 'argumentDefs'), angle_for(__env, locations, 'locations')])) or '_' } }}", DirectiveDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, argumentDefs: Optional[List["GraphqlInputValueDef"]] = None, locations: Optional[List[Tuple[()]]] = None) -> "GraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, typeCondition: ast.Expr, variableDefs: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Fragment.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, typeCondition, 'typeCondition'), angle_for(__env, variableDefs, 'variableDefs'), angle_for(__env, directives, 'directives'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, loc, 'loc')])) or '_' } }}", Fragment

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, typeCondition: Optional["GraphqlValue"] = None, variableDefs: Optional[List["GraphqlVariableDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlFragment":
    raise Exception("this function can only be called from @angle_query")



class GraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.ScalarTypeDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, directives, 'directives')])) or '_' } }}", ScalarTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, directives: ast.Expr, defaultValue: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.VariableDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, directives, 'directives'), angle_for(__env, defaultValue, 'defaultValue')])) or '_' } }}", VariableDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.DeclarationName.2 { angle_for(__env, arg, None) or '_' }", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")



class GraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.FileDeclarations.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, declaration, 'declaration')])) or '_' } }}", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, declaration: Optional[Tuple[()]] = None) -> "GraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")



class GraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.DeclarationLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')])) or '_' } }}", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None) -> "GraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")



class GraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, argumentDefs: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.FieldDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, argumentDefs, 'argumentDefs'), angle_for(__env, directives, 'directives')])) or '_' } }}", FieldDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, argumentDefs: Optional[List["GraphqlInputValueDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InterfaceTypeDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fields, 'fields'), angle_for(__env, directives, 'directives')])) or '_' } }}", InterfaceTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, fields: Optional[List["GraphqlFieldDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, directives: ast.Expr, variableDefs: ast.Expr, selectionSet: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Query.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, directives, 'directives'), angle_for(__env, variableDefs, 'variableDefs'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, loc, 'loc')])) or '_' } }}", Query

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, variableDefs: Optional[List["GraphqlVariableDef"]] = None, selectionSet: Optional[Tuple[()]] = None, loc: Optional[Tuple[()]] = None) -> "GraphqlQuery":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, directives: ast.Expr, defaultValue: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InputValueDef.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, directives, 'directives'), angle_for(__env, defaultValue, 'defaultValue')])) or '_' } }}", InputValueDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, defaultValue: Optional[Tuple[()]] = None) -> "GraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.Value.2 { angle_for(__env, arg, None) or '_' }", Value

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GraphqlValue":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], inferredTypeCondition: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, typeCondition: ast.Expr) -> Tuple[str, Struct]:
    return f"graphql.InlineFragment.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, inferredTypeCondition, 'inferredTypeCondition'), angle_for(__env, directives, 'directives'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, typeCondition, 'typeCondition')])) or '_' } }}", InlineFragment

  @staticmethod
  def angle_query(*, inferredTypeCondition: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, selectionSet: Optional[Tuple[()]] = None, typeCondition: Optional[Tuple[()]] = None) -> "GraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")




