# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
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
    DirectiveDefLocation,
    SelectionSet,
    Declaration,
)


class GraphqlDirective(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, arguments: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, arguments, 'arguments')]))
    return f"graphql.Directive.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Directive

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, arguments: Optional[List["GraphqlArgument"]] = None) -> "GraphqlDirective":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInputObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fields, 'fields'), angle_for(__env, directives, 'directives')]))
    return f"graphql.InputObjectTypeDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", InputObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, fields: Optional[List["GraphqlInputValueDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlInputObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlEnumTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, values: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, values, 'values'), angle_for(__env, directives, 'directives')]))
    return f"graphql.EnumTypeDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EnumTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, values: Optional[List["GraphqlValue"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlEnumTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlUnionTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, types: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, types, 'types'), angle_for(__env, directives, 'directives')]))
    return f"graphql.UnionTypeDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", UnionTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, types: Optional[List["GraphqlValue"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlUnionTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlField(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], type: ast.Expr, name: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, arguments: ast.Expr, alias: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, type, 'type'), angle_for(__env, name, 'name'), angle_for(__env, directives, 'directives'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, arguments, 'arguments'), angle_for(__env, alias, 'alias'), angle_for(__env, loc, 'loc')]))
    return f"graphql.Field.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Field

  @staticmethod
  def angle_query(*, type: Optional["GraphqlValue"] = None, name: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, selectionSet: Optional["GraphqlSelectionSet"] = None, arguments: Optional[List["GraphqlArgument"]] = None, alias: Optional[Union[Just["GraphqlValue"], Just[None]]] = None, loc: Optional["SrcFileLocation"] = None) -> "GraphqlField":
    raise Exception("this function can only be called from @angle_query")



class GraphqlObjectTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, interfaces: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, interfaces, 'interfaces'), angle_for(__env, fields, 'fields'), angle_for(__env, directives, 'directives')]))
    return f"graphql.ObjectTypeDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ObjectTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, interfaces: Optional[List["GraphqlValue"]] = None, fields: Optional[List["GraphqlFieldDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlObjectTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlArgument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, value, 'value')]))
    return f"graphql.Argument.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Argument

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, value: Optional["GraphqlValue"] = None) -> "GraphqlArgument":
    raise Exception("this function can only be called from @angle_query")



class GraphqlDirectiveDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, argumentDefs: ast.Expr, locations: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, argumentDefs, 'argumentDefs'), angle_for(__env, locations, 'locations')]))
    return f"graphql.DirectiveDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DirectiveDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, argumentDefs: Optional[List["GraphqlInputValueDef"]] = None, locations: Optional[List["GraphqlDirectiveDefLocation"]] = None) -> "GraphqlDirectiveDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, typeCondition: ast.Expr, variableDefs: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, typeCondition, 'typeCondition'), angle_for(__env, variableDefs, 'variableDefs'), angle_for(__env, directives, 'directives'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, loc, 'loc')]))
    return f"graphql.Fragment.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Fragment

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, typeCondition: Optional["GraphqlValue"] = None, variableDefs: Optional[List["GraphqlVariableDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None, selectionSet: Optional["GraphqlSelectionSet"] = None, loc: Optional["SrcFileLocation"] = None) -> "GraphqlFragment":
    raise Exception("this function can only be called from @angle_query")



class GraphqlScalarTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, directives, 'directives')]))
    return f"graphql.ScalarTypeDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ScalarTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlScalarTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlVariableDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, directives: ast.Expr, defaultValue: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, directives, 'directives'), angle_for(__env, defaultValue, 'defaultValue')]))
    return f"graphql.VariableDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", VariableDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, defaultValue: Optional[Union[Just["GraphqlValue"], Just[None]]] = None) -> "GraphqlVariableDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlDeclarationName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"graphql.DeclarationName.2 { query_fields if query_fields else '_' }", DeclarationName

  @staticmethod
  def angle_query(*, arg: Optional["GraphqlDeclaration"] = None) -> "GraphqlDeclarationName":
    raise Exception("this function can only be called from @angle_query")



class GraphqlFileDeclarations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, declaration: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, declaration, 'declaration')]))
    return f"graphql.FileDeclarations.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileDeclarations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None, declaration: Optional["GraphqlDeclaration"] = None) -> "GraphqlFileDeclarations":
    raise Exception("this function can only be called from @angle_query")



class GraphqlDeclarationLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], declaration: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, declaration, 'declaration'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"graphql.DeclarationLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DeclarationLocation

  @staticmethod
  def angle_query(*, declaration: Optional["GraphqlDeclaration"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "GraphqlDeclarationLocation":
    raise Exception("this function can only be called from @angle_query")



class GraphqlFieldDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, argumentDefs: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, argumentDefs, 'argumentDefs'), angle_for(__env, directives, 'directives')]))
    return f"graphql.FieldDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FieldDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, argumentDefs: Optional[List["GraphqlInputValueDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlFieldDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInterfaceTypeDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, fields: ast.Expr, directives: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, fields, 'fields'), angle_for(__env, directives, 'directives')]))
    return f"graphql.InterfaceTypeDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", InterfaceTypeDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, fields: Optional[List["GraphqlFieldDef"]] = None, directives: Optional[List["GraphqlDirective"]] = None) -> "GraphqlInterfaceTypeDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlQuery(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, directives: ast.Expr, variableDefs: ast.Expr, selectionSet: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, directives, 'directives'), angle_for(__env, variableDefs, 'variableDefs'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, loc, 'loc')]))
    return f"graphql.Query.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Query

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, variableDefs: Optional[List["GraphqlVariableDef"]] = None, selectionSet: Optional["GraphqlSelectionSet"] = None, loc: Optional["SrcFileLocation"] = None) -> "GraphqlQuery":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInputValueDef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr, directives: ast.Expr, defaultValue: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type'), angle_for(__env, directives, 'directives'), angle_for(__env, defaultValue, 'defaultValue')]))
    return f"graphql.InputValueDef.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", InputValueDef

  @staticmethod
  def angle_query(*, name: Optional["GraphqlValue"] = None, type: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, defaultValue: Optional[Union[Just["GraphqlValue"], Just[None]]] = None) -> "GraphqlInputValueDef":
    raise Exception("this function can only be called from @angle_query")



class GraphqlValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"graphql.Value.2 { query_fields if query_fields else '_' }", Value

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GraphqlValue":
    raise Exception("this function can only be called from @angle_query")



class GraphqlInlineFragment(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], inferredTypeCondition: ast.Expr, directives: ast.Expr, selectionSet: ast.Expr, typeCondition: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, inferredTypeCondition, 'inferredTypeCondition'), angle_for(__env, directives, 'directives'), angle_for(__env, selectionSet, 'selectionSet'), angle_for(__env, typeCondition, 'typeCondition')]))
    return f"graphql.InlineFragment.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", InlineFragment

  @staticmethod
  def angle_query(*, inferredTypeCondition: Optional["GraphqlValue"] = None, directives: Optional[List["GraphqlDirective"]] = None, selectionSet: Optional["GraphqlSelectionSet"] = None, typeCondition: Optional[Union[Just["GraphqlValue"], Just[None]]] = None) -> "GraphqlInlineFragment":
    raise Exception("this function can only be called from @angle_query")





class GraphqlDirectiveDefLocation(Enum):
  QUERY = 0
  MUTATION = 1
  SUBSCRIPTION = 2
  FIELD = 3
  FRAGMENT_DEFINITION = 4
  FRAGMENT_SPREAD = 5
  INLINE_FRAGMENT = 6
  SCHEMA = 7
  SCALAR = 8
  OBJECT = 9
  FIELD_DEFINITION = 10
  ARGUMENT_DEFINITION = 11
  INTERFACE = 12
  UNION = 13
  ENUM = 14
  ENUM_VALUE = 15
  INPUT_OBJECT = 16
  INPUT_FIELD_DEFINITION = 17

class GraphqlSelectionSet(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fields: ast.Expr, inlineFragments: ast.Expr, fragmentSpreads: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, fields, 'fields'), angle_for(__env, inlineFragments, 'inlineFragments'), angle_for(__env, fragmentSpreads, 'fragmentSpreads')]))
    return f"graphql.SelectionSet.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SelectionSet

  @staticmethod
  def angle_query(*, fields: Optional[List["GraphqlField"]] = None, inlineFragments: Optional[List["GraphqlInlineFragment"]] = None, fragmentSpreads: Optional[List["GraphqlValue"]] = None) -> "GraphqlSelectionSet":
    raise Exception("this function can only be called from @angle_query")



class GraphqlDeclaration(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query_: ast.Expr, fragment_: ast.Expr, field_: ast.Expr, enum_: ast.Expr, directive_: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, query_, 'query_'), angle_for(__env, fragment_, 'fragment_'), angle_for(__env, field_, 'field_'), angle_for(__env, enum_, 'enum_'), angle_for(__env, directive_, 'directive_')]))
    return f"graphql.Declaration.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Declaration

  @staticmethod
  def angle_query_query_(*, query_: Optional["GraphqlQuery"] = None) -> "GraphqlDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_fragment_(*, fragment_: Optional["GraphqlFragment"] = None) -> "GraphqlDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_field_(*, field_: Optional["GraphqlFieldDef"] = None) -> "GraphqlDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_enum_(*, enum_: Optional["GraphqlEnumTypeDef"] = None) -> "GraphqlDeclaration":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_directive_(*, directive_: Optional["GraphqlDirectiveDef"] = None) -> "GraphqlDeclaration":
    raise Exception("this function can only be called from @angle_query")





