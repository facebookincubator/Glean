# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.code import *
from glean.schema.py.codemarkuptypes import *
from glean.schema.py.src import *


from glean.schema.codemarkup.types import (
    FileEntityKinds,
    FileEntityXRefKinds,
    EntityAnnotations,
    SearchRelatedEntities,
    EntityVisibility,
    FileEntityInfos,
    FileEntityXRefInfos,
    EntityLocation,
    ExtendsChildEntity,
    ContainsParentEntity,
    EntityToAnnotations,
    EntityInfo,
    ExtendsParentEntity,
    ContainsChildEntity,
    FileEntityXRefLocations,
    FileEntityXRefSpans,
    EntityUses,
    FileEntityLocations,
    EntityReferences,
    EntityKind,
    ResolveLocation,
    RelationType,
    ParentEntity,
    ChildEntity,
)


class CodemarkupFileEntityKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')]))
    return f"codemarkup.FileEntityKinds.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityKinds

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, entity: Optional["CodeEntity"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupFileEntityKinds":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFileEntityXRefKinds(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')]))
    return f"codemarkup.FileEntityXRefKinds.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityXRefKinds

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, entity: Optional["CodeEntity"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupFileEntityXRefKinds":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"codemarkup.EntityAnnotations.30 { query_fields if query_fields else '_' }", EntityAnnotations

  @staticmethod
  def angle_query(*, arg: Optional["CodeEntity"] = None) -> "CodemarkupEntityAnnotations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupSearchRelatedEntities(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], query: ast.Expr, parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, query, 'query'), angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')]))
    return f"codemarkup.SearchRelatedEntities.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchRelatedEntities

  @staticmethod
  def angle_query(*, query: Optional["CodemarkupRelationType"] = None, parent: Optional["CodemarkupParentEntity"] = None, child: Optional["CodemarkupChildEntity"] = None) -> "CodemarkupSearchRelatedEntities":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityVisibility(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"codemarkup.EntityVisibility.30 { query_fields if query_fields else '_' }", EntityVisibility

  @staticmethod
  def angle_query(*, arg: Optional["CodeEntity"] = None) -> "CodemarkupEntityVisibility":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFileEntityInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')]))
    return f"codemarkup.FileEntityInfos.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityInfos

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, entity: Optional["CodeEntity"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupFileEntityInfos":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFileEntityXRefInfos(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')]))
    return f"codemarkup.FileEntityXRefInfos.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityXRefInfos

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, entity: Optional["CodeEntity"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupFileEntityXRefInfos":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"codemarkup.EntityLocation.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupExtendsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')]))
    return f"codemarkup.ExtendsChildEntity.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ExtendsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional["CodeEntity"] = None, child: Optional["CodeEntity"] = None) -> "CodemarkupExtendsChildEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupContainsParentEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, parent, 'parent')]))
    return f"codemarkup.ContainsParentEntity.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContainsParentEntity

  @staticmethod
  def angle_query(*, child: Optional["CodeEntity"] = None, parent: Optional["CodeEntity"] = None) -> "CodemarkupContainsParentEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityToAnnotations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, annotations: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, annotations, 'annotations')]))
    return f"codemarkup.EntityToAnnotations.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityToAnnotations

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, annotations: Optional["CodeAnnotations"] = None) -> "CodemarkupEntityToAnnotations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityInfo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, info: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, info, 'info')]))
    return f"codemarkup.EntityInfo.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityInfo

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, info: Optional["CodemarkupTypesSymbolInfo"] = None) -> "CodemarkupEntityInfo":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupExtendsParentEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, parent, 'parent')]))
    return f"codemarkup.ExtendsParentEntity.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ExtendsParentEntity

  @staticmethod
  def angle_query(*, child: Optional["CodeEntity"] = None, parent: Optional["CodeEntity"] = None) -> "CodemarkupExtendsParentEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupContainsChildEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')]))
    return f"codemarkup.ContainsChildEntity.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContainsChildEntity

  @staticmethod
  def angle_query(*, parent: Optional["CodeEntity"] = None, child: Optional["CodeEntity"] = None) -> "CodemarkupContainsChildEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFileEntityXRefLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xref: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xref, 'xref'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.FileEntityXRefLocations.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityXRefLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xref: Optional["CodemarkupTypesXRefLocation"] = None, entity: Optional["CodeEntity"] = None) -> "CodemarkupFileEntityXRefLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFileEntityXRefSpans(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.FileEntityXRefSpans.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityXRefSpans

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None, entity: Optional["CodeEntity"] = None) -> "CodemarkupFileEntityXRefSpans":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"codemarkup.EntityUses.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityUses

  @staticmethod
  def angle_query(*, target: Optional["CodeEntity"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "CodemarkupEntityUses":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupFileEntityLocations(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.FileEntityLocations.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityLocations

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeEntity"] = None) -> "CodemarkupFileEntityLocations":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityReferences(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')]))
    return f"codemarkup.EntityReferences.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityReferences

  @staticmethod
  def angle_query(*, target: Optional["CodeEntity"] = None, file: Optional["SrcFile"] = None, range: Optional["CodemarkupTypesRangeSpan"] = None) -> "CodemarkupEntityReferences":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')]))
    return f"codemarkup.EntityKind.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityKind

  @staticmethod
  def angle_query(*, entity: Optional["CodeEntity"] = None, kind: Optional["CodemarkupTypesSymbolKind"] = None) -> "CodemarkupEntityKind":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"codemarkup.ResolveLocation.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["CodemarkupTypesLocation"] = None, entity: Optional["CodeEntity"] = None) -> "CodemarkupResolveLocation":
    raise Exception("this function can only be called from @angle_query")





class CodemarkupRelationType(Enum):
  ExtendsParentOfChild = 0
  ExtendsChildOfParent = 1
  ContainsChildOfParent = 2
  ContainsParentOfChild = 3

class CodemarkupParentEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, location, 'location')]))
    return f"codemarkup.ParentEntity.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ParentEntity

  @staticmethod
  def angle_query(*, parent: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupParentEntity":
    raise Exception("this function can only be called from @angle_query")



class CodemarkupChildEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, location, 'location')]))
    return f"codemarkup.ChildEntity.30 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ChildEntity

  @staticmethod
  def angle_query(*, child: Optional["CodeEntity"] = None, location: Optional["CodemarkupTypesLocation"] = None) -> "CodemarkupChildEntity":
    raise Exception("this function can only be called from @angle_query")




