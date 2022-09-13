# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.src import *


from glean.schema.lsif.types import (
    Range,
    MonikerSymbolKind,
    Name,
    Document,
    ToSrcRange,
    PackageInformation,
    Definition,
    Project,
    DefinitionKind,
    SearchByMoniker,
    DefinitionMoniker,
    MonikerScheme,
    MonikerId,
    HoverText,
    Reference,
    SearchByExactLocation,
    SearchNonLocalByLocation,
    NameDefinition,
    ProjectDocument,
    SearchByName,
    ResolveLocation,
    FileEntityXRefLocation,
    DefinitionLocation,
    NameLowerCase,
    Declaration,
    TagDefinition,
    HoverContent,
    SearchByExactLocationAndName,
    DefinitionUse,
    EntityDefinition,
    EntityLocation,
    Metadata,
    MonikerDefinition,
    EntityUses,
    EntityKind,
    DefinitionHover,
    Moniker,
    ToolInfo,
    Location,
    LanguageId,
    SomeEntity,
    SymbolKind,
    Entity,
    RangeSpan,
    MonikerKind,
)


class LsifRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], range: ast.Expr, fullRange: ast.Expr, text: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, range, 'range'), angle_for(__env, fullRange, 'fullRange'), angle_for(__env, text, 'text')]))
    return f"lsif.Range.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Range

  @staticmethod
  def angle_query(*, range: Optional["LsifRangeSpan"] = None, fullRange: Optional[Union[Just["LsifRangeSpan"], Just[None]]] = None, text: Optional["LsifName"] = None) -> "LsifRange":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerSymbolKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], moniker: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, moniker, 'moniker'), angle_for(__env, kind, 'kind')]))
    return f"lsif.MonikerSymbolKind.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MonikerSymbolKind

  @staticmethod
  def angle_query(*, moniker: Optional["LsifMoniker"] = None, kind: Optional["LsifSymbolKind"] = None) -> "LsifMonikerSymbolKind":
    raise Exception("this function can only be called from @angle_query")



class LsifName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"lsif.Name.2 { query_fields if query_fields else '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifName":
    raise Exception("this function can only be called from @angle_query")



class LsifDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, language, 'language')]))
    return f"lsif.Document.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Document

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, language: Optional["LsifLanguageId"] = None) -> "LsifDocument":
    raise Exception("this function can only be called from @angle_query")



class LsifToSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, lsif: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, lsif, 'lsif'), angle_for(__env, range, 'range')]))
    return f"lsif.ToSrcRange.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ToSrcRange

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, lsif: Optional["LsifRangeSpan"] = None, range: Optional["SrcRange"] = None) -> "LsifToSrcRange":
    raise Exception("this function can only be called from @angle_query")



class LsifPackageInformation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, manager: ast.Expr, version: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, manager, 'manager'), angle_for(__env, version, 'version')]))
    return f"lsif.PackageInformation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", PackageInformation

  @staticmethod
  def angle_query(*, name: Optional[str] = None, manager: Optional[str] = None, version: Optional[str] = None) -> "LsifPackageInformation":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, range, 'range')]))
    return f"lsif.Definition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Definition

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None) -> "LsifDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifProject(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind')]))
    return f"lsif.Project.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Project

  @staticmethod
  def angle_query(*, kind: Optional["LsifLanguageId"] = None) -> "LsifProject":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, kind, 'kind')]))
    return f"lsif.DefinitionKind.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionKind

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, kind: Optional["LsifSymbolKind"] = None) -> "LsifDefinitionKind":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], ident: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, ident, 'ident'), angle_for(__env, entity, 'entity')]))
    return f"lsif.SearchByMoniker.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByMoniker

  @staticmethod
  def angle_query(*, ident: Optional["LsifMonikerId"] = None, entity: Optional["LsifEntity"] = None) -> "LsifSearchByMoniker":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, moniker: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, moniker, 'moniker')]))
    return f"lsif.DefinitionMoniker.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionMoniker

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, moniker: Optional[Union[Just["LsifMoniker"], Just[None]]] = None) -> "LsifDefinitionMoniker":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerScheme(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"lsif.MonikerScheme.2 { query_fields if query_fields else '_' }", MonikerScheme

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifMonikerScheme":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"lsif.MonikerId.2 { query_fields if query_fields else '_' }", MonikerId

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifMonikerId":
    raise Exception("this function can only be called from @angle_query")



class LsifHoverText(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"lsif.HoverText.2 { query_fields if query_fields else '_' }", HoverText

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifHoverText":
    raise Exception("this function can only be called from @angle_query")



class LsifReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, range: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, range, 'range'), angle_for(__env, target, 'target')]))
    return f"lsif.Reference.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Reference

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None, target: Optional["LsifDefinition"] = None) -> "LsifReference":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByExactLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, entity, 'entity')]))
    return f"lsif.SearchByExactLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByExactLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["LsifRangeSpan"] = None, entity: Optional["LsifEntity"] = None) -> "LsifSearchByExactLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchNonLocalByLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"lsif.SearchNonLocalByLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchNonLocalByLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, name: Optional["LsifName"] = None, entity: Optional["LsifEntity"] = None) -> "LsifSearchNonLocalByLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifNameDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, defn, 'defn')]))
    return f"lsif.NameDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", NameDefinition

  @staticmethod
  def angle_query(*, name: Optional["LsifName"] = None, defn: Optional["LsifDefinitionMoniker"] = None) -> "LsifNameDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifProjectDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, project: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, project, 'project')]))
    return f"lsif.ProjectDocument.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ProjectDocument

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, project: Optional["LsifProject"] = None) -> "LsifProjectDocument":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')]))
    return f"lsif.SearchByName.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByName

  @staticmethod
  def angle_query(*, name: Optional["LsifName"] = None, entity: Optional["LsifEntity"] = None) -> "LsifSearchByName":
    raise Exception("this function can only be called from @angle_query")



class LsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')]))
    return f"lsif.ResolveLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ResolveLocation

  @staticmethod
  def angle_query(*, location: Optional["LsifLocation"] = None, entity: Optional["LsifEntity"] = None) -> "LsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifFileEntityXRefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, source: ast.Expr, target: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, source, 'source'), angle_for(__env, target, 'target'), angle_for(__env, entity, 'entity')]))
    return f"lsif.FileEntityXRefLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntityXRefLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, source: Optional["SrcRange"] = None, target: Optional["LsifLocation"] = None, entity: Optional["LsifEntity"] = None) -> "LsifFileEntityXRefLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, location, 'location')]))
    return f"lsif.DefinitionLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionLocation

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, location: Optional["LsifLocation"] = None) -> "LsifDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')]))
    return f"lsif.NameLowerCase.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["LsifName"] = None) -> "LsifNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class LsifDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, range, 'range')]))
    return f"lsif.Declaration.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Declaration

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None) -> "LsifDeclaration":
    raise Exception("this function can only be called from @angle_query")



class LsifTagDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], language: ast.Expr, defn: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, language, 'language'), angle_for(__env, defn, 'defn'), angle_for(__env, entity, 'entity')]))
    return f"lsif.TagDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TagDefinition

  @staticmethod
  def angle_query(*, language: Optional["LsifLanguageId"] = None, defn: Optional["LsifDefinitionMoniker"] = None, entity: Optional["LsifEntity"] = None) -> "LsifTagDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifHoverContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], text: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, text, 'text'), angle_for(__env, language, 'language')]))
    return f"lsif.HoverContent.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", HoverContent

  @staticmethod
  def angle_query(*, text: Optional["LsifHoverText"] = None, language: Optional["LsifLanguageId"] = None) -> "LsifHoverContent":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByExactLocationAndName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr, span: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, name, 'name'), angle_for(__env, span, 'span'), angle_for(__env, entity, 'entity')]))
    return f"lsif.SearchByExactLocationAndName.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SearchByExactLocationAndName

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, name: Optional["LsifName"] = None, span: Optional["LsifRangeSpan"] = None, entity: Optional["LsifEntity"] = None) -> "LsifSearchByExactLocationAndName":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionUse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')]))
    return f"lsif.DefinitionUse.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionUse

  @staticmethod
  def angle_query(*, target: Optional["LsifDefinition"] = None, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None) -> "LsifDefinitionUse":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, defn, 'defn')]))
    return f"lsif.EntityDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityDefinition

  @staticmethod
  def angle_query(*, entity: Optional["LsifEntity"] = None, defn: Optional["LsifDefinitionMoniker"] = None) -> "LsifEntityDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')]))
    return f"lsif.EntityLocation.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityLocation

  @staticmethod
  def angle_query(*, entity: Optional["LsifEntity"] = None, location: Optional["LsifLocation"] = None) -> "LsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lsifVersion: ast.Expr, positionEncoding: ast.Expr, toolInfo: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, lsifVersion, 'lsifVersion'), angle_for(__env, positionEncoding, 'positionEncoding'), angle_for(__env, toolInfo, 'toolInfo')]))
    return f"lsif.Metadata.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Metadata

  @staticmethod
  def angle_query(*, lsifVersion: Optional[str] = None, positionEncoding: Optional[str] = None, toolInfo: Optional[Union[Just["LsifToolInfo"], Just[None]]] = None) -> "LsifMetadata":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], ident: ast.Expr, moniker: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, ident, 'ident'), angle_for(__env, moniker, 'moniker'), angle_for(__env, defn, 'defn')]))
    return f"lsif.MonikerDefinition.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", MonikerDefinition

  @staticmethod
  def angle_query(*, ident: Optional["LsifMonikerId"] = None, moniker: Optional["LsifMoniker"] = None, defn: Optional["LsifDefinition"] = None) -> "LsifMonikerDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')]))
    return f"lsif.EntityUses.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityUses

  @staticmethod
  def angle_query(*, target: Optional["LsifEntity"] = None, file: Optional["SrcFile"] = None, range: Optional["SrcRange"] = None) -> "LsifEntityUses":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')]))
    return f"lsif.EntityKind.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", EntityKind

  @staticmethod
  def angle_query(*, entity: Optional["LsifEntity"] = None, kind: Optional["LsifSymbolKind"] = None) -> "LsifEntityKind":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionHover(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, hover: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, hover, 'hover')]))
    return f"lsif.DefinitionHover.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionHover

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, hover: Optional["LsifHoverContent"] = None) -> "LsifDefinitionHover":
    raise Exception("this function can only be called from @angle_query")



class LsifMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr, scheme: ast.Expr, ident: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind'), angle_for(__env, scheme, 'scheme'), angle_for(__env, ident, 'ident')]))
    return f"lsif.Moniker.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Moniker

  @staticmethod
  def angle_query(*, kind: Optional["LsifMonikerKind"] = None, scheme: Optional["LsifMonikerScheme"] = None, ident: Optional["LsifMonikerId"] = None) -> "LsifMoniker":
    raise Exception("this function can only be called from @angle_query")





class LsifToolInfo(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], toolName: ast.Expr, toolArgs: ast.Expr, version: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, toolName, 'toolName'), angle_for(__env, toolArgs, 'toolArgs'), angle_for(__env, version, 'version')]))
    return f"lsif.ToolInfo.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ToolInfo

  @staticmethod
  def angle_query(*, toolName: Optional[str] = None, toolArgs: Optional[List[str]] = None, version: Optional[Union[Just[str], Just[None]]] = None) -> "LsifToolInfo":
    raise Exception("this function can only be called from @angle_query")



class LsifLocation(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, file: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, file, 'file'), angle_for(__env, location, 'location')]))
    return f"lsif.Location.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Location

  @staticmethod
  def angle_query(*, name: Optional[str] = None, file: Optional["SrcFile"] = None, location: Optional["SrcRange"] = None) -> "LsifLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifLanguageId(Enum):
  ABAP = 0
  WindowsBat = 1
  BibTeX = 2
  Clojure = 3
  Coffeescript = 4
  C = 5
  Cpp = 6
  CSharp = 7
  CSS = 8
  Diff = 9
  Dart = 10
  Dockerfile = 11
  Elixir = 12
  Erlang = 13
  FSharp = 14
  Git = 15
  Go = 16
  Groovy = 17
  Handlebars = 18
  Haskell = 19
  HTML = 20
  Ini = 21
  Java = 22
  JavaScript = 23
  JavaScriptReact = 24
  JSON = 25
  LaTeX = 26
  Less = 27
  Lua = 28
  Makefile = 29
  Markdown = 30
  ObjectiveC = 31
  ObjectiveCpp = 32
  Perl = 33
  Perl6 = 34
  PHP = 35
  Powershell = 36
  Pug = 37
  Python = 38
  R = 39
  Razor = 40
  Ruby = 41
  Rust = 42
  SCSS = 43
  Scala = 44
  ShaderLab = 45
  Shell = 46
  SQL = 47
  Swift = 48
  TypeScript = 49
  TypeScriptReact = 50
  TeX = 51
  VisualBasic = 52
  XML = 53
  XSL = 54
  YAML = 55
  UnknownLanguage = 56
  Kotlin = 57
  OCaml = 58

class LsifSomeEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], decl: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, decl, 'decl'), angle_for(__env, defn, 'defn')]))
    return f"lsif.SomeEntity.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SomeEntity

  @staticmethod
  def angle_query_decl(*, decl: Optional["LsifDeclaration"] = None) -> "LsifSomeEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_defn(*, defn: Optional["LsifDefinitionMoniker"] = None) -> "LsifSomeEntity":
    raise Exception("this function can only be called from @angle_query")




class LsifSymbolKind(Enum):
  File = 0
  Module = 1
  Namespace = 2
  Package = 3
  Class_ = 4
  Method = 5
  Property = 6
  Field = 7
  Constructor = 8
  Enum = 9
  Interface = 10
  Function = 11
  Variable = 12
  Constant = 13
  String = 14
  Number = 15
  Boolean = 16
  Array = 17
  Object_ = 18
  Key = 19
  Null = 20
  EnumMember = 21
  Struct = 22
  Event = 23
  Operator = 24
  TypeParameter = 25
  Unknown = 26

class LsifEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], erlang: ast.Expr, fsharp: ast.Expr, go: ast.Expr, haskell: ast.Expr, java: ast.Expr, kotlin: ast.Expr, ocaml: ast.Expr, python: ast.Expr, rust: ast.Expr, scala: ast.Expr, swift: ast.Expr, typescript: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, erlang, 'erlang'), angle_for(__env, fsharp, 'fsharp'), angle_for(__env, go, 'go'), angle_for(__env, haskell, 'haskell'), angle_for(__env, java, 'java'), angle_for(__env, kotlin, 'kotlin'), angle_for(__env, ocaml, 'ocaml'), angle_for(__env, python, 'python'), angle_for(__env, rust, 'rust'), angle_for(__env, scala, 'scala'), angle_for(__env, swift, 'swift'), angle_for(__env, typescript, 'typescript')]))
    return f"lsif.Entity.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Entity

  @staticmethod
  def angle_query_erlang(*, erlang: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_fsharp(*, fsharp: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_go(*, go: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_haskell(*, haskell: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_java(*, java: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_kotlin(*, kotlin: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_ocaml(*, ocaml: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_python(*, python: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_rust(*, rust: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_scala(*, scala: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_swift(*, swift: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typescript(*, typescript: Optional["LsifSomeEntity"] = None) -> "LsifEntity":
    raise Exception("this function can only be called from @angle_query")




class LsifRangeSpan(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lineBegin: ast.Expr, columnBegin: ast.Expr, lineEnd: ast.Expr, columnEnd: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, lineBegin, 'lineBegin'), angle_for(__env, columnBegin, 'columnBegin'), angle_for(__env, lineEnd, 'lineEnd'), angle_for(__env, columnEnd, 'columnEnd')]))
    return f"lsif.RangeSpan.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RangeSpan

  @staticmethod
  def angle_query(*, lineBegin: Optional[int] = None, columnBegin: Optional[int] = None, lineEnd: Optional[int] = None, columnEnd: Optional[int] = None) -> "LsifRangeSpan":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerKind(Enum):
  Export = 0
  Local = 1
  Import = 2
  Implementation = 3


