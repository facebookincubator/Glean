# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
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
)


class LsifRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], range: ast.Expr, fullRange: ast.Expr, text: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Range.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, range, 'range'), angle_for(__env, fullRange, 'fullRange'), angle_for(__env, text, 'text')])) or '_' } }}", Range

  @staticmethod
  def angle_query(*, range: Optional[Tuple[()]] = None, fullRange: Optional[Tuple[()]] = None, text: Optional["LsifName"] = None) -> "LsifRange":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerSymbolKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], moniker: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.MonikerSymbolKind.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, moniker, 'moniker'), angle_for(__env, kind, 'kind')])) or '_' } }}", MonikerSymbolKind

  @staticmethod
  def angle_query(*, moniker: Optional["LsifMoniker"] = None, kind: Optional[Tuple[()]] = None) -> "LsifMonikerSymbolKind":
    raise Exception("this function can only be called from @angle_query")



class LsifName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Name.2 { angle_for(__env, arg, None) or '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifName":
    raise Exception("this function can only be called from @angle_query")



class LsifDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Document.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, language, 'language')])) or '_' } }}", Document

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, language: Optional[Tuple[()]] = None) -> "LsifDocument":
    raise Exception("this function can only be called from @angle_query")



class LsifToSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, lsif: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.ToSrcRange.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, lsif, 'lsif'), angle_for(__env, range, 'range')])) or '_' } }}", ToSrcRange

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, lsif: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "LsifToSrcRange":
    raise Exception("this function can only be called from @angle_query")



class LsifPackageInformation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, manager: ast.Expr, version: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.PackageInformation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, manager, 'manager'), angle_for(__env, version, 'version')])) or '_' } }}", PackageInformation

  @staticmethod
  def angle_query(*, name: Optional[str] = None, manager: Optional[str] = None, version: Optional[str] = None) -> "LsifPackageInformation":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Definition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, range, 'range')])) or '_' } }}", Definition

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None) -> "LsifDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifProject(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Project.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind')])) or '_' } }}", Project

  @staticmethod
  def angle_query(*, kind: Optional[Tuple[()]] = None) -> "LsifProject":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.DefinitionKind.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, kind, 'kind')])) or '_' } }}", DefinitionKind

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, kind: Optional[Tuple[()]] = None) -> "LsifDefinitionKind":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], ident: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.SearchByMoniker.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, ident, 'ident'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchByMoniker

  @staticmethod
  def angle_query(*, ident: Optional["LsifMonikerId"] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByMoniker":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, moniker: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.DefinitionMoniker.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, moniker, 'moniker')])) or '_' } }}", DefinitionMoniker

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, moniker: Optional[Tuple[()]] = None) -> "LsifDefinitionMoniker":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerScheme(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.MonikerScheme.2 { angle_for(__env, arg, None) or '_' }", MonikerScheme

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifMonikerScheme":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.MonikerId.2 { angle_for(__env, arg, None) or '_' }", MonikerId

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifMonikerId":
    raise Exception("this function can only be called from @angle_query")



class LsifHoverText(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.HoverText.2 { angle_for(__env, arg, None) or '_' }", HoverText

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifHoverText":
    raise Exception("this function can only be called from @angle_query")



class LsifReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, range: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Reference.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, range, 'range'), angle_for(__env, target, 'target')])) or '_' } }}", Reference

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None, target: Optional["LsifDefinition"] = None) -> "LsifReference":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByExactLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.SearchByExactLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchByExactLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByExactLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchNonLocalByLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.SearchNonLocalByLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchNonLocalByLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, name: Optional["LsifName"] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchNonLocalByLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifNameDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.NameDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, defn, 'defn')])) or '_' } }}", NameDefinition

  @staticmethod
  def angle_query(*, name: Optional["LsifName"] = None, defn: Optional["LsifDefinitionMoniker"] = None) -> "LsifNameDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifProjectDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, project: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.ProjectDocument.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, project, 'project')])) or '_' } }}", ProjectDocument

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, project: Optional["LsifProject"] = None) -> "LsifProjectDocument":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.SearchByName.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchByName

  @staticmethod
  def angle_query(*, name: Optional["LsifName"] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByName":
    raise Exception("this function can only be called from @angle_query")



class LsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], location: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.ResolveLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, location, 'location'), angle_for(__env, entity, 'entity')])) or '_' } }}", ResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifFileEntityXRefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, source: ast.Expr, target: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.FileEntityXRefLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, source, 'source'), angle_for(__env, target, 'target'), angle_for(__env, entity, 'entity')])) or '_' } }}", FileEntityXRefLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifFileEntityXRefLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.DefinitionLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, location, 'location')])) or '_' } }}", DefinitionLocation

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, location: Optional[Tuple[()]] = None) -> "LsifDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], nameLowerCase: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.NameLowerCase.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, nameLowerCase, 'nameLowerCase'), angle_for(__env, name, 'name')])) or '_' } }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional["LsifName"] = None) -> "LsifNameLowerCase":
    raise Exception("this function can only be called from @angle_query")



class LsifDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Declaration.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, range, 'range')])) or '_' } }}", Declaration

  @staticmethod
  def angle_query(*, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None) -> "LsifDeclaration":
    raise Exception("this function can only be called from @angle_query")



class LsifTagDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], language: ast.Expr, defn: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.TagDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, language, 'language'), angle_for(__env, defn, 'defn'), angle_for(__env, entity, 'entity')])) or '_' } }}", TagDefinition

  @staticmethod
  def angle_query(*, language: Optional[Tuple[()]] = None, defn: Optional["LsifDefinitionMoniker"] = None, entity: Optional[Tuple[()]] = None) -> "LsifTagDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifHoverContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], text: ast.Expr, language: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.HoverContent.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, text, 'text'), angle_for(__env, language, 'language')])) or '_' } }}", HoverContent

  @staticmethod
  def angle_query(*, text: Optional["LsifHoverText"] = None, language: Optional[Tuple[()]] = None) -> "LsifHoverContent":
    raise Exception("this function can only be called from @angle_query")



class LsifSearchByExactLocationAndName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, name: ast.Expr, span: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.SearchByExactLocationAndName.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, name, 'name'), angle_for(__env, span, 'span'), angle_for(__env, entity, 'entity')])) or '_' } }}", SearchByExactLocationAndName

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, name: Optional["LsifName"] = None, span: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByExactLocationAndName":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionUse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.DefinitionUse.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')])) or '_' } }}", DefinitionUse

  @staticmethod
  def angle_query(*, target: Optional["LsifDefinition"] = None, file: Optional["LsifDocument"] = None, range: Optional["LsifRange"] = None) -> "LsifDefinitionUse":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.EntityDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, defn, 'defn')])) or '_' } }}", EntityDefinition

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, defn: Optional["LsifDefinitionMoniker"] = None) -> "LsifEntityDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, location: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.EntityLocation.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, location, 'location')])) or '_' } }}", EntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "LsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")



class LsifMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], lsifVersion: ast.Expr, positionEncoding: ast.Expr, toolInfo: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Metadata.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, lsifVersion, 'lsifVersion'), angle_for(__env, positionEncoding, 'positionEncoding'), angle_for(__env, toolInfo, 'toolInfo')])) or '_' } }}", Metadata

  @staticmethod
  def angle_query(*, lsifVersion: Optional[str] = None, positionEncoding: Optional[str] = None, toolInfo: Optional[Tuple[()]] = None) -> "LsifMetadata":
    raise Exception("this function can only be called from @angle_query")



class LsifMonikerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], ident: ast.Expr, moniker: ast.Expr, defn: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.MonikerDefinition.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, ident, 'ident'), angle_for(__env, moniker, 'moniker'), angle_for(__env, defn, 'defn')])) or '_' } }}", MonikerDefinition

  @staticmethod
  def angle_query(*, ident: Optional["LsifMonikerId"] = None, moniker: Optional["LsifMoniker"] = None, defn: Optional["LsifDefinition"] = None) -> "LsifMonikerDefinition":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr, range: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.EntityUses.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file'), angle_for(__env, range, 'range')])) or '_' } }}", EntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional["SrcFile"] = None, range: Optional[Tuple[()]] = None) -> "LsifEntityUses":
    raise Exception("this function can only be called from @angle_query")



class LsifEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], entity: ast.Expr, kind: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.EntityKind.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, entity, 'entity'), angle_for(__env, kind, 'kind')])) or '_' } }}", EntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "LsifEntityKind":
    raise Exception("this function can only be called from @angle_query")



class LsifDefinitionHover(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], defn: ast.Expr, hover: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.DefinitionHover.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, defn, 'defn'), angle_for(__env, hover, 'hover')])) or '_' } }}", DefinitionHover

  @staticmethod
  def angle_query(*, defn: Optional["LsifDefinition"] = None, hover: Optional["LsifHoverContent"] = None) -> "LsifDefinitionHover":
    raise Exception("this function can only be called from @angle_query")



class LsifMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kind: ast.Expr, scheme: ast.Expr, ident: ast.Expr) -> Tuple[str, Struct]:
    return f"lsif.Moniker.2 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kind, 'kind'), angle_for(__env, scheme, 'scheme'), angle_for(__env, ident, 'ident')])) or '_' } }}", Moniker

  @staticmethod
  def angle_query(*, kind: Optional[Tuple[()]] = None, scheme: Optional["LsifMonikerScheme"] = None, ident: Optional["LsifMonikerId"] = None) -> "LsifMoniker":
    raise Exception("this function can only be called from @angle_query")




