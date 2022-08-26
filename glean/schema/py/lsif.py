# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Range.2 {{ }}", Range
    return f"lsif.Range.2 { concatenateFields(key) }", Range

  @staticmethod
  def angle_query(*, range: Optional[Tuple[()]] = None, fullRange: Optional[Tuple[()]] = None, text: Optional[Tuple[()]] = None) -> "LsifRange":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerSymbolKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.MonikerSymbolKind.2 {{ }}", MonikerSymbolKind
    return f"lsif.MonikerSymbolKind.2 { concatenateFields(key) }", MonikerSymbolKind

  @staticmethod
  def angle_query(*, moniker: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "LsifMonikerSymbolKind":
    raise Exception("this function can only be called from @angle_query")

class LsifName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Name.2 {{ }}", Name
    return f"lsif.Name.2 {key}", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifName":
    raise Exception("this function can only be called from @angle_query")

class LsifDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Document.2 {{ }}", Document
    return f"lsif.Document.2 { concatenateFields(key) }", Document

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "LsifDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifToSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.ToSrcRange.2 {{ }}", ToSrcRange
    return f"lsif.ToSrcRange.2 { concatenateFields(key) }", ToSrcRange

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, lsif: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "LsifToSrcRange":
    raise Exception("this function can only be called from @angle_query")

class LsifPackageInformation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.PackageInformation.2 {{ }}", PackageInformation
    return f"lsif.PackageInformation.2 { concatenateFields(key) }", PackageInformation

  @staticmethod
  def angle_query(*, name: Optional[str] = None, manager: Optional[str] = None, version: Optional[str] = None) -> "LsifPackageInformation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Definition.2 {{ }}", Definition
    return f"lsif.Definition.2 { concatenateFields(key) }", Definition

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "LsifDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProject(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Project.2 {{ }}", Project
    return f"lsif.Project.2 { concatenateFields(key) }", Project

  @staticmethod
  def angle_query(*, kind: Optional[Tuple[()]] = None) -> "LsifProject":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.DefinitionKind.2 {{ }}", DefinitionKind
    return f"lsif.DefinitionKind.2 { concatenateFields(key) }", DefinitionKind

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "LsifDefinitionKind":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.SearchByMoniker.2 {{ }}", SearchByMoniker
    return f"lsif.SearchByMoniker.2 { concatenateFields(key) }", SearchByMoniker

  @staticmethod
  def angle_query(*, ident: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.DefinitionMoniker.2 {{ }}", DefinitionMoniker
    return f"lsif.DefinitionMoniker.2 { concatenateFields(key) }", DefinitionMoniker

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, moniker: Optional[Tuple[()]] = None) -> "LsifDefinitionMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerScheme(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.MonikerScheme.2 {{ }}", MonikerScheme
    return f"lsif.MonikerScheme.2 {key}", MonikerScheme

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifMonikerScheme":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.MonikerId.2 {{ }}", MonikerId
    return f"lsif.MonikerId.2 {key}", MonikerId

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifMonikerId":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverText(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.HoverText.2 {{ }}", HoverText
    return f"lsif.HoverText.2 {key}", HoverText

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "LsifHoverText":
    raise Exception("this function can only be called from @angle_query")

class LsifReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Reference.2 {{ }}", Reference
    return f"lsif.Reference.2 { concatenateFields(key) }", Reference

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "LsifReference":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.SearchByExactLocation.2 {{ }}", SearchByExactLocation
    return f"lsif.SearchByExactLocation.2 { concatenateFields(key) }", SearchByExactLocation

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByExactLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchNonLocalByLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.SearchNonLocalByLocation.2 {{ }}", SearchNonLocalByLocation
    return f"lsif.SearchNonLocalByLocation.2 { concatenateFields(key) }", SearchNonLocalByLocation

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchNonLocalByLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.NameDefinition.2 {{ }}", NameDefinition
    return f"lsif.NameDefinition.2 { concatenateFields(key) }", NameDefinition

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "LsifNameDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProjectDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.ProjectDocument.2 {{ }}", ProjectDocument
    return f"lsif.ProjectDocument.2 { concatenateFields(key) }", ProjectDocument

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, project: Optional[Tuple[()]] = None) -> "LsifProjectDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.SearchByName.2 {{ }}", SearchByName
    return f"lsif.SearchByName.2 { concatenateFields(key) }", SearchByName

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class LsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.ResolveLocation.2 {{ }}", ResolveLocation
    return f"lsif.ResolveLocation.2 { concatenateFields(key) }", ResolveLocation

  @staticmethod
  def angle_query(*, location: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifFileEntityXRefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.FileEntityXRefLocation.2 {{ }}", FileEntityXRefLocation
    return f"lsif.FileEntityXRefLocation.2 { concatenateFields(key) }", FileEntityXRefLocation

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifFileEntityXRefLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.DefinitionLocation.2 {{ }}", DefinitionLocation
    return f"lsif.DefinitionLocation.2 { concatenateFields(key) }", DefinitionLocation

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "LsifDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.NameLowerCase.2 {{ }}", NameLowerCase
    return f"lsif.NameLowerCase.2 { concatenateFields(key) }", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: Optional[str] = None, name: Optional[Tuple[()]] = None) -> "LsifNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class LsifDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Declaration.2 {{ }}", Declaration
    return f"lsif.Declaration.2 { concatenateFields(key) }", Declaration

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "LsifDeclaration":
    raise Exception("this function can only be called from @angle_query")

class LsifTagDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.TagDefinition.2 {{ }}", TagDefinition
    return f"lsif.TagDefinition.2 { concatenateFields(key) }", TagDefinition

  @staticmethod
  def angle_query(*, language: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifTagDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.HoverContent.2 {{ }}", HoverContent
    return f"lsif.HoverContent.2 { concatenateFields(key) }", HoverContent

  @staticmethod
  def angle_query(*, text: Optional[Tuple[()]] = None, language: Optional[Tuple[()]] = None) -> "LsifHoverContent":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocationAndName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.SearchByExactLocationAndName.2 {{ }}", SearchByExactLocationAndName
    return f"lsif.SearchByExactLocationAndName.2 { concatenateFields(key) }", SearchByExactLocationAndName

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "LsifSearchByExactLocationAndName":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionUse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.DefinitionUse.2 {{ }}", DefinitionUse
    return f"lsif.DefinitionUse.2 { concatenateFields(key) }", DefinitionUse

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "LsifDefinitionUse":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.EntityDefinition.2 {{ }}", EntityDefinition
    return f"lsif.EntityDefinition.2 { concatenateFields(key) }", EntityDefinition

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "LsifEntityDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.EntityLocation.2 {{ }}", EntityLocation
    return f"lsif.EntityLocation.2 { concatenateFields(key) }", EntityLocation

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, location: Optional[Tuple[()]] = None) -> "LsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Metadata.2 {{ }}", Metadata
    return f"lsif.Metadata.2 { concatenateFields(key) }", Metadata

  @staticmethod
  def angle_query(*, lsifVersion: Optional[str] = None, positionEncoding: Optional[str] = None, toolInfo: Optional[Tuple[()]] = None) -> "LsifMetadata":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.MonikerDefinition.2 {{ }}", MonikerDefinition
    return f"lsif.MonikerDefinition.2 { concatenateFields(key) }", MonikerDefinition

  @staticmethod
  def angle_query(*, ident: Optional[Tuple[()]] = None, moniker: Optional[Tuple[()]] = None, defn: Optional[Tuple[()]] = None) -> "LsifMonikerDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.EntityUses.2 {{ }}", EntityUses
    return f"lsif.EntityUses.2 { concatenateFields(key) }", EntityUses

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, range: Optional[Tuple[()]] = None) -> "LsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.EntityKind.2 {{ }}", EntityKind
    return f"lsif.EntityKind.2 { concatenateFields(key) }", EntityKind

  @staticmethod
  def angle_query(*, entity: Optional[Tuple[()]] = None, kind: Optional[Tuple[()]] = None) -> "LsifEntityKind":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionHover(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.DefinitionHover.2 {{ }}", DefinitionHover
    return f"lsif.DefinitionHover.2 { concatenateFields(key) }", DefinitionHover

  @staticmethod
  def angle_query(*, defn: Optional[Tuple[()]] = None, hover: Optional[Tuple[()]] = None) -> "LsifDefinitionHover":
    raise Exception("this function can only be called from @angle_query")

class LsifMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"lsif.Moniker.2 {{ }}", Moniker
    return f"lsif.Moniker.2 { concatenateFields(key) }", Moniker

  @staticmethod
  def angle_query(*, kind: Optional[Tuple[()]] = None, scheme: Optional[Tuple[()]] = None, ident: Optional[Tuple[()]] = None) -> "LsifMoniker":
    raise Exception("this function can only be called from @angle_query")


