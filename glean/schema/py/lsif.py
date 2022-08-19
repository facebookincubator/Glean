# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Range.2 { { } }", Range

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifRange":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerSymbolKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerSymbolKind.2 { { } }", MonikerSymbolKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMonikerSymbolKind":
    raise Exception("this function can only be called from @angle_query")

class LsifName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Name.2 { json.dumps(key) }", Name

  @staticmethod
  def angle_query(*, name: str) -> "LsifName":
    raise Exception("this function can only be called from @angle_query")

class LsifDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Document.2 { { } }", Document

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifToSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.ToSrcRange.2 { { } }", ToSrcRange

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifToSrcRange":
    raise Exception("this function can only be called from @angle_query")

class LsifPackageInformation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.PackageInformation.2 { { } }", PackageInformation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifPackageInformation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Definition.2 { { } }", Definition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProject(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Project.2 { { } }", Project

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifProject":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionKind.2 { { } }", DefinitionKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionKind":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByMoniker.2 { { } }", SearchByMoniker

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionMoniker.2 { { } }", DefinitionMoniker

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerScheme(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerScheme.2 { json.dumps(key) }", MonikerScheme

  @staticmethod
  def angle_query(*, name: str) -> "LsifMonikerScheme":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerId.2 { json.dumps(key) }", MonikerId

  @staticmethod
  def angle_query(*, name: str) -> "LsifMonikerId":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverText(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.HoverText.2 { json.dumps(key) }", HoverText

  @staticmethod
  def angle_query(*, name: str) -> "LsifHoverText":
    raise Exception("this function can only be called from @angle_query")

class LsifReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Reference.2 { { } }", Reference

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifReference":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByExactLocation.2 { { } }", SearchByExactLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByExactLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchNonLocalByLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchNonLocalByLocation.2 { { } }", SearchNonLocalByLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchNonLocalByLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.NameDefinition.2 { { } }", NameDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifNameDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProjectDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.ProjectDocument.2 { { } }", ProjectDocument

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifProjectDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByName.2 { { } }", SearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class LsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.ResolveLocation.2 { { } }", ResolveLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifFileEntityXRefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.FileEntityXRefLocation.2 { { } }", FileEntityXRefLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifFileEntityXRefLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionLocation.2 { { } }", DefinitionLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.NameLowerCase.2 { { } }", NameLowerCase

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class LsifDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Declaration.2 { { } }", Declaration

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDeclaration":
    raise Exception("this function can only be called from @angle_query")

class LsifTagDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.TagDefinition.2 { { } }", TagDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifTagDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.HoverContent.2 { { } }", HoverContent

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifHoverContent":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocationAndName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByExactLocationAndName.2 { { } }", SearchByExactLocationAndName

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByExactLocationAndName":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionUse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionUse.2 { { } }", DefinitionUse

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionUse":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityDefinition.2 { { } }", EntityDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityLocation.2 { { } }", EntityLocation

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Metadata.2 { { } }", Metadata

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMetadata":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerDefinition.2 { { } }", MonikerDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMonikerDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityUses.2 { { } }", EntityUses

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityKind.2 { { } }", EntityKind

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityKind":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionHover(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionHover.2 { { } }", DefinitionHover

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionHover":
    raise Exception("this function can only be called from @angle_query")

class LsifMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Moniker.2 { { } }", Moniker

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMoniker":
    raise Exception("this function can only be called from @angle_query")


