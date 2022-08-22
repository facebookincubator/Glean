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
    return f"lsif.Range.2 {{ range = _, fullRange = _, text = _ }}", Range

  @staticmethod
  def angle_query(*, range: Tuple[()], fullRange: Tuple[()], text: Tuple[()]) -> "LsifRange":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerSymbolKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerSymbolKind.2 {{ moniker = _, kind = _ }}", MonikerSymbolKind

  @staticmethod
  def angle_query(*, moniker: Tuple[()], kind: Tuple[()]) -> "LsifMonikerSymbolKind":
    raise Exception("this function can only be called from @angle_query")

class LsifName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Name.2 {json.dumps(key)}", Name

  @staticmethod
  def angle_query(*, arg: str) -> "LsifName":
    raise Exception("this function can only be called from @angle_query")

class LsifDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Document.2 {{ file = _, language = _ }}", Document

  @staticmethod
  def angle_query(*, file: Tuple[()], language: Tuple[()]) -> "LsifDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifToSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.ToSrcRange.2 {{ file = _, lsif = _, range = _ }}", ToSrcRange

  @staticmethod
  def angle_query(*, file: Tuple[()], lsif: Tuple[()], range: Tuple[()]) -> "LsifToSrcRange":
    raise Exception("this function can only be called from @angle_query")

class LsifPackageInformation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.PackageInformation.2 {{ name = _, manager = _, version = _ }}", PackageInformation

  @staticmethod
  def angle_query(*, name: str, manager: str, version: str) -> "LsifPackageInformation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Definition.2 {{ file = _, range = _ }}", Definition

  @staticmethod
  def angle_query(*, file: Tuple[()], range: Tuple[()]) -> "LsifDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProject(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Project.2 {{ kind = _ }}", Project

  @staticmethod
  def angle_query(*, kind: Tuple[()]) -> "LsifProject":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionKind.2 {{ defn = _, kind = _ }}", DefinitionKind

  @staticmethod
  def angle_query(*, defn: Tuple[()], kind: Tuple[()]) -> "LsifDefinitionKind":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByMoniker.2 {{ ident = _, entity = _ }}", SearchByMoniker

  @staticmethod
  def angle_query(*, ident: Tuple[()], entity: Tuple[()]) -> "LsifSearchByMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionMoniker.2 {{ defn = _, moniker = _ }}", DefinitionMoniker

  @staticmethod
  def angle_query(*, defn: Tuple[()], moniker: Tuple[()]) -> "LsifDefinitionMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerScheme(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerScheme.2 {json.dumps(key)}", MonikerScheme

  @staticmethod
  def angle_query(*, arg: str) -> "LsifMonikerScheme":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerId.2 {json.dumps(key)}", MonikerId

  @staticmethod
  def angle_query(*, arg: str) -> "LsifMonikerId":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverText(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.HoverText.2 {json.dumps(key)}", HoverText

  @staticmethod
  def angle_query(*, arg: str) -> "LsifHoverText":
    raise Exception("this function can only be called from @angle_query")

class LsifReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Reference.2 {{ file = _, range = _, target = _ }}", Reference

  @staticmethod
  def angle_query(*, file: Tuple[()], range: Tuple[()], target: Tuple[()]) -> "LsifReference":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByExactLocation.2 {{ file = _, span = _, entity = _ }}", SearchByExactLocation

  @staticmethod
  def angle_query(*, file: Tuple[()], span: Tuple[()], entity: Tuple[()]) -> "LsifSearchByExactLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchNonLocalByLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchNonLocalByLocation.2 {{ file = _, name = _, entity = _ }}", SearchNonLocalByLocation

  @staticmethod
  def angle_query(*, file: Tuple[()], name: Tuple[()], entity: Tuple[()]) -> "LsifSearchNonLocalByLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.NameDefinition.2 {{ name = _, defn = _ }}", NameDefinition

  @staticmethod
  def angle_query(*, name: Tuple[()], defn: Tuple[()]) -> "LsifNameDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProjectDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.ProjectDocument.2 {{ file = _, project = _ }}", ProjectDocument

  @staticmethod
  def angle_query(*, file: Tuple[()], project: Tuple[()]) -> "LsifProjectDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByName.2 {{ name = _, entity = _ }}", SearchByName

  @staticmethod
  def angle_query(*, name: Tuple[()], entity: Tuple[()]) -> "LsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class LsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.ResolveLocation.2 {{ location = _, entity = _ }}", ResolveLocation

  @staticmethod
  def angle_query(*, location: Tuple[()], entity: Tuple[()]) -> "LsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifFileEntityXRefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.FileEntityXRefLocation.2 {{ file = _, source = _, target = _, entity = _ }}", FileEntityXRefLocation

  @staticmethod
  def angle_query(*, file: Tuple[()], source: Tuple[()], target: Tuple[()], entity: Tuple[()]) -> "LsifFileEntityXRefLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionLocation.2 {{ defn = _, location = _ }}", DefinitionLocation

  @staticmethod
  def angle_query(*, defn: Tuple[()], location: Tuple[()]) -> "LsifDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.NameLowerCase.2 {{ nameLowerCase = _, name = _ }}", NameLowerCase

  @staticmethod
  def angle_query(*, nameLowerCase: str, name: Tuple[()]) -> "LsifNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class LsifDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Declaration.2 {{ file = _, range = _ }}", Declaration

  @staticmethod
  def angle_query(*, file: Tuple[()], range: Tuple[()]) -> "LsifDeclaration":
    raise Exception("this function can only be called from @angle_query")

class LsifTagDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.TagDefinition.2 {{ language = _, defn = _, entity = _ }}", TagDefinition

  @staticmethod
  def angle_query(*, language: Tuple[()], defn: Tuple[()], entity: Tuple[()]) -> "LsifTagDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.HoverContent.2 {{ text = _, language = _ }}", HoverContent

  @staticmethod
  def angle_query(*, text: Tuple[()], language: Tuple[()]) -> "LsifHoverContent":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocationAndName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.SearchByExactLocationAndName.2 {{ file = _, name = _, span = _, entity = _ }}", SearchByExactLocationAndName

  @staticmethod
  def angle_query(*, file: Tuple[()], name: Tuple[()], span: Tuple[()], entity: Tuple[()]) -> "LsifSearchByExactLocationAndName":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionUse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionUse.2 {{ target = _, file = _, range = _ }}", DefinitionUse

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], range: Tuple[()]) -> "LsifDefinitionUse":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityDefinition.2 {{ entity = _, defn = _ }}", EntityDefinition

  @staticmethod
  def angle_query(*, entity: Tuple[()], defn: Tuple[()]) -> "LsifEntityDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityLocation.2 {{ entity = _, location = _ }}", EntityLocation

  @staticmethod
  def angle_query(*, entity: Tuple[()], location: Tuple[()]) -> "LsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Metadata.2 {{ lsifVersion = _, positionEncoding = _, toolInfo = _ }}", Metadata

  @staticmethod
  def angle_query(*, lsifVersion: str, positionEncoding: str, toolInfo: Tuple[()]) -> "LsifMetadata":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.MonikerDefinition.2 {{ ident = _, moniker = _, defn = _ }}", MonikerDefinition

  @staticmethod
  def angle_query(*, ident: Tuple[()], moniker: Tuple[()], defn: Tuple[()]) -> "LsifMonikerDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityUses.2 {{ target = _, file = _, range = _ }}", EntityUses

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()], range: Tuple[()]) -> "LsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.EntityKind.2 {{ entity = _, kind = _ }}", EntityKind

  @staticmethod
  def angle_query(*, entity: Tuple[()], kind: Tuple[()]) -> "LsifEntityKind":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionHover(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.DefinitionHover.2 {{ defn = _, hover = _ }}", DefinitionHover

  @staticmethod
  def angle_query(*, defn: Tuple[()], hover: Tuple[()]) -> "LsifDefinitionHover":
    raise Exception("this function can only be called from @angle_query")

class LsifMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"lsif.Moniker.2 {{ kind = _, scheme = _, ident = _ }}", Moniker

  @staticmethod
  def angle_query(*, kind: Tuple[()], scheme: Tuple[()], ident: Tuple[()]) -> "LsifMoniker":
    raise Exception("this function can only be called from @angle_query")


