# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class LsifRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Range.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifRange":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerSymbolKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.MonikerSymbolKind.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMonikerSymbolKind":
    raise Exception("this function can only be called from @angle_query")

class LsifName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Name.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "LsifName":
    raise Exception("this function can only be called from @angle_query")

class LsifDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Document.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifToSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.ToSrcRange.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifToSrcRange":
    raise Exception("this function can only be called from @angle_query")

class LsifPackageInformation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.PackageInformation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifPackageInformation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Definition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProject(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Project.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifProject":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.DefinitionKind.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionKind":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.SearchByMoniker.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.DefinitionMoniker.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionMoniker":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerScheme(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.MonikerScheme.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "LsifMonikerScheme":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.MonikerId.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "LsifMonikerId":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverText(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.HoverText.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "LsifHoverText":
    raise Exception("this function can only be called from @angle_query")

class LsifReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Reference.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifReference":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.SearchByExactLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByExactLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchNonLocalByLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.SearchNonLocalByLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchNonLocalByLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.NameDefinition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifNameDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifProjectDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.ProjectDocument.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifProjectDocument":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.SearchByName.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class LsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.ResolveLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifFileEntityXRefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.FileEntityXRefLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifFileEntityXRefLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.DefinitionLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.NameLowerCase.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class LsifDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Declaration.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDeclaration":
    raise Exception("this function can only be called from @angle_query")

class LsifTagDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.TagDefinition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifTagDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifHoverContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.HoverContent.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifHoverContent":
    raise Exception("this function can only be called from @angle_query")

class LsifSearchByExactLocationAndName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.SearchByExactLocationAndName.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifSearchByExactLocationAndName":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionUse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.DefinitionUse.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionUse":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.EntityDefinition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.EntityLocation.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class LsifMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Metadata.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMetadata":
    raise Exception("this function can only be called from @angle_query")

class LsifMonikerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.MonikerDefinition.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMonikerDefinition":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.EntityUses.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class LsifEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.EntityKind.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifEntityKind":
    raise Exception("this function can only be called from @angle_query")

class LsifDefinitionHover(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.DefinitionHover.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifDefinitionHover":
    raise Exception("this function can only be called from @angle_query")

class LsifMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"lsif.Moniker.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "LsifMoniker":
    raise Exception("this function can only be called from @angle_query")


