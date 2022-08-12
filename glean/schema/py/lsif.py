# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSLsifRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Range.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifRange":
    raise Exception("this function can only be called from @angle_query")

class GSLsifMonikerSymbolKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.MonikerSymbolKind.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifMonikerSymbolKind":
    raise Exception("this function can only be called from @angle_query")

class GSLsifName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Name.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifName":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Document.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDocument":
    raise Exception("this function can only be called from @angle_query")

class GSLsifToSrcRange(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.ToSrcRange.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifToSrcRange":
    raise Exception("this function can only be called from @angle_query")

class GSLsifPackageInformation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.PackageInformation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifPackageInformation":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Definition.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSLsifProject(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Project.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifProject":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDefinitionKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.DefinitionKind.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDefinitionKind":
    raise Exception("this function can only be called from @angle_query")

class GSLsifSearchByMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.SearchByMoniker.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifSearchByMoniker":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDefinitionMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.DefinitionMoniker.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDefinitionMoniker":
    raise Exception("this function can only be called from @angle_query")

class GSLsifMonikerScheme(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.MonikerScheme.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifMonikerScheme":
    raise Exception("this function can only be called from @angle_query")

class GSLsifMonikerId(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.MonikerId.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifMonikerId":
    raise Exception("this function can only be called from @angle_query")

class GSLsifHoverText(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.HoverText.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifHoverText":
    raise Exception("this function can only be called from @angle_query")

class GSLsifReference(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Reference.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifReference":
    raise Exception("this function can only be called from @angle_query")

class GSLsifSearchByExactLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.SearchByExactLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifSearchByExactLocation":
    raise Exception("this function can only be called from @angle_query")

class GSLsifSearchNonLocalByLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.SearchNonLocalByLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifSearchNonLocalByLocation":
    raise Exception("this function can only be called from @angle_query")

class GSLsifNameDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.NameDefinition.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifNameDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSLsifProjectDocument(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.ProjectDocument.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifProjectDocument":
    raise Exception("this function can only be called from @angle_query")

class GSLsifSearchByName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.SearchByName.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifSearchByName":
    raise Exception("this function can only be called from @angle_query")

class GSLsifResolveLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.ResolveLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifResolveLocation":
    raise Exception("this function can only be called from @angle_query")

class GSLsifFileEntityXRefLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.FileEntityXRefLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifFileEntityXRefLocation":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.DefinitionLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class GSLsifNameLowerCase(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.NameLowerCase.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifNameLowerCase":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDeclaration(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Declaration.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDeclaration":
    raise Exception("this function can only be called from @angle_query")

class GSLsifTagDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.TagDefinition.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifTagDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSLsifHoverContent(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.HoverContent.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifHoverContent":
    raise Exception("this function can only be called from @angle_query")

class GSLsifSearchByExactLocationAndName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.SearchByExactLocationAndName.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifSearchByExactLocationAndName":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDefinitionUse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.DefinitionUse.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDefinitionUse":
    raise Exception("this function can only be called from @angle_query")

class GSLsifEntityDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.EntityDefinition.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifEntityDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSLsifEntityLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.EntityLocation.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifEntityLocation":
    raise Exception("this function can only be called from @angle_query")

class GSLsifMetadata(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Metadata.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifMetadata":
    raise Exception("this function can only be called from @angle_query")

class GSLsifMonikerDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.MonikerDefinition.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifMonikerDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSLsifEntityUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.EntityUses.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifEntityUses":
    raise Exception("this function can only be called from @angle_query")

class GSLsifEntityKind(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.EntityKind.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifEntityKind":
    raise Exception("this function can only be called from @angle_query")

class GSLsifDefinitionHover(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.DefinitionHover.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifDefinitionHover":
    raise Exception("this function can only be called from @angle_query")

class GSLsifMoniker(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"lsif.Moniker.2 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSLsifMoniker":
    raise Exception("this function can only be called from @angle_query")


