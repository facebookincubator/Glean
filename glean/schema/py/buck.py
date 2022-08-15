# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSBuckTargetHash(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetHash.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetHash":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetSourcesBaseModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetSourcesBaseModule.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetSourcesBaseModule":
    raise Exception("this function can only be called from @angle_query")

class GSBuckLocatorReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.LocatorReverseDeps.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckLocatorReverseDeps":
    raise Exception("this function can only be called from @angle_query")

class GSBuckType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Type.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckType":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetDependencies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetDependencies.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetDependencies":
    raise Exception("this function can only be called from @angle_query")

class GSBuckLocator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Locator.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckLocator":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetSources.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class GSBuckPlatform(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Platform.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckPlatform":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetAttribute.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetAttribute":
    raise Exception("this function can only be called from @angle_query")

class GSBuckFileEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileEntity.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckFileEntity":
    raise Exception("this function can only be called from @angle_query")

class GSBuckOutTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.OutTarget.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckOutTarget":
    raise Exception("this function can only be called from @angle_query")

class GSBuckLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Label.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckLabel":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetOuts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetOuts.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetOuts":
    raise Exception("this function can only be called from @angle_query")

class GSBuckOutsTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.OutsTarget.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckOutsTarget":
    raise Exception("this function can only be called from @angle_query")

class GSBuckLocatorWithLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.LocatorWithLabel.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckLocatorWithLabel":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetSources.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class GSBuckAttributeValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.AttributeValue.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckAttributeValue":
    raise Exception("this function can only be called from @angle_query")

class GSBuckLabels(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Labels.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckLabels":
    raise Exception("this function can only be called from @angle_query")

class GSBuckSourceFileLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.SourceFileLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckSourceFileLocation":
    raise Exception("this function can only be called from @angle_query")

class GSBuckOutputLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.OutputLabel.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckOutputLabel":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetUses":
    raise Exception("this function can only be called from @angle_query")

class GSBuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Owner.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckOwner":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Target.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTarget":
    raise Exception("this function can only be called from @angle_query")

class GSBuckDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.DefinitionLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetIndexer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetIndexer.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetIndexer":
    raise Exception("this function can only be called from @angle_query")

class GSBuckFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileDefinition.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Target.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTarget":
    raise Exception("this function can only be called from @angle_query")

class GSBuckAttributeName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.AttributeName.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckAttributeName":
    raise Exception("this function can only be called from @angle_query")

class GSBuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Owner.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckOwner":
    raise Exception("this function can only be called from @angle_query")

class GSBuckRuleKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.RuleKey.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckRuleKey":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetLinkWhole(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetLinkWhole.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckTargetLinkWhole":
    raise Exception("this function can only be called from @angle_query")

class GSBuckDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Definition.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckDefinition":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetOut(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetOut.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetOut":
    raise Exception("this function can only be called from @angle_query")

class GSBuckFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileXRefs.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class GSBuckDestinationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.DestinationUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckDestinationUses":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TranslationUnit.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class GSBuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.File.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckFile":
    raise Exception("this function can only be called from @angle_query")

class GSBuckFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileTarget.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckFileTarget":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TranslationUnit.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class GSBuckTargetIndexerName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetIndexerName.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckTargetIndexerName":
    raise Exception("this function can only be called from @angle_query")

class GSBuckFileResolved(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileResolved.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckFileResolved":
    raise Exception("this function can only be called from @angle_query")

class GSBuckConsumer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Consumer.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GSBuckConsumer":
    raise Exception("this function can only be called from @angle_query")

class GSBuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.File.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSBuckFile":
    raise Exception("this function can only be called from @angle_query")


