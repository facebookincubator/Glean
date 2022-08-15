# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class BuckTargetHash(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetHash.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetHash":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSourcesBaseModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetSourcesBaseModule.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetSourcesBaseModule":
    raise Exception("this function can only be called from @angle_query")

class BuckLocatorReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.LocatorReverseDeps.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckLocatorReverseDeps":
    raise Exception("this function can only be called from @angle_query")

class BuckType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Type.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckType":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetDependencies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetDependencies.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetDependencies":
    raise Exception("this function can only be called from @angle_query")

class BuckLocator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Locator.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckLocator":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetSources.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class BuckPlatform(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Platform.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckPlatform":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetAttribute.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetAttribute":
    raise Exception("this function can only be called from @angle_query")

class BuckFileEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileEntity.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckFileEntity":
    raise Exception("this function can only be called from @angle_query")

class BuckOutTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.OutTarget.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckOutTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Label.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetOuts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetOuts.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetOuts":
    raise Exception("this function can only be called from @angle_query")

class BuckOutsTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.OutsTarget.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckOutsTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckLocatorWithLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.LocatorWithLabel.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckLocatorWithLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetSources.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class BuckAttributeValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.AttributeValue.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckAttributeValue":
    raise Exception("this function can only be called from @angle_query")

class BuckLabels(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Labels.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckLabels":
    raise Exception("this function can only be called from @angle_query")

class BuckSourceFileLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.SourceFileLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckSourceFileLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckOutputLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.OutputLabel.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckOutputLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetUses":
    raise Exception("this function can only be called from @angle_query")

class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Owner.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")

class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Target.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.DefinitionLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetIndexer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetIndexer.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetIndexer":
    raise Exception("this function can only be called from @angle_query")

class BuckFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileDefinition.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Target.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckAttributeName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.AttributeName.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckAttributeName":
    raise Exception("this function can only be called from @angle_query")

class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Owner.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")

class BuckRuleKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.RuleKey.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckRuleKey":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetLinkWhole(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetLinkWhole.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckTargetLinkWhole":
    raise Exception("this function can only be called from @angle_query")

class BuckDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Definition.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckDefinition":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetOut(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetOut.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetOut":
    raise Exception("this function can only be called from @angle_query")

class BuckFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileXRefs.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetLocation.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckDestinationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.DestinationUses.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckDestinationUses":
    raise Exception("this function can only be called from @angle_query")

class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TranslationUnit.2 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.File.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")

class BuckFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileTarget.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckFileTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TranslationUnit.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetIndexerName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.TargetIndexerName.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckTargetIndexerName":
    raise Exception("this function can only be called from @angle_query")

class BuckFileResolved(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.FileResolved.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckFileResolved":
    raise Exception("this function can only be called from @angle_query")

class BuckConsumer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.Consumer.3 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "BuckConsumer":
    raise Exception("this function can only be called from @angle_query")

class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"buck.File.3 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")


