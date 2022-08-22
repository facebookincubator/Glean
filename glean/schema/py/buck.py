# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.buck.types import (
    TargetHash,
    TargetSourcesBaseModule,
    LocatorReverseDeps,
    Type,
    TargetDependencies,
    Locator,
    TargetSources,
    Platform,
    TargetAttribute,
    FileEntity,
    OutTarget,
    Label,
    TargetOuts,
    OutsTarget,
    LocatorWithLabel,
    TargetSources,
    AttributeValue,
    Labels,
    SourceFileLocation,
    OutputLabel,
    TargetUses,
    Owner,
    Target,
    DefinitionLocation,
    TargetIndexer,
    FileDefinition,
    Target,
    AttributeName,
    Owner,
    RuleKey,
    TargetLinkWhole,
    Definition,
    TargetOut,
    FileXRefs,
    TargetLocation,
    DestinationUses,
    TranslationUnit,
    File,
    FileTarget,
    TranslationUnit,
    TargetIndexerName,
    FileResolved,
    Consumer,
    File,
)


class BuckTargetHash(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetHash.1 {{ locator = _, targetHash = _ }}", TargetHash

  @staticmethod
  def angle_query(*, locator: Tuple[()], targetHash: str) -> "BuckTargetHash":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSourcesBaseModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetSourcesBaseModule.3 {{ locator = _, srcs = _, baseModule = _ }}", TargetSourcesBaseModule

  @staticmethod
  def angle_query(*, locator: Tuple[()], srcs: Tuple[()], baseModule: Tuple[()]) -> "BuckTargetSourcesBaseModule":
    raise Exception("this function can only be called from @angle_query")

class BuckLocatorReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.LocatorReverseDeps.1 {{ locator = _, rdeps = _ }}", LocatorReverseDeps

  @staticmethod
  def angle_query(*, locator: Tuple[()], rdeps: Tuple[()]) -> "BuckLocatorReverseDeps":
    raise Exception("this function can only be called from @angle_query")

class BuckType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Type.1 {json.dumps(key)}", Type

  @staticmethod
  def angle_query(*, arg: str) -> "BuckType":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetDependencies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetDependencies.1 {{ target = _, dependencies = _ }}", TargetDependencies

  @staticmethod
  def angle_query(*, target: Tuple[()], dependencies: Tuple[()]) -> "BuckTargetDependencies":
    raise Exception("this function can only be called from @angle_query")

class BuckLocator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Locator.1 {{ subdir = _, path = _, name = _ }}", Locator

  @staticmethod
  def angle_query(*, subdir: Tuple[()], path: str, name: str) -> "BuckLocator":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetSources.1 {{ target = _, headers = _, exportedHeaders = _, srcs = _ }}", TargetSources

  @staticmethod
  def angle_query(*, target: Tuple[()], headers: Tuple[()], exportedHeaders: Tuple[()], srcs: Tuple[()]) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class BuckPlatform(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Platform.1 {json.dumps(key)}", Platform

  @staticmethod
  def angle_query(*, arg: str) -> "BuckPlatform":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetAttribute.3 {{ target = _, attribute = _, value = _ }}", TargetAttribute

  @staticmethod
  def angle_query(*, target: Tuple[()], attribute: Tuple[()], value: Tuple[()]) -> "BuckTargetAttribute":
    raise Exception("this function can only be called from @angle_query")

class BuckFileEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.FileEntity.3 {{ file = _, entity = _ }}", FileEntity

  @staticmethod
  def angle_query(*, file: Tuple[()], entity: Tuple[()]) -> "BuckFileEntity":
    raise Exception("this function can only be called from @angle_query")

class BuckOutTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.OutTarget.1 {{ file = _, target = _ }}", OutTarget

  @staticmethod
  def angle_query(*, file: Tuple[()], target: Tuple[()]) -> "BuckOutTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Label.1 {json.dumps(key)}", Label

  @staticmethod
  def angle_query(*, arg: str) -> "BuckLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetOuts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetOuts.3 {{ target = _, outputLabel = _, file = _ }}", TargetOuts

  @staticmethod
  def angle_query(*, target: Tuple[()], outputLabel: Tuple[()], file: Tuple[()]) -> "BuckTargetOuts":
    raise Exception("this function can only be called from @angle_query")

class BuckOutsTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.OutsTarget.3 {{ file = _, target = _, outputLabel = _ }}", OutsTarget

  @staticmethod
  def angle_query(*, file: Tuple[()], target: Tuple[()], outputLabel: Tuple[()]) -> "BuckOutsTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckLocatorWithLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.LocatorWithLabel.3 {{ locator = _, label = _ }}", LocatorWithLabel

  @staticmethod
  def angle_query(*, locator: Tuple[()], label: Tuple[()]) -> "BuckLocatorWithLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetSources.3 {{ target = _, headers = _, exportedHeaders = _, srcs = _ }}", TargetSources

  @staticmethod
  def angle_query(*, target: Tuple[()], headers: Tuple[()], exportedHeaders: Tuple[()], srcs: Tuple[()]) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class BuckAttributeValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.AttributeValue.3 {json.dumps(key)}", AttributeValue

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "BuckAttributeValue":
    raise Exception("this function can only be called from @angle_query")

class BuckLabels(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Labels.1 {json.dumps(key)}", Labels

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "BuckLabels":
    raise Exception("this function can only be called from @angle_query")

class BuckSourceFileLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.SourceFileLocation.3 {{ file = _, span = _ }}", SourceFileLocation

  @staticmethod
  def angle_query(*, file: Tuple[()], span: Tuple[()]) -> "BuckSourceFileLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckOutputLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.OutputLabel.3 {json.dumps(key)}", OutputLabel

  @staticmethod
  def angle_query(*, arg: str) -> "BuckOutputLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetUses.3 {{ locator = _, file = _, spans = _ }}", TargetUses

  @staticmethod
  def angle_query(*, locator: Tuple[()], file: Tuple[()], spans: Tuple[()]) -> "BuckTargetUses":
    raise Exception("this function can only be called from @angle_query")

class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Owner.3 {{ source = _, owner = _ }}", Owner

  @staticmethod
  def angle_query(*, source: Tuple[()], owner: Tuple[()]) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")

class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Target.2 {{ locator = _, type_ = _, defaultPlatform = _, labels = _ }}", Target

  @staticmethod
  def angle_query(*, locator: Tuple[()], type_: Tuple[()], defaultPlatform: Tuple[()], labels: Tuple[()]) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.DefinitionLocation.3 {{ definition = _, file = _, span = _ }}", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "BuckDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetIndexer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetIndexer.3 {{ name = _, target = _ }}", TargetIndexer

  @staticmethod
  def angle_query(*, name: Tuple[()], target: Tuple[()]) -> "BuckTargetIndexer":
    raise Exception("this function can only be called from @angle_query")

class BuckFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.FileDefinition.3 {{ file = _, definition = _ }}", FileDefinition

  @staticmethod
  def angle_query(*, file: Tuple[()], definition: Tuple[()]) -> "BuckFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Target.1 {{ repo = _, name = _, platform = _ }}", Target

  @staticmethod
  def angle_query(*, repo: Tuple[()], name: Tuple[()], platform: Tuple[()]) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckAttributeName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.AttributeName.3 {json.dumps(key)}", AttributeName

  @staticmethod
  def angle_query(*, arg: str) -> "BuckAttributeName":
    raise Exception("this function can only be called from @angle_query")

class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Owner.1 {{ source = _, owner = _ }}", Owner

  @staticmethod
  def angle_query(*, source: Tuple[()], owner: Tuple[()]) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")

class BuckRuleKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.RuleKey.1 {{ locator = _, ruleKey = _ }}", RuleKey

  @staticmethod
  def angle_query(*, locator: Tuple[()], ruleKey: str) -> "BuckRuleKey":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetLinkWhole(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetLinkWhole.1 {json.dumps(key)}", TargetLinkWhole

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "BuckTargetLinkWhole":
    raise Exception("this function can only be called from @angle_query")

class BuckDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Definition.3 {{ module = _, name = _ }}", Definition

  @staticmethod
  def angle_query(*, module: Tuple[()], name: str) -> "BuckDefinition":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetOut(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetOut.1 {{ target = _, file = _ }}", TargetOut

  @staticmethod
  def angle_query(*, target: Tuple[()], file: Tuple[()]) -> "BuckTargetOut":
    raise Exception("this function can only be called from @angle_query")

class BuckFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.FileXRefs.3 {{ file = _, xrefs = _ }}", FileXRefs

  @staticmethod
  def angle_query(*, file: Tuple[()], xrefs: Tuple[()]) -> "BuckFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetLocation.3 {{ locator = _, file = _, span = _ }}", TargetLocation

  @staticmethod
  def angle_query(*, locator: Tuple[()], file: Tuple[()], span: Tuple[()]) -> "BuckTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckDestinationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.DestinationUses.3 {{ destination = _, file = _, spans = _ }}", DestinationUses

  @staticmethod
  def angle_query(*, destination: Tuple[()], file: Tuple[()], spans: Tuple[()]) -> "BuckDestinationUses":
    raise Exception("this function can only be called from @angle_query")

class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TranslationUnit.2 {{ file = _, target = _, platform = _ }}", TranslationUnit

  @staticmethod
  def angle_query(*, file: Tuple[()], target: Tuple[()], platform: Tuple[()]) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.File.1 {json.dumps(key)}", File

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")

class BuckFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.FileTarget.3 {{ file = _, locator = _ }}", FileTarget

  @staticmethod
  def angle_query(*, file: Tuple[()], locator: Tuple[()]) -> "BuckFileTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TranslationUnit.1 {{ file = _, target = _ }}", TranslationUnit

  @staticmethod
  def angle_query(*, file: Tuple[()], target: Tuple[()]) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetIndexerName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.TargetIndexerName.3 {json.dumps(key)}", TargetIndexerName

  @staticmethod
  def angle_query(*, arg: str) -> "BuckTargetIndexerName":
    raise Exception("this function can only be called from @angle_query")

class BuckFileResolved(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.FileResolved.3 {{ buckFile = _, srcFile = _ }}", FileResolved

  @staticmethod
  def angle_query(*, buckFile: Tuple[()], srcFile: Tuple[()]) -> "BuckFileResolved":
    raise Exception("this function can only be called from @angle_query")

class BuckConsumer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.Consumer.3 {{ source = _, consumer = _ }}", Consumer

  @staticmethod
  def angle_query(*, source: Tuple[()], consumer: Tuple[()]) -> "BuckConsumer":
    raise Exception("this function can only be called from @angle_query")

class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"buck.File.3 {json.dumps(key)}", File

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")


