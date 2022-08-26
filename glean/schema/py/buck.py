# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetHash.1 {{ }}", TargetHash
    return f"buck.TargetHash.1 { concatenateFields(key) }", TargetHash

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, targetHash: Optional[str] = None) -> "BuckTargetHash":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSourcesBaseModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetSourcesBaseModule.3 {{ }}", TargetSourcesBaseModule
    return f"buck.TargetSourcesBaseModule.3 { concatenateFields(key) }", TargetSourcesBaseModule

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, srcs: Optional[Tuple[()]] = None, baseModule: Optional[Tuple[()]] = None) -> "BuckTargetSourcesBaseModule":
    raise Exception("this function can only be called from @angle_query")

class BuckLocatorReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.LocatorReverseDeps.1 {{ }}", LocatorReverseDeps
    return f"buck.LocatorReverseDeps.1 { concatenateFields(key) }", LocatorReverseDeps

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, rdeps: Optional[Tuple[()]] = None) -> "BuckLocatorReverseDeps":
    raise Exception("this function can only be called from @angle_query")

class BuckType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Type.1 {{ }}", Type
    return f"buck.Type.1 {key}", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckType":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetDependencies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetDependencies.1 {{ }}", TargetDependencies
    return f"buck.TargetDependencies.1 { concatenateFields(key) }", TargetDependencies

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, dependencies: Optional[Tuple[()]] = None) -> "BuckTargetDependencies":
    raise Exception("this function can only be called from @angle_query")

class BuckLocator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Locator.1 {{ }}", Locator
    return f"buck.Locator.1 { concatenateFields(key) }", Locator

  @staticmethod
  def angle_query(*, subdir: Optional[Tuple[()]] = None, path: Optional[str] = None, name: Optional[str] = None) -> "BuckLocator":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetSources.1 {{ }}", TargetSources
    return f"buck.TargetSources.1 { concatenateFields(key) }", TargetSources

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, headers: Optional[Tuple[()]] = None, exportedHeaders: Optional[Tuple[()]] = None, srcs: Optional[Tuple[()]] = None) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class BuckPlatform(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Platform.1 {{ }}", Platform
    return f"buck.Platform.1 {key}", Platform

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckPlatform":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetAttribute.3 {{ }}", TargetAttribute
    return f"buck.TargetAttribute.3 { concatenateFields(key) }", TargetAttribute

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, attribute: Optional[Tuple[()]] = None, value: Optional[Tuple[()]] = None) -> "BuckTargetAttribute":
    raise Exception("this function can only be called from @angle_query")

class BuckFileEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.FileEntity.3 {{ }}", FileEntity
    return f"buck.FileEntity.3 { concatenateFields(key) }", FileEntity

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, entity: Optional[Tuple[()]] = None) -> "BuckFileEntity":
    raise Exception("this function can only be called from @angle_query")

class BuckOutTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.OutTarget.1 {{ }}", OutTarget
    return f"buck.OutTarget.1 { concatenateFields(key) }", OutTarget

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "BuckOutTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Label.1 {{ }}", Label
    return f"buck.Label.1 {key}", Label

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetOuts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetOuts.3 {{ }}", TargetOuts
    return f"buck.TargetOuts.3 { concatenateFields(key) }", TargetOuts

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, outputLabel: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "BuckTargetOuts":
    raise Exception("this function can only be called from @angle_query")

class BuckOutsTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.OutsTarget.3 {{ }}", OutsTarget
    return f"buck.OutsTarget.3 { concatenateFields(key) }", OutsTarget

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, outputLabel: Optional[Tuple[()]] = None) -> "BuckOutsTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckLocatorWithLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.LocatorWithLabel.3 {{ }}", LocatorWithLabel
    return f"buck.LocatorWithLabel.3 { concatenateFields(key) }", LocatorWithLabel

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, label: Optional[Tuple[()]] = None) -> "BuckLocatorWithLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetSources.3 {{ }}", TargetSources
    return f"buck.TargetSources.3 { concatenateFields(key) }", TargetSources

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, headers: Optional[Tuple[()]] = None, exportedHeaders: Optional[Tuple[()]] = None, srcs: Optional[Tuple[()]] = None) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")

class BuckAttributeValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.AttributeValue.3 {{ }}", AttributeValue
    return f"buck.AttributeValue.3 {key}", AttributeValue

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "BuckAttributeValue":
    raise Exception("this function can only be called from @angle_query")

class BuckLabels(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Labels.1 {{ }}", Labels
    return f"buck.Labels.1 {key}", Labels

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "BuckLabels":
    raise Exception("this function can only be called from @angle_query")

class BuckSourceFileLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.SourceFileLocation.3 {{ }}", SourceFileLocation
    return f"buck.SourceFileLocation.3 { concatenateFields(key) }", SourceFileLocation

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "BuckSourceFileLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckOutputLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.OutputLabel.3 {{ }}", OutputLabel
    return f"buck.OutputLabel.3 {key}", OutputLabel

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckOutputLabel":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetUses.3 {{ }}", TargetUses
    return f"buck.TargetUses.3 { concatenateFields(key) }", TargetUses

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, spans: Optional[Tuple[()]] = None) -> "BuckTargetUses":
    raise Exception("this function can only be called from @angle_query")

class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Owner.3 {{ }}", Owner
    return f"buck.Owner.3 { concatenateFields(key) }", Owner

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, owner: Optional[Tuple[()]] = None) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")

class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Target.2 {{ }}", Target
    return f"buck.Target.2 { concatenateFields(key) }", Target

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, type_: Optional[Tuple[()]] = None, defaultPlatform: Optional[Tuple[()]] = None, labels: Optional[Tuple[()]] = None) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.DefinitionLocation.3 {{ }}", DefinitionLocation
    return f"buck.DefinitionLocation.3 { concatenateFields(key) }", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "BuckDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetIndexer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetIndexer.3 {{ }}", TargetIndexer
    return f"buck.TargetIndexer.3 { concatenateFields(key) }", TargetIndexer

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "BuckTargetIndexer":
    raise Exception("this function can only be called from @angle_query")

class BuckFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.FileDefinition.3 {{ }}", FileDefinition
    return f"buck.FileDefinition.3 { concatenateFields(key) }", FileDefinition

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, definition: Optional[Tuple[()]] = None) -> "BuckFileDefinition":
    raise Exception("this function can only be called from @angle_query")

class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Target.1 {{ }}", Target
    return f"buck.Target.1 { concatenateFields(key) }", Target

  @staticmethod
  def angle_query(*, repo: Optional[Tuple[()]] = None, name: Optional[Tuple[()]] = None, platform: Optional[Tuple[()]] = None) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckAttributeName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.AttributeName.3 {{ }}", AttributeName
    return f"buck.AttributeName.3 {key}", AttributeName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckAttributeName":
    raise Exception("this function can only be called from @angle_query")

class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Owner.1 {{ }}", Owner
    return f"buck.Owner.1 { concatenateFields(key) }", Owner

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, owner: Optional[Tuple[()]] = None) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")

class BuckRuleKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.RuleKey.1 {{ }}", RuleKey
    return f"buck.RuleKey.1 { concatenateFields(key) }", RuleKey

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, ruleKey: Optional[str] = None) -> "BuckRuleKey":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetLinkWhole(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetLinkWhole.1 {{ }}", TargetLinkWhole
    return f"buck.TargetLinkWhole.1 {key}", TargetLinkWhole

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "BuckTargetLinkWhole":
    raise Exception("this function can only be called from @angle_query")

class BuckDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Definition.3 {{ }}", Definition
    return f"buck.Definition.3 { concatenateFields(key) }", Definition

  @staticmethod
  def angle_query(*, module: Optional[Tuple[()]] = None, name: Optional[str] = None) -> "BuckDefinition":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetOut(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetOut.1 {{ }}", TargetOut
    return f"buck.TargetOut.1 { concatenateFields(key) }", TargetOut

  @staticmethod
  def angle_query(*, target: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None) -> "BuckTargetOut":
    raise Exception("this function can only be called from @angle_query")

class BuckFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.FileXRefs.3 {{ }}", FileXRefs
    return f"buck.FileXRefs.3 { concatenateFields(key) }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, xrefs: Optional[Tuple[()]] = None) -> "BuckFileXRefs":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetLocation.3 {{ }}", TargetLocation
    return f"buck.TargetLocation.3 { concatenateFields(key) }", TargetLocation

  @staticmethod
  def angle_query(*, locator: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, span: Optional[Tuple[()]] = None) -> "BuckTargetLocation":
    raise Exception("this function can only be called from @angle_query")

class BuckDestinationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.DestinationUses.3 {{ }}", DestinationUses
    return f"buck.DestinationUses.3 { concatenateFields(key) }", DestinationUses

  @staticmethod
  def angle_query(*, destination: Optional[Tuple[()]] = None, file: Optional[Tuple[()]] = None, spans: Optional[Tuple[()]] = None) -> "BuckDestinationUses":
    raise Exception("this function can only be called from @angle_query")

class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TranslationUnit.2 {{ }}", TranslationUnit
    return f"buck.TranslationUnit.2 { concatenateFields(key) }", TranslationUnit

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None, platform: Optional[Tuple[()]] = None) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.File.1 {{ }}", File
    return f"buck.File.1 {key}", File

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")

class BuckFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.FileTarget.3 {{ }}", FileTarget
    return f"buck.FileTarget.3 { concatenateFields(key) }", FileTarget

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, locator: Optional[Tuple[()]] = None) -> "BuckFileTarget":
    raise Exception("this function can only be called from @angle_query")

class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TranslationUnit.1 {{ }}", TranslationUnit
    return f"buck.TranslationUnit.1 { concatenateFields(key) }", TranslationUnit

  @staticmethod
  def angle_query(*, file: Optional[Tuple[()]] = None, target: Optional[Tuple[()]] = None) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")

class BuckTargetIndexerName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.TargetIndexerName.3 {{ }}", TargetIndexerName
    return f"buck.TargetIndexerName.3 {key}", TargetIndexerName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckTargetIndexerName":
    raise Exception("this function can only be called from @angle_query")

class BuckFileResolved(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.FileResolved.3 {{ }}", FileResolved
    return f"buck.FileResolved.3 { concatenateFields(key) }", FileResolved

  @staticmethod
  def angle_query(*, buckFile: Optional[Tuple[()]] = None, srcFile: Optional[Tuple[()]] = None) -> "BuckFileResolved":
    raise Exception("this function can only be called from @angle_query")

class BuckConsumer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.Consumer.3 {{ }}", Consumer
    return f"buck.Consumer.3 { concatenateFields(key) }", Consumer

  @staticmethod
  def angle_query(*, source: Optional[Tuple[()]] = None, consumer: Optional[Tuple[()]] = None) -> "BuckConsumer":
    raise Exception("this function can only be called from @angle_query")

class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"buck.File.3 {{ }}", File
    return f"buck.File.3 {key}", File

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")


