# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate , Just, InnerGleanSchemaPredicate
from glean.client.py3.angle_query import angle_for, R
from glean.schema.py.src import *
from glean.schema.py.sys import *


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
    ContbuildName,
    SourceFileLocation,
    OutputLabel,
    TargetUses,
    Owner,
    ContbuildLocator,
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
    XRef,
    AttributeMapping,
    XRefDestination,
    Dependency,
)


class BuckTargetHash(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, targetHash: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, targetHash, 'targetHash')]))
    return f"buck.TargetHash.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetHash

  @staticmethod
  def angle_query(*, locator: Optional["BuckLocator"] = None, targetHash: Optional[str] = None) -> "BuckTargetHash":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetSourcesBaseModule(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, srcs: ast.Expr, baseModule: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, srcs, 'srcs'), angle_for(__env, baseModule, 'baseModule')]))
    return f"buck.TargetSourcesBaseModule.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetSourcesBaseModule

  @staticmethod
  def angle_query(*, locator: Optional["BuckTarget"] = None, srcs: Optional[List["BuckFile"]] = None, baseModule: Optional[Union[Just["BuckAttributeValue"], Just[None]]] = None) -> "BuckTargetSourcesBaseModule":
    raise Exception("this function can only be called from @angle_query")



class BuckLocatorReverseDeps(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, rdeps: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, rdeps, 'rdeps')]))
    return f"buck.LocatorReverseDeps.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LocatorReverseDeps

  @staticmethod
  def angle_query(*, locator: Optional["BuckLocator"] = None, rdeps: Optional[List["BuckLocator"]] = None) -> "BuckLocatorReverseDeps":
    raise Exception("this function can only be called from @angle_query")



class BuckType(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.Type.1 { query_fields if query_fields else '_' }", Type

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckType":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetDependencies(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, dependencies: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, dependencies, 'dependencies')]))
    return f"buck.TargetDependencies.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetDependencies

  @staticmethod
  def angle_query(*, target: Optional["BuckTarget"] = None, dependencies: Optional[List["BuckDependency"]] = None) -> "BuckTargetDependencies":
    raise Exception("this function can only be called from @angle_query")



class BuckLocator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], subdir: ast.Expr, path: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, subdir, 'subdir'), angle_for(__env, path, 'path'), angle_for(__env, name, 'name')]))
    return f"buck.Locator.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Locator

  @staticmethod
  def angle_query(*, subdir: Optional[Union[Just[str], Just[None]]] = None, path: Optional[str] = None, name: Optional[str] = None) -> "BuckLocator":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, headers: ast.Expr, exportedHeaders: ast.Expr, srcs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, headers, 'headers'), angle_for(__env, exportedHeaders, 'exportedHeaders'), angle_for(__env, srcs, 'srcs')]))
    return f"buck.TargetSources.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetSources

  @staticmethod
  def angle_query(*, target: Optional["BuckTarget"] = None, headers: Optional[List["BuckFile_1"]] = None, exportedHeaders: Optional[List["BuckFile_1"]] = None, srcs: Optional[List["BuckFile_1"]] = None) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")



class BuckPlatform(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.Platform.1 { query_fields if query_fields else '_' }", Platform

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckPlatform":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetAttribute(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, attribute: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, attribute, 'attribute'), angle_for(__env, value, 'value')]))
    return f"buck.TargetAttribute.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetAttribute

  @staticmethod
  def angle_query(*, target: Optional["BuckTarget"] = None, attribute: Optional["BuckAttributeName"] = None, value: Optional["BuckAttributeValue"] = None) -> "BuckTargetAttribute":
    raise Exception("this function can only be called from @angle_query")



class BuckFileEntity(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, entity: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, entity, 'entity')]))
    return f"buck.FileEntity.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileEntity

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, entity: Optional["BuckXRefDestination"] = None) -> "BuckFileEntity":
    raise Exception("this function can only be called from @angle_query")



class BuckOutTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, target, 'target')]))
    return f"buck.OutTarget.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OutTarget

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, target: Optional["BuckTarget"] = None) -> "BuckOutTarget":
    raise Exception("this function can only be called from @angle_query")



class BuckLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.Label.1 { query_fields if query_fields else '_' }", Label

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckLabel":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetOuts(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, outputLabel: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, outputLabel, 'outputLabel'), angle_for(__env, file, 'file')]))
    return f"buck.TargetOuts.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetOuts

  @staticmethod
  def angle_query(*, target: Optional["BuckTarget"] = None, outputLabel: Optional[Union[Just["BuckOutputLabel"], Just[None]]] = None, file: Optional["SrcFile"] = None) -> "BuckTargetOuts":
    raise Exception("this function can only be called from @angle_query")



class BuckOutsTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, target: ast.Expr, outputLabel: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, target, 'target'), angle_for(__env, outputLabel, 'outputLabel')]))
    return f"buck.OutsTarget.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", OutsTarget

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, target: Optional["BuckTarget"] = None, outputLabel: Optional[Union[Just["BuckOutputLabel"], Just[None]]] = None) -> "BuckOutsTarget":
    raise Exception("this function can only be called from @angle_query")



class BuckLocatorWithLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, label: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, label, 'label')]))
    return f"buck.LocatorWithLabel.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", LocatorWithLabel

  @staticmethod
  def angle_query(*, locator: Optional["BuckLocator"] = None, label: Optional["BuckOutputLabel"] = None) -> "BuckLocatorWithLabel":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetSources(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, headers: ast.Expr, exportedHeaders: ast.Expr, srcs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, headers, 'headers'), angle_for(__env, exportedHeaders, 'exportedHeaders'), angle_for(__env, srcs, 'srcs')]))
    return f"buck.TargetSources.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetSources

  @staticmethod
  def angle_query(*, target: Optional["BuckTarget"] = None, headers: Optional[List["BuckFile"]] = None, exportedHeaders: Optional[List["BuckFile"]] = None, srcs: Optional[List["BuckFile"]] = None) -> "BuckTargetSources":
    raise Exception("this function can only be called from @angle_query")



class BuckAttributeValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], _str: ast.Expr, sequence: ast.Expr, mapping: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, _str, 'str'), angle_for(__env, sequence, 'sequence'), angle_for(__env, mapping, 'mapping')]))
    return f"buck.AttributeValue.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", AttributeValue

  @staticmethod
  def angle_query__str(*, _str: Optional[str] = None) -> "BuckAttributeValue":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_sequence(*, sequence: Optional[List["BuckAttributeValue"]] = None) -> "BuckAttributeValue":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_mapping(*, mapping: Optional[List["BuckAttributeMapping"]] = None) -> "BuckAttributeValue":
    raise Exception("this function can only be called from @angle_query")




class BuckLabels(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.Labels.1 { query_fields if query_fields else '_' }", Labels

  @staticmethod
  def angle_query(*, arg: Optional[List["BuckLabel"]] = None) -> "BuckLabels":
    raise Exception("this function can only be called from @angle_query")



class BuckContbuildName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.ContbuildName.3 { query_fields if query_fields else '_' }", ContbuildName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckContbuildName":
    raise Exception("this function can only be called from @angle_query")



class BuckSourceFileLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"buck.SourceFileLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", SourceFileLocation

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "BuckSourceFileLocation":
    raise Exception("this function can only be called from @angle_query")



class BuckOutputLabel(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.OutputLabel.3 { query_fields if query_fields else '_' }", OutputLabel

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckOutputLabel":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, file: ast.Expr, spans: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, file, 'file'), angle_for(__env, spans, 'spans')]))
    return f"buck.TargetUses.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetUses

  @staticmethod
  def angle_query(*, locator: Optional["BuckLocator"] = None, file: Optional["SrcFile"] = None, spans: Optional[List["SrcByteSpan"]] = None) -> "BuckTargetUses":
    raise Exception("this function can only be called from @angle_query")



class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, owner: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, owner, 'owner')]))
    return f"buck.Owner.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Owner

  @staticmethod
  def angle_query(*, source: Optional["SrcFile"] = None, owner: Optional["BuckTargetSources"] = None) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")



class BuckContbuildLocator(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, name, 'name')]))
    return f"buck.ContbuildLocator.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", ContbuildLocator

  @staticmethod
  def angle_query(*, target: Optional["BuckLocator"] = None, name: Optional["BuckContbuildName"] = None) -> "BuckContbuildLocator":
    raise Exception("this function can only be called from @angle_query")



class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, type_: ast.Expr, defaultPlatform: ast.Expr, labels: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, type_, 'type_'), angle_for(__env, defaultPlatform, 'defaultPlatform'), angle_for(__env, labels, 'labels')]))
    return f"buck.Target.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Target

  @staticmethod
  def angle_query(*, locator: Optional["BuckLocator"] = None, type_: Optional["BuckType"] = None, defaultPlatform: Optional[Union[Just["BuckPlatform"], Just[None]]] = None, labels: Optional["BuckLabels"] = None) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")



class BuckDefinitionLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], definition: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, definition, 'definition'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"buck.DefinitionLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DefinitionLocation

  @staticmethod
  def angle_query(*, definition: Optional["BuckDefinition"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "BuckDefinitionLocation":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetIndexer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, target, 'target')]))
    return f"buck.TargetIndexer.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetIndexer

  @staticmethod
  def angle_query(*, name: Optional["BuckTargetIndexerName"] = None, target: Optional["BuckTarget"] = None) -> "BuckTargetIndexer":
    raise Exception("this function can only be called from @angle_query")



class BuckFileDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, definition, 'definition')]))
    return f"buck.FileDefinition.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileDefinition

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, definition: Optional["BuckDefinition"] = None) -> "BuckFileDefinition":
    raise Exception("this function can only be called from @angle_query")



class BuckTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], repo: ast.Expr, name: ast.Expr, platform: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, repo, 'repo'), angle_for(__env, name, 'name'), angle_for(__env, platform, 'platform')]))
    return f"buck.Target.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Target

  @staticmethod
  def angle_query(*, repo: Optional["SysBlob"] = None, name: Optional["SysBlob"] = None, platform: Optional[Union[Just["SysBlob"], Just[None]]] = None) -> "BuckTarget":
    raise Exception("this function can only be called from @angle_query")



class BuckAttributeName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.AttributeName.3 { query_fields if query_fields else '_' }", AttributeName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckAttributeName":
    raise Exception("this function can only be called from @angle_query")



class BuckOwner(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, owner: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, owner, 'owner')]))
    return f"buck.Owner.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Owner

  @staticmethod
  def angle_query(*, source: Optional["SrcFile"] = None, owner: Optional["BuckTargetSources_1"] = None) -> "BuckOwner":
    raise Exception("this function can only be called from @angle_query")



class BuckRuleKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, ruleKey: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, ruleKey, 'ruleKey')]))
    return f"buck.RuleKey.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", RuleKey

  @staticmethod
  def angle_query(*, locator: Optional["BuckLocator"] = None, ruleKey: Optional[str] = None) -> "BuckRuleKey":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetLinkWhole(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.TargetLinkWhole.1 { query_fields if query_fields else '_' }", TargetLinkWhole

  @staticmethod
  def angle_query(*, arg: Optional["BuckTarget"] = None) -> "BuckTargetLinkWhole":
    raise Exception("this function can only be called from @angle_query")



class BuckDefinition(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], module: ast.Expr, name: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, module, 'module'), angle_for(__env, name, 'name')]))
    return f"buck.Definition.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Definition

  @staticmethod
  def angle_query(*, module: Optional["SrcFile"] = None, name: Optional[str] = None) -> "BuckDefinition":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetOut(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, file: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, file, 'file')]))
    return f"buck.TargetOut.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetOut

  @staticmethod
  def angle_query(*, target: Optional["BuckTarget"] = None, file: Optional["SrcFile"] = None) -> "BuckTargetOut":
    raise Exception("this function can only be called from @angle_query")



class BuckFileXRefs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, xrefs: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, xrefs, 'xrefs')]))
    return f"buck.FileXRefs.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileXRefs

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, xrefs: Optional[List["BuckXRef"]] = None) -> "BuckFileXRefs":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetLocation(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, file: ast.Expr, span: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, file, 'file'), angle_for(__env, span, 'span')]))
    return f"buck.TargetLocation.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TargetLocation

  @staticmethod
  def angle_query(*, locator: Optional["BuckLocator"] = None, file: Optional["SrcFile"] = None, span: Optional["SrcByteSpan"] = None) -> "BuckTargetLocation":
    raise Exception("this function can only be called from @angle_query")



class BuckDestinationUses(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], destination: ast.Expr, file: ast.Expr, spans: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, destination, 'destination'), angle_for(__env, file, 'file'), angle_for(__env, spans, 'spans')]))
    return f"buck.DestinationUses.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", DestinationUses

  @staticmethod
  def angle_query(*, destination: Optional["BuckXRefDestination"] = None, file: Optional["SrcFile"] = None, spans: Optional[List["SrcByteSpan"]] = None) -> "BuckDestinationUses":
    raise Exception("this function can only be called from @angle_query")



class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, target: ast.Expr, platform: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, target, 'target'), angle_for(__env, platform, 'platform')]))
    return f"buck.TranslationUnit.2 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TranslationUnit

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, target: Optional["BuckLocator"] = None, platform: Optional[Union[Just["BuckPlatform"], Just[None]]] = None) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")



class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, generated: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, generated, 'generated')]))
    return f"buck.File.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", File

  @staticmethod
  def angle_query_source(*, source: Optional["SrcFile"] = None) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_generated(*, generated: Optional["BuckLocator"] = None) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")




class BuckFileTarget(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, locator: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, locator, 'locator')]))
    return f"buck.FileTarget.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileTarget

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, locator: Optional["BuckLocator"] = None) -> "BuckFileTarget":
    raise Exception("this function can only be called from @angle_query")



class BuckTranslationUnit(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], file: ast.Expr, target: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, file, 'file'), angle_for(__env, target, 'target')]))
    return f"buck.TranslationUnit.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", TranslationUnit

  @staticmethod
  def angle_query(*, file: Optional["SrcFile"] = None, target: Optional["BuckTarget_1"] = None) -> "BuckTranslationUnit":
    raise Exception("this function can only be called from @angle_query")



class BuckTargetIndexerName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  angle_for(__env, arg, None)
    return f"buck.TargetIndexerName.3 { query_fields if query_fields else '_' }", TargetIndexerName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "BuckTargetIndexerName":
    raise Exception("this function can only be called from @angle_query")



class BuckFileResolved(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], buckFile: ast.Expr, srcFile: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, buckFile, 'buckFile'), angle_for(__env, srcFile, 'srcFile')]))
    return f"buck.FileResolved.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", FileResolved

  @staticmethod
  def angle_query(*, buckFile: Optional["BuckFile"] = None, srcFile: Optional["SrcFile"] = None) -> "BuckFileResolved":
    raise Exception("this function can only be called from @angle_query")



class BuckConsumer(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, consumer: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, consumer, 'consumer')]))
    return f"buck.Consumer.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Consumer

  @staticmethod
  def angle_query(*, source: Optional["SrcFile"] = None, consumer: Optional["BuckTargetSources"] = None) -> "BuckConsumer":
    raise Exception("this function can only be called from @angle_query")



class BuckFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], source: ast.Expr, generated: ast.Expr, generatedLabel: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, source, 'source'), angle_for(__env, generated, 'generated'), angle_for(__env, generatedLabel, 'generatedLabel')]))
    return f"buck.File.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", File

  @staticmethod
  def angle_query_source(*, source: Optional["SrcFile"] = None) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_generated(*, generated: Optional["BuckLocator"] = None) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_generatedLabel(*, generatedLabel: Optional["BuckLocatorWithLabel"] = None) -> "BuckFile":
    raise Exception("this function can only be called from @angle_query")






class BuckXRef(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], destination: ast.Expr, ranges: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, destination, 'destination'), angle_for(__env, ranges, 'ranges')]))
    return f"buck.XRef.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRef

  @staticmethod
  def angle_query(*, destination: Optional["BuckXRefDestination"] = None, ranges: Optional[List["SrcByteSpan"]] = None) -> "BuckXRef":
    raise Exception("this function can only be called from @angle_query")



class BuckAttributeMapping(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, value: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, key, 'key'), angle_for(__env, value, 'value')]))
    return f"buck.AttributeMapping.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", AttributeMapping

  @staticmethod
  def angle_query(*, key: Optional[str] = None, value: Optional["BuckAttributeValue"] = None) -> "BuckAttributeMapping":
    raise Exception("this function can only be called from @angle_query")



class BuckXRefDestination(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], locator: ast.Expr, file: ast.Expr, definition: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, locator, 'locator'), angle_for(__env, file, 'file'), angle_for(__env, definition, 'definition')]))
    return f"buck.XRefDestination.3 { ('{ ' + query_fields + ' }') if query_fields else '_' }", XRefDestination

  @staticmethod
  def angle_query_locator(*, locator: Optional["BuckLocator"] = None) -> "BuckXRefDestination":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_file(*, file: Optional["SrcFile"] = None) -> "BuckXRefDestination":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_definition(*, definition: Optional["BuckDefinition"] = None) -> "BuckXRefDestination":
    raise Exception("this function can only be called from @angle_query")




class BuckDependency(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], target: ast.Expr, explicit_: ast.Expr, exported: ast.Expr) -> Tuple[str, Struct]:
    query_fields =  ', '.join(filter(lambda x: x != '', [angle_for(__env, target, 'target'), angle_for(__env, explicit_, 'explicit_'), angle_for(__env, exported, 'exported')]))
    return f"buck.Dependency.1 { ('{ ' + query_fields + ' }') if query_fields else '_' }", Dependency

  @staticmethod
  def angle_query(*, target: Optional["BuckLocator"] = None, explicit_: Optional[bool] = None, exported: Optional[bool] = None) -> "BuckDependency":
    raise Exception("this function can only be called from @angle_query")




