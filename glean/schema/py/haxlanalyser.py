# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R
from glean.schema.py.src import *


from glean.schema.haxlanalyser.types import (
    ThriftResponse,
    TallyConfig,
    PolicyName,
    RestrictionResponse,
    Context,
    EndpointName,
    JankyJSONResponse,
    ClassifierRead,
    ScubaResponse,
    FeatureSetFeature,
    SitevarFetch,
    Call,
    ACDCProperty,
    Edge,
    Endpoint,
    TallyFetch,
    Policy,
    ContextName,
    TestFile,
    ThriftFetch,
    InputKey,
    PiranhaResponse,
    ACDCPropertyAccess,
    TallyResponse,
    ScribeResponse,
    InputFetch,
    ClassifierFetch,
    LogFeatureResponse,
    LaserDataset,
    TallyName,
    ConfigeratorFetch,
    HiveResponse,
)


class HaxlanalyserThriftResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ThriftResponse.1 {angle_for(__env, arg)}", ThriftResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserThriftResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyConfig.1 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", TallyConfig

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserTallyName"] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserTallyConfig":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.PolicyName.1 {angle_for(__env, arg)}", PolicyName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserPolicyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserRestrictionResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.RestrictionResponse.1 {angle_for(__env, arg)}", RestrictionResponse

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "HaxlanalyserRestrictionResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Context.1 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", Context

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserContextName"] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserContext":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpointName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.EndpointName.1 {angle_for(__env, arg)}", EndpointName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserEndpointName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserJankyJSONResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.JankyJSONResponse.1 {angle_for(__env, arg)}", JankyJSONResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserJankyJSONResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierRead(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ClassifierRead.1 {angle_for(__env, arg)}", ClassifierRead

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserClassifierRead":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScubaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ScubaResponse.1 {angle_for(__env, arg)}", ScubaResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserScubaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserFeatureSetFeature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.FeatureSetFeature.1 {angle_for(__env, arg)}", FeatureSetFeature

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserFeatureSetFeature":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserSitevarFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.SitevarFetch.1 {angle_for(__env, arg)}", SitevarFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserSitevarFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], ref: ast.Expr, qname: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Call.1 {{ ref = {angle_for(__env, ref)}, qname = {angle_for(__env, qname)}, loc = {angle_for(__env, loc)} }}", Call

  @staticmethod
  def angle_query(*, ref: Optional[str] = None, qname: Optional[str] = None, loc: Optional[Tuple[()]] = None) -> "HaxlanalyserCall":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCProperty(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ACDCProperty.1 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", ACDCProperty

  @staticmethod
  def angle_query(*, name: Optional[str] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserACDCProperty":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], origin: ast.Expr, dest: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Edge.1 {{ origin = {angle_for(__env, origin)}, dest = {angle_for(__env, dest)} }}", Edge

  @staticmethod
  def angle_query(*, origin: Optional[Tuple[()]] = None, dest: Optional[Tuple[()]] = None) -> "HaxlanalyserEdge":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Endpoint.1 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", Endpoint

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserEndpointName"] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserEndpoint":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyFetch.1 {angle_for(__env, arg)}", TallyFetch

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HaxlanalyserTallyFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Policy.1 {{ name = {angle_for(__env, name)}, source = {angle_for(__env, source)} }}", Policy

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserPolicyName"] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserPolicy":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContextName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ContextName.1 {angle_for(__env, arg)}", ContextName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserContextName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTestFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TestFile.1 {angle_for(__env, arg)}", TestFile

  @staticmethod
  def angle_query(*, arg: Optional["SrcFile"] = None) -> "HaxlanalyserTestFile":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserThriftFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ThriftFetch.1 {angle_for(__env, arg)}", ThriftFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserThriftFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.InputKey.1 {angle_for(__env, arg)}", InputKey

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserInputKey":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPiranhaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.PiranhaResponse.1 {angle_for(__env, arg)}", PiranhaResponse

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "HaxlanalyserPiranhaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCPropertyAccess(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ACDCPropertyAccess.1 {angle_for(__env, arg)}", ACDCPropertyAccess

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserACDCPropertyAccess":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyResponse.1 {angle_for(__env, arg)}", TallyResponse

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HaxlanalyserTallyResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScribeResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ScribeResponse.1 {angle_for(__env, arg)}", ScribeResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserScribeResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.InputFetch.1 {{ key = {angle_for(__env, key)}, source = {angle_for(__env, source)} }}", InputFetch

  @staticmethod
  def angle_query(*, key: Optional["HaxlanalyserInputKey"] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserInputFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ClassifierFetch.1 {angle_for(__env, arg)}", ClassifierFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserClassifierFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLogFeatureResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.LogFeatureResponse.1 {angle_for(__env, arg)}", LogFeatureResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserLogFeatureResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLaserDataset(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.LaserDataset.1 {angle_for(__env, arg)}", LaserDataset

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserLaserDataset":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyName.1 {angle_for(__env, arg)}", TallyName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserTallyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserConfigeratorFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ConfigeratorFetch.1 {angle_for(__env, arg)}", ConfigeratorFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserConfigeratorFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserHiveResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.HiveResponse.1 {angle_for(__env, arg)}", HiveResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserHiveResponse":
    raise Exception("this function can only be called from @angle_query")


