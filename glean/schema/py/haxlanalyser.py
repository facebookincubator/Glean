# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


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
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ThriftResponse.1 {json.dumps(key)}", ThriftResponse

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserThriftResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyConfig.1 {{ name = _, source = _ }}", TallyConfig

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HaxlanalyserTallyConfig":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.PolicyName.1 {json.dumps(key)}", PolicyName

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserPolicyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserRestrictionResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.RestrictionResponse.1 {json.dumps(key)}", RestrictionResponse

  @staticmethod
  def angle_query(*, arg: int) -> "HaxlanalyserRestrictionResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.Context.1 {{ name = _, source = _ }}", Context

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HaxlanalyserContext":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpointName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.EndpointName.1 {json.dumps(key)}", EndpointName

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserEndpointName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserJankyJSONResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.JankyJSONResponse.1 {json.dumps(key)}", JankyJSONResponse

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserJankyJSONResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierRead(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ClassifierRead.1 {json.dumps(key)}", ClassifierRead

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserClassifierRead":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScubaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ScubaResponse.1 {json.dumps(key)}", ScubaResponse

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserScubaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserFeatureSetFeature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.FeatureSetFeature.1 {json.dumps(key)}", FeatureSetFeature

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserFeatureSetFeature":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserSitevarFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.SitevarFetch.1 {json.dumps(key)}", SitevarFetch

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserSitevarFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.Call.1 {{ ref = _, qname = _, loc = _ }}", Call

  @staticmethod
  def angle_query(*, ref: str, qname: str, loc: Tuple[()]) -> "HaxlanalyserCall":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCProperty(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ACDCProperty.1 {{ name = _, source = _ }}", ACDCProperty

  @staticmethod
  def angle_query(*, name: str, source: Tuple[()]) -> "HaxlanalyserACDCProperty":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.Edge.1 {{ origin = _, dest = _ }}", Edge

  @staticmethod
  def angle_query(*, origin: Tuple[()], dest: Tuple[()]) -> "HaxlanalyserEdge":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.Endpoint.1 {{ name = _, source = _ }}", Endpoint

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HaxlanalyserEndpoint":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyFetch.1 {json.dumps(key)}", TallyFetch

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "HaxlanalyserTallyFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.Policy.1 {{ name = _, source = _ }}", Policy

  @staticmethod
  def angle_query(*, name: Tuple[()], source: Tuple[()]) -> "HaxlanalyserPolicy":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContextName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ContextName.1 {json.dumps(key)}", ContextName

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserContextName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTestFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.TestFile.1 {json.dumps(key)}", TestFile

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "HaxlanalyserTestFile":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserThriftFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ThriftFetch.1 {json.dumps(key)}", ThriftFetch

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserThriftFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.InputKey.1 {json.dumps(key)}", InputKey

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserInputKey":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPiranhaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.PiranhaResponse.1 {json.dumps(key)}", PiranhaResponse

  @staticmethod
  def angle_query(*, arg: int) -> "HaxlanalyserPiranhaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCPropertyAccess(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ACDCPropertyAccess.1 {json.dumps(key)}", ACDCPropertyAccess

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserACDCPropertyAccess":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyResponse.1 {json.dumps(key)}", TallyResponse

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "HaxlanalyserTallyResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScribeResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ScribeResponse.1 {json.dumps(key)}", ScribeResponse

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserScribeResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.InputFetch.1 {{ key = _, source = _ }}", InputFetch

  @staticmethod
  def angle_query(*, key: Tuple[()], source: Tuple[()]) -> "HaxlanalyserInputFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ClassifierFetch.1 {json.dumps(key)}", ClassifierFetch

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserClassifierFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLogFeatureResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.LogFeatureResponse.1 {json.dumps(key)}", LogFeatureResponse

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserLogFeatureResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLaserDataset(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.LaserDataset.1 {json.dumps(key)}", LaserDataset

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserLaserDataset":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyName.1 {json.dumps(key)}", TallyName

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserTallyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserConfigeratorFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.ConfigeratorFetch.1 {json.dumps(key)}", ConfigeratorFetch

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserConfigeratorFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserHiveResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"haxlanalyser.HiveResponse.1 {json.dumps(key)}", HiveResponse

  @staticmethod
  def angle_query(*, arg: str) -> "HaxlanalyserHiveResponse":
    raise Exception("this function can only be called from @angle_query")


