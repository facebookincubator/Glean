# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ThriftResponse.1 {{ }}", ThriftResponse
    return f"haxlanalyser.ThriftResponse.1 {key}", ThriftResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserThriftResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.TallyConfig.1 {{ }}", TallyConfig
    return f"haxlanalyser.TallyConfig.1 { concatenateFields(key) }", TallyConfig

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserTallyConfig":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.PolicyName.1 {{ }}", PolicyName
    return f"haxlanalyser.PolicyName.1 {key}", PolicyName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserPolicyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserRestrictionResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.RestrictionResponse.1 {{ }}", RestrictionResponse
    return f"haxlanalyser.RestrictionResponse.1 {key}", RestrictionResponse

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "HaxlanalyserRestrictionResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.Context.1 {{ }}", Context
    return f"haxlanalyser.Context.1 { concatenateFields(key) }", Context

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserContext":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpointName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.EndpointName.1 {{ }}", EndpointName
    return f"haxlanalyser.EndpointName.1 {key}", EndpointName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserEndpointName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserJankyJSONResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.JankyJSONResponse.1 {{ }}", JankyJSONResponse
    return f"haxlanalyser.JankyJSONResponse.1 {key}", JankyJSONResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserJankyJSONResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierRead(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ClassifierRead.1 {{ }}", ClassifierRead
    return f"haxlanalyser.ClassifierRead.1 {key}", ClassifierRead

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserClassifierRead":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScubaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ScubaResponse.1 {{ }}", ScubaResponse
    return f"haxlanalyser.ScubaResponse.1 {key}", ScubaResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserScubaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserFeatureSetFeature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.FeatureSetFeature.1 {{ }}", FeatureSetFeature
    return f"haxlanalyser.FeatureSetFeature.1 {key}", FeatureSetFeature

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserFeatureSetFeature":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserSitevarFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.SitevarFetch.1 {{ }}", SitevarFetch
    return f"haxlanalyser.SitevarFetch.1 {key}", SitevarFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserSitevarFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.Call.1 {{ }}", Call
    return f"haxlanalyser.Call.1 { concatenateFields(key) }", Call

  @staticmethod
  def angle_query(*, ref: Optional[str] = None, qname: Optional[str] = None, loc: Optional[Tuple[()]] = None) -> "HaxlanalyserCall":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCProperty(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ACDCProperty.1 {{ }}", ACDCProperty
    return f"haxlanalyser.ACDCProperty.1 { concatenateFields(key) }", ACDCProperty

  @staticmethod
  def angle_query(*, name: Optional[str] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserACDCProperty":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.Edge.1 {{ }}", Edge
    return f"haxlanalyser.Edge.1 { concatenateFields(key) }", Edge

  @staticmethod
  def angle_query(*, origin: Optional[Tuple[()]] = None, dest: Optional[Tuple[()]] = None) -> "HaxlanalyserEdge":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.Endpoint.1 {{ }}", Endpoint
    return f"haxlanalyser.Endpoint.1 { concatenateFields(key) }", Endpoint

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserEndpoint":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.TallyFetch.1 {{ }}", TallyFetch
    return f"haxlanalyser.TallyFetch.1 {key}", TallyFetch

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HaxlanalyserTallyFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.Policy.1 {{ }}", Policy
    return f"haxlanalyser.Policy.1 { concatenateFields(key) }", Policy

  @staticmethod
  def angle_query(*, name: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserPolicy":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContextName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ContextName.1 {{ }}", ContextName
    return f"haxlanalyser.ContextName.1 {key}", ContextName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserContextName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTestFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.TestFile.1 {{ }}", TestFile
    return f"haxlanalyser.TestFile.1 {key}", TestFile

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HaxlanalyserTestFile":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserThriftFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ThriftFetch.1 {{ }}", ThriftFetch
    return f"haxlanalyser.ThriftFetch.1 {key}", ThriftFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserThriftFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.InputKey.1 {{ }}", InputKey
    return f"haxlanalyser.InputKey.1 {key}", InputKey

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserInputKey":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPiranhaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.PiranhaResponse.1 {{ }}", PiranhaResponse
    return f"haxlanalyser.PiranhaResponse.1 {key}", PiranhaResponse

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "HaxlanalyserPiranhaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCPropertyAccess(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ACDCPropertyAccess.1 {{ }}", ACDCPropertyAccess
    return f"haxlanalyser.ACDCPropertyAccess.1 {key}", ACDCPropertyAccess

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserACDCPropertyAccess":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.TallyResponse.1 {{ }}", TallyResponse
    return f"haxlanalyser.TallyResponse.1 {key}", TallyResponse

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "HaxlanalyserTallyResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScribeResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ScribeResponse.1 {{ }}", ScribeResponse
    return f"haxlanalyser.ScribeResponse.1 {key}", ScribeResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserScribeResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.InputFetch.1 {{ }}", InputFetch
    return f"haxlanalyser.InputFetch.1 { concatenateFields(key) }", InputFetch

  @staticmethod
  def angle_query(*, key: Optional[Tuple[()]] = None, source: Optional[Tuple[()]] = None) -> "HaxlanalyserInputFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ClassifierFetch.1 {{ }}", ClassifierFetch
    return f"haxlanalyser.ClassifierFetch.1 {key}", ClassifierFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserClassifierFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLogFeatureResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.LogFeatureResponse.1 {{ }}", LogFeatureResponse
    return f"haxlanalyser.LogFeatureResponse.1 {key}", LogFeatureResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserLogFeatureResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLaserDataset(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.LaserDataset.1 {{ }}", LaserDataset
    return f"haxlanalyser.LaserDataset.1 {key}", LaserDataset

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserLaserDataset":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.TallyName.1 {{ }}", TallyName
    return f"haxlanalyser.TallyName.1 {key}", TallyName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserTallyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserConfigeratorFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.ConfigeratorFetch.1 {{ }}", ConfigeratorFetch
    return f"haxlanalyser.ConfigeratorFetch.1 {key}", ConfigeratorFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserConfigeratorFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserHiveResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"haxlanalyser.HiveResponse.1 {{ }}", HiveResponse
    return f"haxlanalyser.HiveResponse.1 {key}", HiveResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserHiveResponse":
    raise Exception("this function can only be called from @angle_query")


