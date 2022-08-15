# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class HaxlanalyserThriftResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ThriftResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserThriftResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.TallyConfig.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserTallyConfig":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.PolicyName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserPolicyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserRestrictionResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.RestrictionResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "HaxlanalyserRestrictionResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.Context.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserContext":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpointName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.EndpointName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserEndpointName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserJankyJSONResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.JankyJSONResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserJankyJSONResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierRead(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ClassifierRead.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserClassifierRead":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScubaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ScubaResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserScubaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserFeatureSetFeature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.FeatureSetFeature.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserFeatureSetFeature":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserSitevarFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.SitevarFetch.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserSitevarFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.Call.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserCall":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCProperty(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ACDCProperty.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserACDCProperty":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.Edge.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserEdge":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.Endpoint.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserEndpoint":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.TallyFetch.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserTallyFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.Policy.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserPolicy":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserContextName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ContextName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserContextName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTestFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.TestFile.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserTestFile":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserThriftFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ThriftFetch.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserThriftFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.InputKey.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserInputKey":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserPiranhaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.PiranhaResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "HaxlanalyserPiranhaResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserACDCPropertyAccess(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ACDCPropertyAccess.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserACDCPropertyAccess":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.TallyResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserTallyResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserScribeResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ScribeResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserScribeResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserInputFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.InputFetch.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "HaxlanalyserInputFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserClassifierFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ClassifierFetch.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserClassifierFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLogFeatureResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.LogFeatureResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserLogFeatureResponse":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserLaserDataset(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.LaserDataset.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserLaserDataset":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserTallyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.TallyName.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserTallyName":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserConfigeratorFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.ConfigeratorFetch.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserConfigeratorFetch":
    raise Exception("this function can only be called from @angle_query")

class HaxlanalyserHiveResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"haxlanalyser.HiveResponse.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "HaxlanalyserHiveResponse":
    raise Exception("this function can only be called from @angle_query")


