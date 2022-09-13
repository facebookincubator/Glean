# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.hs import *
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
    Fetch,
    TallyCounterType,
    Tally,
    Response,
    Node,
)


class HaxlanalyserThriftResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ThriftResponse.1 { angle_for(__env, arg, None) or '_' }", ThriftResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserThriftResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserTallyConfig(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyConfig.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", TallyConfig

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserTallyName"] = None, source: Optional["SrcLoc"] = None) -> "HaxlanalyserTallyConfig":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserPolicyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.PolicyName.1 { angle_for(__env, arg, None) or '_' }", PolicyName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserPolicyName":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserRestrictionResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.RestrictionResponse.1 { angle_for(__env, arg, None) or '_' }", RestrictionResponse

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "HaxlanalyserRestrictionResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserContext(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Context.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", Context

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserContextName"] = None, source: Optional["SrcLoc"] = None) -> "HaxlanalyserContext":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserEndpointName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.EndpointName.1 { angle_for(__env, arg, None) or '_' }", EndpointName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserEndpointName":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserJankyJSONResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.JankyJSONResponse.1 { angle_for(__env, arg, None) or '_' }", JankyJSONResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserJankyJSONResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserClassifierRead(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ClassifierRead.1 { angle_for(__env, arg, None) or '_' }", ClassifierRead

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserClassifierRead":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserScubaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ScubaResponse.1 { angle_for(__env, arg, None) or '_' }", ScubaResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserScubaResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserFeatureSetFeature(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.FeatureSetFeature.1 { angle_for(__env, arg, None) or '_' }", FeatureSetFeature

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserFeatureSetFeature":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserSitevarFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.SitevarFetch.1 { angle_for(__env, arg, None) or '_' }", SitevarFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserSitevarFetch":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserCall(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], ref: ast.Expr, qname: ast.Expr, loc: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Call.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, ref, 'ref'), angle_for(__env, qname, 'qname'), angle_for(__env, loc, 'loc')])) or '_' } }}", Call

  @staticmethod
  def angle_query(*, ref: Optional[str] = None, qname: Optional[str] = None, loc: Optional["SrcLoc"] = None) -> "HaxlanalyserCall":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserACDCProperty(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ACDCProperty.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", ACDCProperty

  @staticmethod
  def angle_query(*, name: Optional[str] = None, source: Optional["SrcLoc"] = None) -> "HaxlanalyserACDCProperty":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], origin: ast.Expr, dest: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Edge.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, origin, 'origin'), angle_for(__env, dest, 'dest')])) or '_' } }}", Edge

  @staticmethod
  def angle_query(*, origin: Optional["HaxlanalyserNode"] = None, dest: Optional["HaxlanalyserNode"] = None) -> "HaxlanalyserEdge":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserEndpoint(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Endpoint.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", Endpoint

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserEndpointName"] = None, source: Optional["SrcLoc"] = None) -> "HaxlanalyserEndpoint":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserTallyFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyFetch.1 { angle_for(__env, arg, None) or '_' }", TallyFetch

  @staticmethod
  def angle_query(*, arg: Optional["HaxlanalyserTally"] = None) -> "HaxlanalyserTallyFetch":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserPolicy(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Policy.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, source, 'source')])) or '_' } }}", Policy

  @staticmethod
  def angle_query(*, name: Optional["HaxlanalyserPolicyName"] = None, source: Optional["SrcLoc"] = None) -> "HaxlanalyserPolicy":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserContextName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ContextName.1 { angle_for(__env, arg, None) or '_' }", ContextName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserContextName":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserTestFile(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TestFile.1 { angle_for(__env, arg, None) or '_' }", TestFile

  @staticmethod
  def angle_query(*, arg: Optional["SrcFile"] = None) -> "HaxlanalyserTestFile":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserThriftFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ThriftFetch.1 { angle_for(__env, arg, None) or '_' }", ThriftFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserThriftFetch":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserInputKey(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.InputKey.1 { angle_for(__env, arg, None) or '_' }", InputKey

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserInputKey":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserPiranhaResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.PiranhaResponse.1 { angle_for(__env, arg, None) or '_' }", PiranhaResponse

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "HaxlanalyserPiranhaResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserACDCPropertyAccess(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ACDCPropertyAccess.1 { angle_for(__env, arg, None) or '_' }", ACDCPropertyAccess

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserACDCPropertyAccess":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserTallyResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyResponse.1 { angle_for(__env, arg, None) or '_' }", TallyResponse

  @staticmethod
  def angle_query(*, arg: Optional["HaxlanalyserTally"] = None) -> "HaxlanalyserTallyResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserScribeResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ScribeResponse.1 { angle_for(__env, arg, None) or '_' }", ScribeResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserScribeResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserInputFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], key: ast.Expr, source: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.InputFetch.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, key, 'key'), angle_for(__env, source, 'source')])) or '_' } }}", InputFetch

  @staticmethod
  def angle_query(*, key: Optional["HaxlanalyserInputKey"] = None, source: Optional["SrcLoc"] = None) -> "HaxlanalyserInputFetch":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserClassifierFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ClassifierFetch.1 { angle_for(__env, arg, None) or '_' }", ClassifierFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserClassifierFetch":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserLogFeatureResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.LogFeatureResponse.1 { angle_for(__env, arg, None) or '_' }", LogFeatureResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserLogFeatureResponse":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserLaserDataset(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.LaserDataset.1 { angle_for(__env, arg, None) or '_' }", LaserDataset

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserLaserDataset":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserTallyName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.TallyName.1 { angle_for(__env, arg, None) or '_' }", TallyName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserTallyName":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserConfigeratorFetch(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.ConfigeratorFetch.1 { angle_for(__env, arg, None) or '_' }", ConfigeratorFetch

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserConfigeratorFetch":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserHiveResponse(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.HiveResponse.1 { angle_for(__env, arg, None) or '_' }", HiveResponse

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "HaxlanalyserHiveResponse":
    raise Exception("this function can only be called from @angle_query")





class HaxlanalyserFetch(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], thrift: ast.Expr, configerator: ast.Expr, tally: ast.Expr, classifier: ast.Expr, sitevar: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Fetch.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, thrift, 'thrift'), angle_for(__env, configerator, 'configerator'), angle_for(__env, tally, 'tally'), angle_for(__env, classifier, 'classifier'), angle_for(__env, sitevar, 'sitevar')])) or '_' } }}", Fetch

  @staticmethod
  def angle_query_thrift(*, thrift: Optional["HaxlanalyserThriftFetch"] = None) -> "HaxlanalyserFetch":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_configerator(*, configerator: Optional["HaxlanalyserConfigeratorFetch"] = None) -> "HaxlanalyserFetch":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_tally(*, tally: Optional["HaxlanalyserTallyFetch"] = None) -> "HaxlanalyserFetch":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_classifier(*, classifier: Optional["HaxlanalyserClassifierFetch"] = None) -> "HaxlanalyserFetch":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_sitevar(*, sitevar: Optional["HaxlanalyserSitevarFetch"] = None) -> "HaxlanalyserFetch":
    raise Exception("this function can only be called from @angle_query")




class HaxlanalyserTallyCounterType(Enum):
  counter = 0
  uniqueCounter = 1
  topK = 2
  quantiles = 3
  uniqueQuantiles = 4
  firstN = 5
  lastN = 6
  moments = 7
  infiniteCounter = 8

class HaxlanalyserTally(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], name: ast.Expr, type: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Tally.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, name, 'name'), angle_for(__env, type, 'type')])) or '_' } }}", Tally

  @staticmethod
  def angle_query(*, name: Optional[str] = None, type: Optional["HaxlanalyserTallyCounterType"] = None) -> "HaxlanalyserTally":
    raise Exception("this function can only be called from @angle_query")



class HaxlanalyserResponse(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], janky_json: ast.Expr, scuba: ast.Expr, thrift: ast.Expr, piranha: ast.Expr, tally: ast.Expr, log_feature: ast.Expr, restriction: ast.Expr, hive: ast.Expr, scribe: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Response.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, janky_json, 'janky_json'), angle_for(__env, scuba, 'scuba'), angle_for(__env, thrift, 'thrift'), angle_for(__env, piranha, 'piranha'), angle_for(__env, tally, 'tally'), angle_for(__env, log_feature, 'log_feature'), angle_for(__env, restriction, 'restriction'), angle_for(__env, hive, 'hive'), angle_for(__env, scribe, 'scribe')])) or '_' } }}", Response

  @staticmethod
  def angle_query_janky_json(*, janky_json: Optional["HaxlanalyserJankyJSONResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_scuba(*, scuba: Optional["HaxlanalyserScubaResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_thrift(*, thrift: Optional["HaxlanalyserThriftResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_piranha(*, piranha: Optional["HaxlanalyserPiranhaResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_tally(*, tally: Optional["HaxlanalyserTallyResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_log_feature(*, log_feature: Optional["HaxlanalyserLogFeatureResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_restriction(*, restriction: Optional["HaxlanalyserRestrictionResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_hive(*, hive: Optional["HaxlanalyserHiveResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_scribe(*, scribe: Optional["HaxlanalyserScribeResponse"] = None) -> "HaxlanalyserResponse":
    raise Exception("this function can only be called from @angle_query")




class HaxlanalyserNode(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], hs_module: ast.Expr, definition: ast.Expr, typeclass: ast.Expr, class_instance: ast.Expr, context: ast.Expr, policy: ast.Expr, tally: ast.Expr, endpoint: ast.Expr, acdc: ast.Expr, input_fetch: ast.Expr, response: ast.Expr, fetch: ast.Expr, classifier_read: ast.Expr, laser_dataset: ast.Expr, test_file: ast.Expr, call: ast.Expr, acdc_access: ast.Expr, feature: ast.Expr) -> Tuple[str, Struct]:
    return f"haxlanalyser.Node.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, hs_module, 'hs_module'), angle_for(__env, definition, 'definition'), angle_for(__env, typeclass, 'typeclass'), angle_for(__env, class_instance, 'class_instance'), angle_for(__env, context, 'context'), angle_for(__env, policy, 'policy'), angle_for(__env, tally, 'tally'), angle_for(__env, endpoint, 'endpoint'), angle_for(__env, acdc, 'acdc'), angle_for(__env, input_fetch, 'input_fetch'), angle_for(__env, response, 'response'), angle_for(__env, fetch, 'fetch'), angle_for(__env, classifier_read, 'classifier_read'), angle_for(__env, laser_dataset, 'laser_dataset'), angle_for(__env, test_file, 'test_file'), angle_for(__env, call, 'call'), angle_for(__env, acdc_access, 'acdc_access'), angle_for(__env, feature, 'feature')])) or '_' } }}", Node

  @staticmethod
  def angle_query_hs_module(*, hs_module: Optional["HsSourceModule"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_definition(*, definition: Optional["HsDefinitionName"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_typeclass(*, typeclass: Optional["HsClassName"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_class_instance(*, class_instance: Optional["HsClassInstance"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_context(*, context: Optional["HaxlanalyserContext"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_policy(*, policy: Optional["HaxlanalyserPolicy"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_tally(*, tally: Optional["HaxlanalyserTallyConfig"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_endpoint(*, endpoint: Optional["HaxlanalyserEndpoint"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_acdc(*, acdc: Optional["HaxlanalyserACDCProperty"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_input_fetch(*, input_fetch: Optional["HaxlanalyserInputFetch"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_response(*, response: Optional["HaxlanalyserResponse"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_fetch(*, fetch: Optional["HaxlanalyserFetch"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_classifier_read(*, classifier_read: Optional["HaxlanalyserClassifierRead"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_laser_dataset(*, laser_dataset: Optional["HaxlanalyserLaserDataset"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_test_file(*, test_file: Optional["HaxlanalyserTestFile"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_call(*, call: Optional["HaxlanalyserCall"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_acdc_access(*, acdc_access: Optional["HaxlanalyserACDCPropertyAccess"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_feature(*, feature: Optional["HaxlanalyserFeatureSetFeature"] = None) -> "HaxlanalyserNode":
    raise Exception("this function can only be called from @angle_query")





