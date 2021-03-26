-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Hs.Types
import qualified Glean.Schema.Src.Types


instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ScubaResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.ScubaResponse = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ScubaResponse"1
  getIndex _proxy  = 505
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.scubaResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ScubaResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.scubaResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ScubaResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ClassifierRead where
  type KeyType Glean.Schema.Haxlanalyser.Types.ClassifierRead = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ClassifierRead"1
  getIndex _proxy  = 496
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.classifierRead_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ClassifierRead x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.classifierRead_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ClassifierRead where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.SitevarFetch where
  type KeyType Glean.Schema.Haxlanalyser.Types.SitevarFetch = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.SitevarFetch"1
  getIndex _proxy  = 466
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.sitevarFetch_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.SitevarFetch x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.sitevarFetch_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.SitevarFetch where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Call_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Call_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.Call_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.Call_key = 'Angle.TField "ref" (Data.Text.Text) ('Angle.TField "qname" (Data.Text.Text) ('Angle.TField "loc" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.Call where
  type KeyType Glean.Schema.Haxlanalyser.Types.Call =
    Glean.Schema.Haxlanalyser.Types.Call_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.Call"1
  getIndex _proxy  = 463
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.call_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.Call x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.call_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Call where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.FeatureSetFeature where
  type KeyType Glean.Schema.Haxlanalyser.Types.FeatureSetFeature =
    Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.FeatureSetFeature"1
  getIndex _proxy  = 452
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.featureSetFeature_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.FeatureSetFeature x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.featureSetFeature_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.FeatureSetFeature where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ACDCProperty_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.ACDCProperty_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.ACDCProperty_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.ACDCProperty_key = 'Angle.TField "name" (Data.Text.Text) ('Angle.TField "source" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ACDCProperty where
  type KeyType Glean.Schema.Haxlanalyser.Types.ACDCProperty =
    Glean.Schema.Haxlanalyser.Types.ACDCProperty_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ACDCProperty"1
  getIndex _proxy  = 451
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.aCDCProperty_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ACDCProperty x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.aCDCProperty_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ACDCProperty where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ScribeResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.ScribeResponse = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ScribeResponse"1
  getIndex _proxy  = 444
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.scribeResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ScribeResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.scribeResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ScribeResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.TallyResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.TallyResponse =
    Glean.Schema.Haxlanalyser.Types.Tally
  getName _proxy  = Glean.PredicateRef "haxlanalyser.TallyResponse"1
  getIndex _proxy  = 439
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.tallyResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.TallyResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.tallyResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.TallyResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.HiveResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.HiveResponse = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.HiveResponse"1
  getIndex _proxy  = 387
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.hiveResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.HiveResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.hiveResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.HiveResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch where
  type KeyType Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch =
    Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ConfigeratorFetch"1
  getIndex _proxy  = 364
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.configeratorFetch_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.configeratorFetch_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.TallyConfig_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.TallyConfig_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.TallyConfig_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.TallyConfig_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.TallyName) ('Angle.TField "source" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.TallyConfig where
  type KeyType Glean.Schema.Haxlanalyser.Types.TallyConfig =
    Glean.Schema.Haxlanalyser.Types.TallyConfig_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.TallyConfig"1
  getIndex _proxy  = 358
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.tallyConfig_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.TallyConfig x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.tallyConfig_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.TallyConfig where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ThriftResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.ThriftResponse = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ThriftResponse"1
  getIndex _proxy  = 352
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.thriftResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ThriftResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.thriftResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ThriftResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Endpoint_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Endpoint_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.Endpoint_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.Endpoint_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.EndpointName) ('Angle.TField "source" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.Endpoint where
  type KeyType Glean.Schema.Haxlanalyser.Types.Endpoint =
    Glean.Schema.Haxlanalyser.Types.Endpoint_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.Endpoint"1
  getIndex _proxy  = 316
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.endpoint_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.Endpoint x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.endpoint_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Endpoint where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Edge_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Edge_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.Edge_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.Edge_key = 'Angle.TField "origin" (Glean.Schema.Haxlanalyser.Types.Node) ('Angle.TField "dest" (Glean.Schema.Haxlanalyser.Types.Node) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.Edge where
  type KeyType Glean.Schema.Haxlanalyser.Types.Edge =
    Glean.Schema.Haxlanalyser.Types.Edge_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.Edge"1
  getIndex _proxy  = 311
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.edge_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.Edge x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.edge_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Edge where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ContextName where
  type KeyType Glean.Schema.Haxlanalyser.Types.ContextName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ContextName"1
  getIndex _proxy  = 305
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.contextName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ContextName x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.contextName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ContextName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.TestFile where
  type KeyType Glean.Schema.Haxlanalyser.Types.TestFile =
    Glean.Schema.Src.Types.File
  getName _proxy  = Glean.PredicateRef "haxlanalyser.TestFile"1
  getIndex _proxy  = 291
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.testFile_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.TestFile x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.testFile_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.TestFile where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.TallyFetch where
  type KeyType Glean.Schema.Haxlanalyser.Types.TallyFetch =
    Glean.Schema.Haxlanalyser.Types.Tally
  getName _proxy  = Glean.PredicateRef "haxlanalyser.TallyFetch"1
  getIndex _proxy  = 272
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.tallyFetch_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.TallyFetch x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.tallyFetch_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.TallyFetch where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Policy_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Policy_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.Policy_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.Policy_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.PolicyName) ('Angle.TField "source" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.Policy where
  type KeyType Glean.Schema.Haxlanalyser.Types.Policy =
    Glean.Schema.Haxlanalyser.Types.Policy_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.Policy"1
  getIndex _proxy  = 270
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.policy_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.Policy x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.policy_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Policy where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.InputFetch_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.InputFetch_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.InputFetch_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.InputFetch_key = 'Angle.TField "key" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.InputKey) ('Angle.TField "source" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.InputFetch where
  type KeyType Glean.Schema.Haxlanalyser.Types.InputFetch =
    Glean.Schema.Haxlanalyser.Types.InputFetch_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.InputFetch"1
  getIndex _proxy  = 231
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.inputFetch_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.InputFetch x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.inputFetch_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.InputFetch where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.RestrictionResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.RestrictionResponse = Glean.Nat
  getName _proxy  = Glean.PredicateRef "haxlanalyser.RestrictionResponse"1
  getIndex _proxy  = 194
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.restrictionResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.RestrictionResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.restrictionResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.RestrictionResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Context_key where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Context_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.Context_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.Context_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ContextName) ('Angle.TField "source" (Glean.Schema.Src.Types.Loc) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.Context where
  type KeyType Glean.Schema.Haxlanalyser.Types.Context =
    Glean.Schema.Haxlanalyser.Types.Context_key
  getName _proxy  = Glean.PredicateRef "haxlanalyser.Context"1
  getIndex _proxy  = 193
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.context_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.Context x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.context_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Context where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.PolicyName where
  type KeyType Glean.Schema.Haxlanalyser.Types.PolicyName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.PolicyName"1
  getIndex _proxy  = 188
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.policyName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.PolicyName x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.policyName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.PolicyName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.EndpointName where
  type KeyType Glean.Schema.Haxlanalyser.Types.EndpointName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.EndpointName"1
  getIndex _proxy  = 173
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.endpointName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.EndpointName x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.endpointName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.EndpointName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.JankyJSONResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.JankyJSONResponse =
    Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.JankyJSONResponse"1
  getIndex _proxy  = 152
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.jankyJSONResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.JankyJSONResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.jankyJSONResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.JankyJSONResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ThriftFetch where
  type KeyType Glean.Schema.Haxlanalyser.Types.ThriftFetch = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ThriftFetch"1
  getIndex _proxy  = 150
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.thriftFetch_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ThriftFetch x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.thriftFetch_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ThriftFetch where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.InputKey where
  type KeyType Glean.Schema.Haxlanalyser.Types.InputKey = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.InputKey"1
  getIndex _proxy  = 127
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.inputKey_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.InputKey x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.inputKey_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.InputKey where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess where
  type KeyType Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess =
    Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ACDCPropertyAccess"1
  getIndex _proxy  = 111
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.aCDCPropertyAccess_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.aCDCPropertyAccess_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.PiranhaResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.PiranhaResponse = Glean.Nat
  getName _proxy  = Glean.PredicateRef "haxlanalyser.PiranhaResponse"1
  getIndex _proxy  = 87
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.piranhaResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.PiranhaResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.piranhaResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.PiranhaResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.LaserDataset where
  type KeyType Glean.Schema.Haxlanalyser.Types.LaserDataset = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.LaserDataset"1
  getIndex _proxy  = 69
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.laserDataset_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.LaserDataset x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.laserDataset_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.LaserDataset where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.ClassifierFetch where
  type KeyType Glean.Schema.Haxlanalyser.Types.ClassifierFetch =
    Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.ClassifierFetch"1
  getIndex _proxy  = 59
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.classifierFetch_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.ClassifierFetch x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.classifierFetch_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.ClassifierFetch where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.TallyName where
  type KeyType Glean.Schema.Haxlanalyser.Types.TallyName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.TallyName"1
  getIndex _proxy  = 55
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.tallyName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.TallyName x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.tallyName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.TallyName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Haxlanalyser.Types.LogFeatureResponse where
  type KeyType Glean.Schema.Haxlanalyser.Types.LogFeatureResponse =
    Data.Text.Text
  getName _proxy  = Glean.PredicateRef "haxlanalyser.LogFeatureResponse"1
  getIndex _proxy  = 51
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Haxlanalyser.Types.logFeatureResponse_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Haxlanalyser.Types.LogFeatureResponse x k
  getFactKey = Glean.Schema.Haxlanalyser.Types.logFeatureResponse_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Haxlanalyser.Types.LogFeatureResponse where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Tally where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Tally x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Haxlanalyser.Types.Tally
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Haxlanalyser.Types.Tally = 'Angle.TField "name" (Data.Text.Text) ('Angle.TField "type" (Glean.Schema.Haxlanalyser.Types.TallyCounterType) ('Angle.TNoFields))

instance Glean.Type Glean.Schema.Haxlanalyser.Types.TallyCounterType where
  buildRtsValue = Glean.thriftEnum_buildRtsValue
  decodeRtsValue = Glean.thriftEnumD

type instance Angle.SumFields Glean.Schema.Haxlanalyser.Types.TallyCounterType = 'Angle.TField "counter" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "uniqueCounter" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "topK" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "quantiles" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "uniqueQuantiles" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "firstN" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "lastN" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "moments" (Glean.Schema.Builtin.Types.Unit) ('Angle.TField "infiniteCounter" (Glean.Schema.Builtin.Types.Unit) ('Angle.TNoFields)))))))))

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Fetch where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Fetch_thrift x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Fetch_configerator x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Fetch_tally x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Fetch_classifier x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Fetch_sitevar x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Haxlanalyser.Types.Fetch_thrift
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Fetch_configerator
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Fetch_tally
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Fetch_classifier
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Fetch_sitevar
    ]

type instance Angle.SumFields Glean.Schema.Haxlanalyser.Types.Fetch = 'Angle.TField "thrift" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ThriftFetch) ('Angle.TField "configerator" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch) ('Angle.TField "tally" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.TallyFetch) ('Angle.TField "classifier" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ClassifierFetch) ('Angle.TField "sitevar" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.SitevarFetch) ('Angle.TNoFields)))))

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ThriftFetch Glean.Schema.Haxlanalyser.Types.Fetch where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Fetch_thrift
  projectBranch (Glean.Schema.Haxlanalyser.Types.Fetch_thrift x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ConfigeratorFetch Glean.Schema.Haxlanalyser.Types.Fetch where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Fetch_configerator
  projectBranch (Glean.Schema.Haxlanalyser.Types.Fetch_configerator x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.TallyFetch Glean.Schema.Haxlanalyser.Types.Fetch where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Fetch_tally
  projectBranch (Glean.Schema.Haxlanalyser.Types.Fetch_tally x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ClassifierFetch Glean.Schema.Haxlanalyser.Types.Fetch where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Fetch_classifier
  projectBranch (Glean.Schema.Haxlanalyser.Types.Fetch_classifier x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.SitevarFetch Glean.Schema.Haxlanalyser.Types.Fetch where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Fetch_sitevar
  projectBranch (Glean.Schema.Haxlanalyser.Types.Fetch_sitevar x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Response where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_janky_json x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_scuba x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_thrift x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_piranha x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_tally x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_log_feature x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_restriction x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_hive x) = do
    Glean.buildRtsSelector b 7
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Response_scribe x) = do
    Glean.buildRtsSelector b 8
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_janky_json
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_scuba
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_thrift
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_piranha
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_tally
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_log_feature
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_restriction
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_hive
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Response_scribe
    ]

type instance Angle.SumFields Glean.Schema.Haxlanalyser.Types.Response = 'Angle.TField "janky_json" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.JankyJSONResponse) ('Angle.TField "scuba" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ScubaResponse) ('Angle.TField "thrift" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ThriftResponse) ('Angle.TField "piranha" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.PiranhaResponse) ('Angle.TField "tally" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.TallyResponse) ('Angle.TField "log_feature" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.LogFeatureResponse) ('Angle.TField "restriction" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.RestrictionResponse) ('Angle.TField "hive" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.HiveResponse) ('Angle.TField "scribe" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ScribeResponse) ('Angle.TNoFields)))))))))

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.JankyJSONResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_janky_json
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_janky_json x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ScubaResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_scuba
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_scuba x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ThriftResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_thrift
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_thrift x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.PiranhaResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_piranha
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_piranha x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.TallyResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_tally
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_tally x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.LogFeatureResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_log_feature
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_log_feature x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.RestrictionResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_restriction
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_restriction x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.HiveResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_hive
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_hive x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ScribeResponse Glean.Schema.Haxlanalyser.Types.Response where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Response_scribe
  projectBranch (Glean.Schema.Haxlanalyser.Types.Response_scribe x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Type Glean.Schema.Haxlanalyser.Types.Node where
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_hs_module x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_definition x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_typeclass x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_class_instance x) = do
    Glean.buildRtsSelector b 3
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_context x) = do
    Glean.buildRtsSelector b 4
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_policy x) = do
    Glean.buildRtsSelector b 5
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_tally x) = do
    Glean.buildRtsSelector b 6
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_endpoint x) = do
    Glean.buildRtsSelector b 7
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_acdc x) = do
    Glean.buildRtsSelector b 8
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_input_fetch x) = do
    Glean.buildRtsSelector b 9
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_response x) = do
    Glean.buildRtsSelector b 10
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_fetch x) = do
    Glean.buildRtsSelector b 11
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_classifier_read x) = do
    Glean.buildRtsSelector b 12
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_laser_dataset x) = do
    Glean.buildRtsSelector b 13
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_test_file x) = do
    Glean.buildRtsSelector b 14
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_call x) = do
    Glean.buildRtsSelector b 15
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_acdc_access x) = do
    Glean.buildRtsSelector b 16
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Haxlanalyser.Types.Node_feature x) = do
    Glean.buildRtsSelector b 17
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_hs_module
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_definition
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_typeclass
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_class_instance
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_context
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_policy
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_tally
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_endpoint
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_acdc
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_input_fetch
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_response
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_fetch
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_classifier_read
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_laser_dataset
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_test_file
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_call
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_acdc_access
    , Glean.mapD Glean.Schema.Haxlanalyser.Types.Node_feature
    ]

type instance Angle.SumFields Glean.Schema.Haxlanalyser.Types.Node = 'Angle.TField "hs_module" (Glean.KeyType Glean.Schema.Hs.Types.SourceModule) ('Angle.TField "definition" (Glean.KeyType Glean.Schema.Hs.Types.DefinitionName) ('Angle.TField "typeclass" (Glean.KeyType Glean.Schema.Hs.Types.ClassName) ('Angle.TField "class_instance" (Glean.KeyType Glean.Schema.Hs.Types.ClassInstance) ('Angle.TField "context" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.Context) ('Angle.TField "policy" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.Policy) ('Angle.TField "tally" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.TallyConfig) ('Angle.TField "endpoint" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.Endpoint) ('Angle.TField "acdc" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ACDCProperty) ('Angle.TField "input_fetch" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.InputFetch) ('Angle.TField "response" (Glean.Schema.Haxlanalyser.Types.Response) ('Angle.TField "fetch" (Glean.Schema.Haxlanalyser.Types.Fetch) ('Angle.TField "classifier_read" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ClassifierRead) ('Angle.TField "laser_dataset" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.LaserDataset) ('Angle.TField "test_file" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.TestFile) ('Angle.TField "call" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.Call) ('Angle.TField "acdc_access" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess) ('Angle.TField "feature" (Glean.KeyType Glean.Schema.Haxlanalyser.Types.FeatureSetFeature) ('Angle.TNoFields))))))))))))))))))

instance Glean.SumBranches Glean.Schema.Hs.Types.SourceModule Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_hs_module
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_hs_module x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hs.Types.DefinitionName Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_definition
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_definition x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hs.Types.ClassName Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_typeclass
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_typeclass x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Hs.Types.ClassInstance Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_class_instance
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_class_instance x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.Context Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_context
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_context x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.Policy Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_policy
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_policy x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.TallyConfig Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_tally
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_tally x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.Endpoint Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_endpoint
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_endpoint x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ACDCProperty Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_acdc
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_acdc x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.InputFetch Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_input_fetch
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_input_fetch x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.Response Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_response
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_response x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.Fetch Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_fetch
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_fetch x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ClassifierRead Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_classifier_read
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_classifier_read x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.LaserDataset Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_laser_dataset
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_laser_dataset x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.TestFile Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_test_file
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_test_file x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.Call Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_call
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_call x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.ACDCPropertyAccess Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_acdc_access
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_acdc_access x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Haxlanalyser.Types.FeatureSetFeature Glean.Schema.Haxlanalyser.Types.Node where
  injectBranch = Glean.Schema.Haxlanalyser.Types.Node_feature
  projectBranch (Glean.Schema.Haxlanalyser.Types.Node_feature x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
