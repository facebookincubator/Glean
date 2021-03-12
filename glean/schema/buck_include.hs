-- @generated
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
import qualified Data.ByteString
import qualified Data.Default
import qualified Data.Text

import qualified Glean.Types as Glean
import qualified Glean.Typed as Glean
import qualified Glean.Query.Angle as Angle

import qualified Glean.Schema.Builtin.Types
import qualified Glean.Schema.Src.Types
import qualified Glean.Schema.Sys.Types


instance Glean.Type Glean.Schema.Buck.Types.Locator_key where
  buildRtsValue b (Glean.Schema.Buck.Types.Locator_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buck.Types.Locator_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.Locator_key = 'Angle.TField "subdir" (Prelude.Maybe Data.Text.Text) ('Angle.TField "path" (Data.Text.Text) ('Angle.TField "name" (Data.Text.Text) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Buck.Types.Locator where
  type KeyType Glean.Schema.Buck.Types.Locator =
    Glean.Schema.Buck.Types.Locator_key
  getName _proxy  = Glean.PredicateRef "buck.Locator"1
  getIndex _proxy  = 494
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.locator_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Locator x k
  getFactKey = Glean.Schema.Buck.Types.locator_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Locator where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TargetDependencies_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TargetDependencies_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.TargetDependencies_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TargetDependencies_key = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TField "dependencies" ([Glean.Schema.Buck.Types.Dependency]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.TargetDependencies where
  type KeyType Glean.Schema.Buck.Types.TargetDependencies =
    Glean.Schema.Buck.Types.TargetDependencies_key
  getName _proxy  = Glean.PredicateRef "buck.TargetDependencies"1
  getIndex _proxy  = 483
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetDependencies_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetDependencies x k
  getFactKey = Glean.Schema.Buck.Types.targetDependencies_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetDependencies where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Buck.Types.Type where
  type KeyType Glean.Schema.Buck.Types.Type = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "buck.Type"1
  getIndex _proxy  = 479
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.type_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Type x k
  getFactKey = Glean.Schema.Buck.Types.type_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Type where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Buck.Types.Platform where
  type KeyType Glean.Schema.Buck.Types.Platform = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "buck.Platform"1
  getIndex _proxy  = 464
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.platform_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Platform x k
  getFactKey = Glean.Schema.Buck.Types.platform_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Platform where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TargetSources_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TargetSources_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Buck.Types.TargetSources_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TargetSources_key = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TField "headers" ([Glean.KeyType Glean.Schema.Buck.Types.File]) ('Angle.TField "exportedHeaders" ([Glean.KeyType Glean.Schema.Buck.Types.File]) ('Angle.TField "srcs" ([Glean.KeyType Glean.Schema.Buck.Types.File]) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Buck.Types.TargetSources where
  type KeyType Glean.Schema.Buck.Types.TargetSources =
    Glean.Schema.Buck.Types.TargetSources_key
  getName _proxy  = Glean.PredicateRef "buck.TargetSources"3
  getIndex _proxy  = 441
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetSources_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetSources x k
  getFactKey = Glean.Schema.Buck.Types.targetSources_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetSources where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TargetSources_1_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TargetSources_1_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Buck.Types.TargetSources_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TargetSources_1_key = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TField "headers" ([Glean.KeyType Glean.Schema.Buck.Types.File_1]) ('Angle.TField "exportedHeaders" ([Glean.KeyType Glean.Schema.Buck.Types.File_1]) ('Angle.TField "srcs" ([Glean.KeyType Glean.Schema.Buck.Types.File_1]) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Buck.Types.TargetSources_1 where
  type KeyType Glean.Schema.Buck.Types.TargetSources_1 =
    Glean.Schema.Buck.Types.TargetSources_1_key
  getName _proxy  = Glean.PredicateRef "buck.TargetSources"1
  getIndex _proxy  = 440
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetSources_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetSources_1 x k
  getFactKey = Glean.Schema.Buck.Types.targetSources_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetSources_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.OutTarget_key where
  buildRtsValue b (Glean.Schema.Buck.Types.OutTarget_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.OutTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.OutTarget_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.OutTarget where
  type KeyType Glean.Schema.Buck.Types.OutTarget =
    Glean.Schema.Buck.Types.OutTarget_key
  getName _proxy  = Glean.PredicateRef "buck.OutTarget"1
  getIndex _proxy  = 433
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.outTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.OutTarget x k
  getFactKey = Glean.Schema.Buck.Types.outTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.OutTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Buck.Types.OutputLabel where
  type KeyType Glean.Schema.Buck.Types.OutputLabel = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "buck.OutputLabel"3
  getIndex _proxy  = 411
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.outputLabel_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.OutputLabel x k
  getFactKey = Glean.Schema.Buck.Types.outputLabel_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.OutputLabel where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.Owner_key where
  buildRtsValue b (Glean.Schema.Buck.Types.Owner_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.Owner_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.Owner_key = 'Angle.TField "source" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "owner" (Glean.KeyType Glean.Schema.Buck.Types.TargetSources) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.Owner where
  type KeyType Glean.Schema.Buck.Types.Owner =
    Glean.Schema.Buck.Types.Owner_key
  getName _proxy  = Glean.PredicateRef "buck.Owner"3
  getIndex _proxy  = 403
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.owner_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Owner x k
  getFactKey = Glean.Schema.Buck.Types.owner_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Owner where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.Owner_1_key where
  buildRtsValue b (Glean.Schema.Buck.Types.Owner_1_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.Owner_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.Owner_1_key = 'Angle.TField "source" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "owner" (Glean.KeyType Glean.Schema.Buck.Types.TargetSources_1) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.Owner_1 where
  type KeyType Glean.Schema.Buck.Types.Owner_1 =
    Glean.Schema.Buck.Types.Owner_1_key
  getName _proxy  = Glean.PredicateRef "buck.Owner"1
  getIndex _proxy  = 402
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.owner_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Owner_1 x k
  getFactKey = Glean.Schema.Buck.Types.owner_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Owner_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.Target_key where
  buildRtsValue b (Glean.Schema.Buck.Types.Target_key x1 x2 x3 x4) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
    Glean.buildRtsValue b x4
  decodeRtsValue = Glean.Schema.Buck.Types.Target_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.Target_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "type_" (Glean.KeyType Glean.Schema.Buck.Types.Type) ('Angle.TField "defaultPlatform" (Prelude.Maybe (Glean.KeyType Glean.Schema.Buck.Types.Platform)) ('Angle.TField "labels" (Glean.KeyType Glean.Schema.Buck.Types.Labels) ('Angle.TNoFields))))

instance Glean.Predicate Glean.Schema.Buck.Types.Target where
  type KeyType Glean.Schema.Buck.Types.Target =
    Glean.Schema.Buck.Types.Target_key
  getName _proxy  = Glean.PredicateRef "buck.Target"2
  getIndex _proxy  = 394
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.target_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Target x k
  getFactKey = Glean.Schema.Buck.Types.target_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Target where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.Target_1_key where
  buildRtsValue b (Glean.Schema.Buck.Types.Target_1_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buck.Types.Target_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.Target_1_key = 'Angle.TField "repo" (Glean.KeyType Glean.Schema.Sys.Types.Blob) ('Angle.TField "name" (Glean.KeyType Glean.Schema.Sys.Types.Blob) ('Angle.TField "platform" (Prelude.Maybe (Glean.KeyType Glean.Schema.Sys.Types.Blob)) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Buck.Types.Target_1 where
  type KeyType Glean.Schema.Buck.Types.Target_1 =
    Glean.Schema.Buck.Types.Target_1_key
  getName _proxy  = Glean.PredicateRef "buck.Target"1
  getIndex _proxy  = 393
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.target_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Target_1 x k
  getFactKey = Glean.Schema.Buck.Types.target_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Target_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.RuleKey_key where
  buildRtsValue b (Glean.Schema.Buck.Types.RuleKey_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.RuleKey_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.RuleKey_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "ruleKey" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.RuleKey where
  type KeyType Glean.Schema.Buck.Types.RuleKey =
    Glean.Schema.Buck.Types.RuleKey_key
  getName _proxy  = Glean.PredicateRef "buck.RuleKey"1
  getIndex _proxy  = 391
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.ruleKey_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.RuleKey x k
  getFactKey = Glean.Schema.Buck.Types.ruleKey_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.RuleKey where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TargetHash_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TargetHash_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.TargetHash_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TargetHash_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "targetHash" (Data.Text.Text) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.TargetHash where
  type KeyType Glean.Schema.Buck.Types.TargetHash =
    Glean.Schema.Buck.Types.TargetHash_key
  getName _proxy  = Glean.PredicateRef "buck.TargetHash"1
  getIndex _proxy  = 352
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetHash_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetHash x k
  getFactKey = Glean.Schema.Buck.Types.targetHash_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetHash where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Buck.Types.Label where
  type KeyType Glean.Schema.Buck.Types.Label = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "buck.Label"1
  getIndex _proxy  = 314
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.label_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Label x k
  getFactKey = Glean.Schema.Buck.Types.label_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Label where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Buck.Types.Labels where
  type KeyType Glean.Schema.Buck.Types.Labels =
    [Glean.Schema.Buck.Types.Label]
  getName _proxy  = Glean.PredicateRef "buck.Labels"1
  getIndex _proxy  = 283
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.labels_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Labels x k
  getFactKey = Glean.Schema.Buck.Types.labels_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Labels where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Buck.Types.TargetLinkWhole where
  type KeyType Glean.Schema.Buck.Types.TargetLinkWhole =
    Glean.Schema.Buck.Types.Target
  getName _proxy  = Glean.PredicateRef "buck.TargetLinkWhole"1
  getIndex _proxy  = 243
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetLinkWhole_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetLinkWhole x k
  getFactKey = Glean.Schema.Buck.Types.targetLinkWhole_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetLinkWhole where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TargetIndexer_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TargetIndexer_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.TargetIndexer_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TargetIndexer_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Buck.Types.TargetIndexerName) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.TargetIndexer where
  type KeyType Glean.Schema.Buck.Types.TargetIndexer =
    Glean.Schema.Buck.Types.TargetIndexer_key
  getName _proxy  = Glean.PredicateRef "buck.TargetIndexer"3
  getIndex _proxy  = 242
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetIndexer_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetIndexer x k
  getFactKey = Glean.Schema.Buck.Types.targetIndexer_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetIndexer where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Buck.Types.TargetIndexerName where
  type KeyType Glean.Schema.Buck.Types.TargetIndexerName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "buck.TargetIndexerName"3
  getIndex _proxy  = 228
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetIndexerName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetIndexerName x k
  getFactKey = Glean.Schema.Buck.Types.targetIndexerName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetIndexerName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TargetOut_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TargetOut_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.TargetOut_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TargetOut_key = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.TargetOut where
  type KeyType Glean.Schema.Buck.Types.TargetOut =
    Glean.Schema.Buck.Types.TargetOut_key
  getName _proxy  = Glean.PredicateRef "buck.TargetOut"1
  getIndex _proxy  = 220
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetOut_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetOut x k
  getFactKey = Glean.Schema.Buck.Types.targetOut_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetOut where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.LocatorReverseDeps_key where
  buildRtsValue b (Glean.Schema.Buck.Types.LocatorReverseDeps_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.LocatorReverseDeps_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.LocatorReverseDeps_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "rdeps" ([Glean.KeyType Glean.Schema.Buck.Types.Locator]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.LocatorReverseDeps where
  type KeyType Glean.Schema.Buck.Types.LocatorReverseDeps =
    Glean.Schema.Buck.Types.LocatorReverseDeps_key
  getName _proxy  = Glean.PredicateRef "buck.LocatorReverseDeps"1
  getIndex _proxy  = 206
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.locatorReverseDeps_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.LocatorReverseDeps x k
  getFactKey = Glean.Schema.Buck.Types.locatorReverseDeps_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.LocatorReverseDeps where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TargetOuts_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TargetOuts_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buck.Types.TargetOuts_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TargetOuts_key = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TField "outputLabel" (Prelude.Maybe (Glean.KeyType Glean.Schema.Buck.Types.OutputLabel)) ('Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Buck.Types.TargetOuts where
  type KeyType Glean.Schema.Buck.Types.TargetOuts =
    Glean.Schema.Buck.Types.TargetOuts_key
  getName _proxy  = Glean.PredicateRef "buck.TargetOuts"3
  getIndex _proxy  = 147
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.targetOuts_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TargetOuts x k
  getFactKey = Glean.Schema.Buck.Types.targetOuts_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TargetOuts where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.LocatorWithLabel_key where
  buildRtsValue b (Glean.Schema.Buck.Types.LocatorWithLabel_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.LocatorWithLabel_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.LocatorWithLabel_key = 'Angle.TField "locator" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "label" (Glean.KeyType Glean.Schema.Buck.Types.OutputLabel) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.LocatorWithLabel where
  type KeyType Glean.Schema.Buck.Types.LocatorWithLabel =
    Glean.Schema.Buck.Types.LocatorWithLabel_key
  getName _proxy  = Glean.PredicateRef "buck.LocatorWithLabel"3
  getIndex _proxy  = 140
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.locatorWithLabel_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.LocatorWithLabel x k
  getFactKey = Glean.Schema.Buck.Types.locatorWithLabel_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.LocatorWithLabel where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.OutsTarget_key where
  buildRtsValue b (Glean.Schema.Buck.Types.OutsTarget_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buck.Types.OutsTarget_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.OutsTarget_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target) ('Angle.TField "outputLabel" (Prelude.Maybe (Glean.KeyType Glean.Schema.Buck.Types.OutputLabel)) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Buck.Types.OutsTarget where
  type KeyType Glean.Schema.Buck.Types.OutsTarget =
    Glean.Schema.Buck.Types.OutsTarget_key
  getName _proxy  = Glean.PredicateRef "buck.OutsTarget"3
  getIndex _proxy  = 127
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.outsTarget_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.OutsTarget x k
  getFactKey = Glean.Schema.Buck.Types.outsTarget_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.OutsTarget where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.File_key where
  buildRtsValue b (Glean.Schema.Buck.Types.File_key_source x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Buck.Types.File_key_generated x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Buck.Types.File_key_generatedLabel x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Buck.Types.File_key_source
    , Glean.mapD Glean.Schema.Buck.Types.File_key_generated
    , Glean.mapD Glean.Schema.Buck.Types.File_key_generatedLabel
    ]

type instance Angle.SumFields Glean.Schema.Buck.Types.File_key = 'Angle.TField "source" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "generated" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "generatedLabel" (Glean.KeyType Glean.Schema.Buck.Types.LocatorWithLabel) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Src.Types.File Glean.Schema.Buck.Types.File_key where
  injectBranch = Glean.Schema.Buck.Types.File_key_source
  projectBranch (Glean.Schema.Buck.Types.File_key_source x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Buck.Types.Locator Glean.Schema.Buck.Types.File_key where
  injectBranch = Glean.Schema.Buck.Types.File_key_generated
  projectBranch (Glean.Schema.Buck.Types.File_key_generated x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Buck.Types.LocatorWithLabel Glean.Schema.Buck.Types.File_key where
  injectBranch = Glean.Schema.Buck.Types.File_key_generatedLabel
  projectBranch (Glean.Schema.Buck.Types.File_key_generatedLabel x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Predicate Glean.Schema.Buck.Types.File where
  type KeyType Glean.Schema.Buck.Types.File = Glean.Schema.Buck.Types.File_key
  getName _proxy  = Glean.PredicateRef "buck.File"3
  getIndex _proxy  = 76
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.file_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.File x k
  getFactKey = Glean.Schema.Buck.Types.file_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.File where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.File_1_key where
  buildRtsValue b (Glean.Schema.Buck.Types.File_1_key_source x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Buck.Types.File_1_key_generated x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Buck.Types.File_1_key_source
    , Glean.mapD Glean.Schema.Buck.Types.File_1_key_generated
    ]

type instance Angle.SumFields Glean.Schema.Buck.Types.File_1_key = 'Angle.TField "source" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "generated" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TNoFields))

instance Glean.SumBranches Glean.Schema.Src.Types.File Glean.Schema.Buck.Types.File_1_key where
  injectBranch = Glean.Schema.Buck.Types.File_1_key_source
  projectBranch (Glean.Schema.Buck.Types.File_1_key_source x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Buck.Types.Locator Glean.Schema.Buck.Types.File_1_key where
  injectBranch = Glean.Schema.Buck.Types.File_1_key_generated
  projectBranch (Glean.Schema.Buck.Types.File_1_key_generated x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.Predicate Glean.Schema.Buck.Types.File_1 where
  type KeyType Glean.Schema.Buck.Types.File_1 =
    Glean.Schema.Buck.Types.File_1_key
  getName _proxy  = Glean.PredicateRef "buck.File"1
  getIndex _proxy  = 75
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.file_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.File_1 x k
  getFactKey = Glean.Schema.Buck.Types.file_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.File_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.FileResolved_key where
  buildRtsValue b (Glean.Schema.Buck.Types.FileResolved_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.FileResolved_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.FileResolved_key = 'Angle.TField "buckFile" (Glean.KeyType Glean.Schema.Buck.Types.File) ('Angle.TField "srcFile" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.FileResolved where
  type KeyType Glean.Schema.Buck.Types.FileResolved =
    Glean.Schema.Buck.Types.FileResolved_key
  getName _proxy  = Glean.PredicateRef "buck.FileResolved"3
  getIndex _proxy  = 72
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.fileResolved_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.FileResolved x k
  getFactKey = Glean.Schema.Buck.Types.fileResolved_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.FileResolved where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.Consumer_key where
  buildRtsValue b (Glean.Schema.Buck.Types.Consumer_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.Consumer_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.Consumer_key = 'Angle.TField "source" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "consumer" (Glean.KeyType Glean.Schema.Buck.Types.TargetSources) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.Consumer where
  type KeyType Glean.Schema.Buck.Types.Consumer =
    Glean.Schema.Buck.Types.Consumer_key
  getName _proxy  = Glean.PredicateRef "buck.Consumer"3
  getIndex _proxy  = 66
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.consumer_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.Consumer x k
  getFactKey = Glean.Schema.Buck.Types.consumer_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.Consumer where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TranslationUnit_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TranslationUnit_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buck.Types.TranslationUnit_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TranslationUnit_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "platform" (Prelude.Maybe (Glean.KeyType Glean.Schema.Buck.Types.Platform)) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Buck.Types.TranslationUnit where
  type KeyType Glean.Schema.Buck.Types.TranslationUnit =
    Glean.Schema.Buck.Types.TranslationUnit_key
  getName _proxy  = Glean.PredicateRef "buck.TranslationUnit"2
  getIndex _proxy  = 16
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.translationUnit_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TranslationUnit x k
  getFactKey = Glean.Schema.Buck.Types.translationUnit_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TranslationUnit where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.TranslationUnit_1_key where
  buildRtsValue b (Glean.Schema.Buck.Types.TranslationUnit_1_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Buck.Types.TranslationUnit_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.TranslationUnit_1_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Target_1) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Buck.Types.TranslationUnit_1 where
  type KeyType Glean.Schema.Buck.Types.TranslationUnit_1 =
    Glean.Schema.Buck.Types.TranslationUnit_1_key
  getName _proxy  = Glean.PredicateRef "buck.TranslationUnit"1
  getIndex _proxy  = 15
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Buck.Types.translationUnit_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Buck.Types.TranslationUnit_1 x k
  getFactKey = Glean.Schema.Buck.Types.translationUnit_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Buck.Types.TranslationUnit_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Buck.Types.Dependency where
  buildRtsValue b (Glean.Schema.Buck.Types.Dependency x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Buck.Types.Dependency
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Buck.Types.Dependency = 'Angle.TField "target" (Glean.KeyType Glean.Schema.Buck.Types.Locator) ('Angle.TField "explicit_" (Prelude.Bool) ('Angle.TField "exported" (Prelude.Bool) ('Angle.TNoFields)))
