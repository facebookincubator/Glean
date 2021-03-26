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


instance Glean.Predicate Glean.Schema.Omegaanalyser.Types.OmegaPolicy where
  type KeyType Glean.Schema.Omegaanalyser.Types.OmegaPolicy = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "omegaanalyser.OmegaPolicy"2
  getIndex _proxy  = 498
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Omegaanalyser.Types.omegaPolicy_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Omegaanalyser.Types.OmegaPolicy x k
  getFactKey = Glean.Schema.Omegaanalyser.Types.omegaPolicy_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Omegaanalyser.Types.OmegaPolicy where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Omegaanalyser.Types.Class_ where
  type KeyType Glean.Schema.Omegaanalyser.Types.Class_ = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "omegaanalyser.Class_"1
  getIndex _proxy  = 483
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Omegaanalyser.Types.class__id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Omegaanalyser.Types.Class_ x k
  getFactKey = Glean.Schema.Omegaanalyser.Types.class__key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Omegaanalyser.Types.Class_ where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Omegaanalyser.Types.DependencyList_key where
  buildRtsValue b (Glean.Schema.Omegaanalyser.Types.DependencyList_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Omegaanalyser.Types.DependencyList_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Omegaanalyser.Types.DependencyList_key = 'Angle.TField "node" (Glean.Schema.Omegaanalyser.Types.Node) ('Angle.TField "endpoints" ([Glean.KeyType Glean.Schema.Omegaanalyser.Types.OmegaEndpoint]) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Omegaanalyser.Types.DependencyList where
  type KeyType Glean.Schema.Omegaanalyser.Types.DependencyList =
    Glean.Schema.Omegaanalyser.Types.DependencyList_key
  getName _proxy  = Glean.PredicateRef "omegaanalyser.DependencyList"1
  getIndex _proxy  = 377
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Omegaanalyser.Types.dependencyList_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Omegaanalyser.Types.DependencyList x k
  getFactKey = Glean.Schema.Omegaanalyser.Types.dependencyList_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Omegaanalyser.Types.DependencyList where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Omegaanalyser.Types.Function_ where
  type KeyType Glean.Schema.Omegaanalyser.Types.Function_ = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "omegaanalyser.Function_"1
  getIndex _proxy  = 368
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Omegaanalyser.Types.function__id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Omegaanalyser.Types.Function_ x k
  getFactKey = Glean.Schema.Omegaanalyser.Types.function__key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Omegaanalyser.Types.Function_ where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Omegaanalyser.Types.OmegaEndpoint where
  type KeyType Glean.Schema.Omegaanalyser.Types.OmegaEndpoint = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "omegaanalyser.OmegaEndpoint"1
  getIndex _proxy  = 353
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Omegaanalyser.Types.omegaEndpoint_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Omegaanalyser.Types.OmegaEndpoint x k
  getFactKey = Glean.Schema.Omegaanalyser.Types.omegaEndpoint_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Omegaanalyser.Types.OmegaEndpoint where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Omegaanalyser.Types.Method where
  type KeyType Glean.Schema.Omegaanalyser.Types.Method = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "omegaanalyser.Method"1
  getIndex _proxy  = 310
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Omegaanalyser.Types.method_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Omegaanalyser.Types.Method x k
  getFactKey = Glean.Schema.Omegaanalyser.Types.method_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Omegaanalyser.Types.Method where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Omegaanalyser.Types.Node where
  buildRtsValue b (Glean.Schema.Omegaanalyser.Types.Node_class_ x) = do
    Glean.buildRtsSelector b 0
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Omegaanalyser.Types.Node_method x) = do
    Glean.buildRtsSelector b 1
    Glean.buildRtsValue b x
  buildRtsValue b (Glean.Schema.Omegaanalyser.Types.Node_function_ x) = do
    Glean.buildRtsSelector b 2
    Glean.buildRtsValue b x
  decodeRtsValue = Glean.sumD
    [ Glean.mapD Glean.Schema.Omegaanalyser.Types.Node_class_
    , Glean.mapD Glean.Schema.Omegaanalyser.Types.Node_method
    , Glean.mapD Glean.Schema.Omegaanalyser.Types.Node_function_
    ]

type instance Angle.SumFields Glean.Schema.Omegaanalyser.Types.Node = 'Angle.TField "class_" (Glean.KeyType Glean.Schema.Omegaanalyser.Types.Class_) ('Angle.TField "method" (Glean.KeyType Glean.Schema.Omegaanalyser.Types.Method) ('Angle.TField "function_" (Glean.KeyType Glean.Schema.Omegaanalyser.Types.Function_) ('Angle.TNoFields)))

instance Glean.SumBranches Glean.Schema.Omegaanalyser.Types.Class_ Glean.Schema.Omegaanalyser.Types.Node where
  injectBranch = Glean.Schema.Omegaanalyser.Types.Node_class_
  projectBranch (Glean.Schema.Omegaanalyser.Types.Node_class_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Omegaanalyser.Types.Method Glean.Schema.Omegaanalyser.Types.Node where
  injectBranch = Glean.Schema.Omegaanalyser.Types.Node_method
  projectBranch (Glean.Schema.Omegaanalyser.Types.Node_method x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing

instance Glean.SumBranches Glean.Schema.Omegaanalyser.Types.Function_ Glean.Schema.Omegaanalyser.Types.Node where
  injectBranch = Glean.Schema.Omegaanalyser.Types.Node_function_
  projectBranch (Glean.Schema.Omegaanalyser.Types.Node_function_ x) = Prelude.Just x
  projectBranch _ = Prelude.Nothing
