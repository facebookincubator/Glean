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
import qualified Glean.Schema.Testinfra.Types


instance Glean.Type Glean.Schema.Lionhead.Types.CoveredHarness_key where
  buildRtsValue b (Glean.Schema.Lionhead.Types.CoveredHarness_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Lionhead.Types.CoveredHarness_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Lionhead.Types.CoveredHarness_key = 'Angle.TField "harnessId" (Glean.KeyType Glean.Schema.Lionhead.Types.FbId) ('Angle.TField "root" (Glean.KeyType Glean.Schema.Testinfra.Types.CoveredFolder) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Lionhead.Types.CoveredHarness where
  type KeyType Glean.Schema.Lionhead.Types.CoveredHarness =
    Glean.Schema.Lionhead.Types.CoveredHarness_key
  getName _proxy  = Glean.PredicateRef "lionhead.CoveredHarness"1
  getIndex _proxy  = 478
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Lionhead.Types.coveredHarness_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Lionhead.Types.CoveredHarness x k
  getFactKey = Glean.Schema.Lionhead.Types.coveredHarness_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Lionhead.Types.CoveredHarness where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Lionhead.Types.FbId where
  type KeyType Glean.Schema.Lionhead.Types.FbId = Glean.Nat
  getName _proxy  = Glean.PredicateRef "lionhead.FbId"1
  getIndex _proxy  = 435
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Lionhead.Types.fbId_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Lionhead.Types.FbId x k
  getFactKey = Glean.Schema.Lionhead.Types.fbId_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Lionhead.Types.FbId where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
