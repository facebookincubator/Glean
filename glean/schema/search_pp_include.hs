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
import qualified Glean.Schema.Pp1.Types


instance Glean.Type Glean.Schema.SearchPp.Types.SearchByName_key where
  buildRtsValue b (Glean.Schema.SearchPp.Types.SearchByName_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.SearchPp.Types.SearchByName_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchPp.Types.SearchByName_key = 'Angle.TField "macro" (Glean.KeyType Glean.Schema.Pp1.Types.Macro) ('Angle.TField "entity" (Glean.KeyType Glean.Schema.Pp1.Types.Define) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.SearchPp.Types.SearchByName where
  type KeyType Glean.Schema.SearchPp.Types.SearchByName =
    Glean.Schema.SearchPp.Types.SearchByName_key
  getName _proxy  = Glean.PredicateRef "search.pp.SearchByName"2
  getIndex _proxy  = 281
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchPp.Types.searchByName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchPp.Types.SearchByName x k
  getFactKey = Glean.Schema.SearchPp.Types.searchByName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchPp.Types.SearchByName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.SearchPp.Types.SearchByName_1_key where
  buildRtsValue b (Glean.Schema.SearchPp.Types.SearchByName_1_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.SearchPp.Types.SearchByName_1_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.SearchPp.Types.SearchByName_1_key = 'Angle.TField "name" (Data.Text.Text) ('Angle.TField "entity" (Glean.KeyType Glean.Schema.Pp1.Types.Define) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.SearchPp.Types.SearchByName_1 where
  type KeyType Glean.Schema.SearchPp.Types.SearchByName_1 =
    Glean.Schema.SearchPp.Types.SearchByName_1_key
  getName _proxy  = Glean.PredicateRef "search.pp.SearchByName"1
  getIndex _proxy  = 280
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.SearchPp.Types.searchByName_1_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.SearchPp.Types.SearchByName_1 x k
  getFactKey = Glean.Schema.SearchPp.Types.searchByName_1_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.SearchPp.Types.SearchByName_1 where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
