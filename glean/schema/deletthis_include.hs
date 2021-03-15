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
import qualified Glean.Schema.Code.Types
import qualified Glean.Schema.Src.Types


instance Glean.Type Glean.Schema.Deletthis.Types.FileReverseDeps_key where
  buildRtsValue b (Glean.Schema.Deletthis.Types.FileReverseDeps_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Deletthis.Types.FileReverseDeps_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Deletthis.Types.FileReverseDeps_key = 'Angle.TField "file" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "referenced_by" (Glean.KeyType Glean.Schema.Src.Types.File) ('Angle.TField "via" (Glean.Schema.Code.Types.Entity) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Deletthis.Types.FileReverseDeps where
  type KeyType Glean.Schema.Deletthis.Types.FileReverseDeps =
    Glean.Schema.Deletthis.Types.FileReverseDeps_key
  getName _proxy  = Glean.PredicateRef "deletthis.FileReverseDeps"3
  getIndex _proxy  = 177
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Deletthis.Types.fileReverseDeps_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Deletthis.Types.FileReverseDeps x k
  getFactKey = Glean.Schema.Deletthis.Types.fileReverseDeps_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Deletthis.Types.FileReverseDeps where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
