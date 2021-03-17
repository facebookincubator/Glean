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


instance Glean.Predicate Glean.Schema.Sys.Types.Blob where
  type KeyType Glean.Schema.Sys.Types.Blob = Data.ByteString.ByteString
  getName _proxy  = Glean.PredicateRef "sys.Blob"1
  getIndex _proxy  = 10
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Sys.Types.blob_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Sys.Types.Blob x k
  getFactKey = Glean.Schema.Sys.Types.blob_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Sys.Types.Blob where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
