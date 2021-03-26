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


instance Glean.Predicate Glean.Schema.Scm.Types.RepoType where
  type KeyType Glean.Schema.Scm.Types.RepoType = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "scm.RepoType"1
  getIndex _proxy  = 427
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Scm.Types.repoType_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Scm.Types.RepoType x k
  getFactKey = Glean.Schema.Scm.Types.repoType_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Scm.Types.RepoType where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Scm.Types.Rev where
  type KeyType Glean.Schema.Scm.Types.Rev = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "scm.Rev"1
  getIndex _proxy  = 339
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Scm.Types.rev_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Scm.Types.Rev x k
  getFactKey = Glean.Schema.Scm.Types.rev_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Scm.Types.Rev where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Scm.Types.RepoName where
  type KeyType Glean.Schema.Scm.Types.RepoName = Data.Text.Text
  getName _proxy  = Glean.PredicateRef "scm.RepoName"1
  getIndex _proxy  = 303
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Scm.Types.repoName_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Scm.Types.RepoName x k
  getFactKey = Glean.Schema.Scm.Types.repoName_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Scm.Types.RepoName where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Scm.Types.Repo_key where
  buildRtsValue b (Glean.Schema.Scm.Types.Repo_key x1 x2) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
  decodeRtsValue = Glean.Schema.Scm.Types.Repo_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Scm.Types.Repo_key = 'Angle.TField "name" (Glean.KeyType Glean.Schema.Scm.Types.RepoName) ('Angle.TField "type" (Glean.KeyType Glean.Schema.Scm.Types.RepoType) ('Angle.TNoFields))

instance Glean.Predicate Glean.Schema.Scm.Types.Repo where
  type KeyType Glean.Schema.Scm.Types.Repo = Glean.Schema.Scm.Types.Repo_key
  getName _proxy  = Glean.PredicateRef "scm.Repo"1
  getIndex _proxy  = 192
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Scm.Types.repo_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Scm.Types.Repo x k
  getFactKey = Glean.Schema.Scm.Types.repo_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Scm.Types.Repo where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Predicate Glean.Schema.Scm.Types.Timestamp where
  type KeyType Glean.Schema.Scm.Types.Timestamp = Glean.Nat
  getName _proxy  = Glean.PredicateRef "scm.Timestamp"1
  getIndex _proxy  = 121
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Scm.Types.timestamp_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Scm.Types.Timestamp x k
  getFactKey = Glean.Schema.Scm.Types.timestamp_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Scm.Types.Timestamp where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef

instance Glean.Type Glean.Schema.Scm.Types.Commit_key where
  buildRtsValue b (Glean.Schema.Scm.Types.Commit_key x1 x2 x3) = do
    Glean.buildRtsValue b x1
    Glean.buildRtsValue b x2
    Glean.buildRtsValue b x3
  decodeRtsValue = Glean.Schema.Scm.Types.Commit_key
    <$> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue
    <*> Glean.decodeRtsValue

type instance Angle.RecordFields Glean.Schema.Scm.Types.Commit_key = 'Angle.TField "repo" (Glean.KeyType Glean.Schema.Scm.Types.Repo) ('Angle.TField "rev" (Glean.KeyType Glean.Schema.Scm.Types.Rev) ('Angle.TField "timestamp" (Glean.KeyType Glean.Schema.Scm.Types.Timestamp) ('Angle.TNoFields)))

instance Glean.Predicate Glean.Schema.Scm.Types.Commit where
  type KeyType Glean.Schema.Scm.Types.Commit =
    Glean.Schema.Scm.Types.Commit_key
  getName _proxy  = Glean.PredicateRef "scm.Commit"1
  getIndex _proxy  = 93
  getId = Glean.IdOf . Glean.Fid . Glean.Schema.Scm.Types.commit_id
  mkFact (Glean.IdOf (Glean.Fid x)) k _ = Glean.Schema.Scm.Types.Commit x k
  getFactKey = Glean.Schema.Scm.Types.commit_key
  getFactValue _ = Prelude.Just ()

instance Glean.Type Glean.Schema.Scm.Types.Commit where
  buildRtsValue b = Glean.buildRtsValue b . Glean.getId
  decodeRtsValue = Glean.decodeRef
