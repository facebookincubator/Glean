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
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.Scm.Types


instance Glean.PredicateQuery Glean.Schema.Scm.Types.RepoType where
  toQueryId = Glean.Schema.Query.Scm.Types.RepoType_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Scm.Types.RepoType_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.RepoType = Glean.Schema.Scm.Types.RepoType
type instance Glean.QueryOf Glean.Schema.Scm.Types.RepoType = Glean.Schema.Query.Scm.Types.RepoType

instance Glean.ToQuery Glean.Schema.Scm.Types.RepoType

instance Glean.PredicateQuery Glean.Schema.Scm.Types.Rev where
  toQueryId = Glean.Schema.Query.Scm.Types.Rev_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Scm.Types.Rev_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.Rev = Glean.Schema.Scm.Types.Rev
type instance Glean.QueryOf Glean.Schema.Scm.Types.Rev = Glean.Schema.Query.Scm.Types.Rev

instance Glean.ToQuery Glean.Schema.Scm.Types.Rev

instance Glean.PredicateQuery Glean.Schema.Scm.Types.RepoName where
  toQueryId = Glean.Schema.Query.Scm.Types.RepoName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Scm.Types.RepoName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.RepoName = Glean.Schema.Scm.Types.RepoName
type instance Glean.QueryOf Glean.Schema.Scm.Types.RepoName = Glean.Schema.Query.Scm.Types.RepoName

instance Glean.ToQuery Glean.Schema.Scm.Types.RepoName

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.Repo_key = Glean.Schema.Scm.Types.Repo_key
type instance Glean.QueryOf Glean.Schema.Scm.Types.Repo_key = Glean.Schema.Query.Scm.Types.Repo_key

instance Glean.ToQuery Glean.Schema.Scm.Types.Repo_key where
  toQuery (Glean.Schema.Scm.Types.Repo_key x1 x2) = Glean.Schema.Query.Scm.Types.Repo_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Scm.Types.Repo where
  toQueryId = Glean.Schema.Query.Scm.Types.Repo_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Scm.Types.Repo_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.Repo = Glean.Schema.Scm.Types.Repo
type instance Glean.QueryOf Glean.Schema.Scm.Types.Repo = Glean.Schema.Query.Scm.Types.Repo

instance Glean.ToQuery Glean.Schema.Scm.Types.Repo

instance Glean.PredicateQuery Glean.Schema.Scm.Types.Timestamp where
  toQueryId = Glean.Schema.Query.Scm.Types.Timestamp_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Scm.Types.Timestamp_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.Timestamp = Glean.Schema.Scm.Types.Timestamp
type instance Glean.QueryOf Glean.Schema.Scm.Types.Timestamp = Glean.Schema.Query.Scm.Types.Timestamp

instance Glean.ToQuery Glean.Schema.Scm.Types.Timestamp

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.Commit_key = Glean.Schema.Scm.Types.Commit_key
type instance Glean.QueryOf Glean.Schema.Scm.Types.Commit_key = Glean.Schema.Query.Scm.Types.Commit_key

instance Glean.ToQuery Glean.Schema.Scm.Types.Commit_key where
  toQuery (Glean.Schema.Scm.Types.Commit_key x1 x2 x3) = Glean.Schema.Query.Scm.Types.Commit_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.Scm.Types.Commit where
  toQueryId = Glean.Schema.Query.Scm.Types.Commit_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Scm.Types.Commit_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Scm.Types.Commit = Glean.Schema.Scm.Types.Commit
type instance Glean.QueryOf Glean.Schema.Scm.Types.Commit = Glean.Schema.Query.Scm.Types.Commit

instance Glean.ToQuery Glean.Schema.Scm.Types.Commit
