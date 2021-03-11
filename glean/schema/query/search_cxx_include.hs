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
import qualified Glean.Schema.Query.Builtin.Types

import qualified Glean.Schema.CodeCxx.Types
import qualified Glean.Schema.Query.CodeCxx.Types

import qualified Glean.Schema.Cxx1.Types
import qualified Glean.Schema.Query.Cxx1.Types

import qualified Glean.Schema.SearchCxx.Types


type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.EntityUses_key = Glean.Schema.SearchCxx.Types.EntityUses_key
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.EntityUses_key = Glean.Schema.Query.SearchCxx.Types.EntityUses_key

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.EntityUses_key where
  toQuery (Glean.Schema.SearchCxx.Types.EntityUses_key x1 x2) = Glean.Schema.Query.SearchCxx.Types.EntityUses_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.SearchCxx.Types.EntityUses where
  toQueryId = Glean.Schema.Query.SearchCxx.Types.EntityUses_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchCxx.Types.EntityUses_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.EntityUses = Glean.Schema.SearchCxx.Types.EntityUses
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.EntityUses = Glean.Schema.Query.SearchCxx.Types.EntityUses

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.EntityUses

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.SearchByNameAndScope_key = Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key = Glean.Schema.Query.SearchCxx.Types.SearchByNameAndScope_key

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key where
  toQuery (Glean.Schema.SearchCxx.Types.SearchByNameAndScope_key x1 x2 x3) = Glean.Schema.Query.SearchCxx.Types.SearchByNameAndScope_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

instance Glean.PredicateQuery Glean.Schema.SearchCxx.Types.SearchByNameAndScope where
  toQueryId = Glean.Schema.Query.SearchCxx.Types.SearchByNameAndScope_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchCxx.Types.SearchByNameAndScope_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.SearchByNameAndScope = Glean.Schema.SearchCxx.Types.SearchByNameAndScope
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.SearchByNameAndScope = Glean.Schema.Query.SearchCxx.Types.SearchByNameAndScope

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.SearchByNameAndScope

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.GlobalDeclarationWithName_key = Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key = Glean.Schema.Query.SearchCxx.Types.GlobalDeclarationWithName_key

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key where
  toQuery (Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName_key x1 x2) = Glean.Schema.Query.SearchCxx.Types.GlobalDeclarationWithName_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName where
  toQueryId = Glean.Schema.Query.SearchCxx.Types.GlobalDeclarationWithName_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchCxx.Types.GlobalDeclarationWithName_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.GlobalDeclarationWithName = Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName = Glean.Schema.Query.SearchCxx.Types.GlobalDeclarationWithName

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.GlobalDeclarationWithName

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.DeclIsDefn_key = Glean.Schema.SearchCxx.Types.DeclIsDefn_key
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.DeclIsDefn_key = Glean.Schema.Query.SearchCxx.Types.DeclIsDefn_key

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.DeclIsDefn_key where
  toQuery (Glean.Schema.SearchCxx.Types.DeclIsDefn_key x1 x2) = Glean.Schema.Query.SearchCxx.Types.DeclIsDefn_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.SearchCxx.Types.DeclIsDefn where
  toQueryId = Glean.Schema.Query.SearchCxx.Types.DeclIsDefn_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchCxx.Types.DeclIsDefn_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.DeclIsDefn = Glean.Schema.SearchCxx.Types.DeclIsDefn
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.DeclIsDefn = Glean.Schema.Query.SearchCxx.Types.DeclIsDefn

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.DeclIsDefn

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.SearchBySelector_key = Glean.Schema.SearchCxx.Types.SearchBySelector_key
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.SearchBySelector_key = Glean.Schema.Query.SearchCxx.Types.SearchBySelector_key

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.SearchBySelector_key where
  toQuery (Glean.Schema.SearchCxx.Types.SearchBySelector_key x1 x2) = Glean.Schema.Query.SearchCxx.Types.SearchBySelector_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.SearchCxx.Types.SearchBySelector where
  toQueryId = Glean.Schema.Query.SearchCxx.Types.SearchBySelector_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.SearchCxx.Types.SearchBySelector_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.SearchCxx.Types.SearchBySelector = Glean.Schema.SearchCxx.Types.SearchBySelector
type instance Glean.QueryOf Glean.Schema.SearchCxx.Types.SearchBySelector = Glean.Schema.Query.SearchCxx.Types.SearchBySelector

instance Glean.ToQuery Glean.Schema.SearchCxx.Types.SearchBySelector
