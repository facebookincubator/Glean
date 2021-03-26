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

import qualified Glean.Schema.Src.Types


type instance Glean.QueryResult Glean.Schema.Query.Src.Types.ByteSpanContains_key = Glean.Schema.Src.Types.ByteSpanContains_key
type instance Glean.QueryOf Glean.Schema.Src.Types.ByteSpanContains_key = Glean.Schema.Query.Src.Types.ByteSpanContains_key

instance Glean.ToQuery Glean.Schema.Src.Types.ByteSpanContains_key where
  toQuery (Glean.Schema.Src.Types.ByteSpanContains_key x1 x2) = Glean.Schema.Query.Src.Types.ByteSpanContains_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Src.Types.ByteSpanContains where
  toQueryId = Glean.Schema.Query.Src.Types.ByteSpanContains_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Src.Types.ByteSpanContains_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.ByteSpanContains = Glean.Schema.Src.Types.ByteSpanContains
type instance Glean.QueryOf Glean.Schema.Src.Types.ByteSpanContains = Glean.Schema.Query.Src.Types.ByteSpanContains

instance Glean.ToQuery Glean.Schema.Src.Types.ByteSpanContains

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.FileLanguage_key = Glean.Schema.Src.Types.FileLanguage_key
type instance Glean.QueryOf Glean.Schema.Src.Types.FileLanguage_key = Glean.Schema.Query.Src.Types.FileLanguage_key

instance Glean.ToQuery Glean.Schema.Src.Types.FileLanguage_key where
  toQuery (Glean.Schema.Src.Types.FileLanguage_key x1 x2) = Glean.Schema.Query.Src.Types.FileLanguage_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

instance Glean.PredicateQuery Glean.Schema.Src.Types.FileLanguage where
  toQueryId = Glean.Schema.Query.Src.Types.FileLanguage_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Src.Types.FileLanguage_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.FileLanguage = Glean.Schema.Src.Types.FileLanguage
type instance Glean.QueryOf Glean.Schema.Src.Types.FileLanguage = Glean.Schema.Query.Src.Types.FileLanguage

instance Glean.ToQuery Glean.Schema.Src.Types.FileLanguage

instance Glean.PredicateQuery Glean.Schema.Src.Types.File where
  toQueryId = Glean.Schema.Query.Src.Types.File_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Src.Types.File_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.File = Glean.Schema.Src.Types.File
type instance Glean.QueryOf Glean.Schema.Src.Types.File = Glean.Schema.Query.Src.Types.File

instance Glean.ToQuery Glean.Schema.Src.Types.File

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.FileLines_key = Glean.Schema.Src.Types.FileLines_key
type instance Glean.QueryOf Glean.Schema.Src.Types.FileLines_key = Glean.Schema.Query.Src.Types.FileLines_key

instance Glean.ToQuery Glean.Schema.Src.Types.FileLines_key where
  toQuery (Glean.Schema.Src.Types.FileLines_key x1 x2 x3 x4) = Glean.Schema.Query.Src.Types.FileLines_key (Prelude.Just (Glean.toQuery x1)) (Prelude.Just ((Glean.Schema.Query.Src.Types.FileLines_lengths_array_exact . Prelude.map Glean.toQuery) x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4))

instance Glean.PredicateQuery Glean.Schema.Src.Types.FileLines where
  toQueryId = Glean.Schema.Query.Src.Types.FileLines_with_id . Glean.fromFid . Glean.idOf
  toQueryKey = Glean.Schema.Query.Src.Types.FileLines_with_key . Glean.toQuery

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.FileLines = Glean.Schema.Src.Types.FileLines
type instance Glean.QueryOf Glean.Schema.Src.Types.FileLines = Glean.Schema.Query.Src.Types.FileLines

instance Glean.ToQuery Glean.Schema.Src.Types.FileLines

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.RelByteSpan = Glean.Schema.Src.Types.RelByteSpan
type instance Glean.QueryOf Glean.Schema.Src.Types.RelByteSpan = Glean.Schema.Query.Src.Types.RelByteSpan

instance Glean.ToQuery Glean.Schema.Src.Types.RelByteSpan where
  toQuery (Glean.Schema.Src.Types.RelByteSpan x1 x2) = Glean.Schema.Query.Src.Types.RelByteSpan (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.ByteRange = Glean.Schema.Src.Types.ByteRange
type instance Glean.QueryOf Glean.Schema.Src.Types.ByteRange = Glean.Schema.Query.Src.Types.ByteRange

instance Glean.ToQuery Glean.Schema.Src.Types.ByteRange where
  toQuery (Glean.Schema.Src.Types.ByteRange x1 x2) = Glean.Schema.Query.Src.Types.ByteRange (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.Range = Glean.Schema.Src.Types.Range
type instance Glean.QueryOf Glean.Schema.Src.Types.Range = Glean.Schema.Query.Src.Types.Range

instance Glean.ToQuery Glean.Schema.Src.Types.Range where
  toQuery (Glean.Schema.Src.Types.Range x1 x2 x3 x4 x5) = Glean.Schema.Query.Src.Types.Range (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3)) (Prelude.Just (Glean.toQuery x4)) (Prelude.Just (Glean.toQuery x5))

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.Loc = Glean.Schema.Src.Types.Loc
type instance Glean.QueryOf Glean.Schema.Src.Types.Loc = Glean.Schema.Query.Src.Types.Loc

instance Glean.ToQuery Glean.Schema.Src.Types.Loc where
  toQuery (Glean.Schema.Src.Types.Loc x1 x2 x3) = Glean.Schema.Query.Src.Types.Loc (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2)) (Prelude.Just (Glean.toQuery x3))

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.Language = Glean.Schema.Src.Types.Language
type instance Glean.QueryOf Glean.Schema.Src.Types.Language = Glean.Schema.Query.Src.Types.Language

instance Glean.ToQuery Glean.Schema.Src.Types.Language where
  toQuery Glean.Schema.Src.Types.Language_Buck = Glean.Schema.Query.Src.Types.Language_Buck
  toQuery Glean.Schema.Src.Types.Language_C = Glean.Schema.Query.Src.Types.Language_C
  toQuery Glean.Schema.Src.Types.Language_Cpp = Glean.Schema.Query.Src.Types.Language_Cpp
  toQuery Glean.Schema.Src.Types.Language_Hack = Glean.Schema.Query.Src.Types.Language_Hack
  toQuery Glean.Schema.Src.Types.Language_Haskell = Glean.Schema.Query.Src.Types.Language_Haskell
  toQuery Glean.Schema.Src.Types.Language_ObjC = Glean.Schema.Query.Src.Types.Language_ObjC
  toQuery Glean.Schema.Src.Types.Language_ObjCpp = Glean.Schema.Query.Src.Types.Language_ObjCpp
  toQuery Glean.Schema.Src.Types.Language_Python = Glean.Schema.Query.Src.Types.Language_Python
  toQuery Glean.Schema.Src.Types.Language_Thrift = Glean.Schema.Query.Src.Types.Language_Thrift
  toQuery Glean.Schema.Src.Types.Language_Java = Glean.Schema.Query.Src.Types.Language_Java
  toQuery Glean.Schema.Src.Types.Language_GraphQL = Glean.Schema.Query.Src.Types.Language_GraphQL

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.ByteSpan = Glean.Schema.Src.Types.ByteSpan
type instance Glean.QueryOf Glean.Schema.Src.Types.ByteSpan = Glean.Schema.Query.Src.Types.ByteSpan

instance Glean.ToQuery Glean.Schema.Src.Types.ByteSpan where
  toQuery (Glean.Schema.Src.Types.ByteSpan x1 x2) = Glean.Schema.Query.Src.Types.ByteSpan (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))

type instance Glean.QueryResult Glean.Schema.Query.Src.Types.FileLocation = Glean.Schema.Src.Types.FileLocation
type instance Glean.QueryOf Glean.Schema.Src.Types.FileLocation = Glean.Schema.Query.Src.Types.FileLocation

instance Glean.ToQuery Glean.Schema.Src.Types.FileLocation where
  toQuery (Glean.Schema.Src.Types.FileLocation x1 x2) = Glean.Schema.Query.Src.Types.FileLocation (Prelude.Just (Glean.toQuery x1)) (Prelude.Just (Glean.toQuery x2))
