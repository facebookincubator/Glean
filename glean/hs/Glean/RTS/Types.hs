{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE UnboxedTuples #-}

-- required for deriving 'Prim' below (at least with GHC 8.4),
-- see https://gitlab.haskell.org/ghc/ghc/-/issues/15073
{-# LANGUAGE TypeInType #-}

module Glean.RTS.Types
  ( -- * fact types
    -- ** type
    Id
    -- ** newtype
  , Fid(..), invalidFid, lowestFid
    -- ** phantom type
  , Pid(..), invalidPid, lowestPid
    -- * Types
  , Type
  , Glean.RTS.Types.FieldDef
  , PidRef(..)
  , ExpandedType(..)
  , repType
  , sumLike
  , eqType
  , derefType
    -- ** Representation types
  , Rep(..)
) where

import Control.DeepSeq
import Data.Hashable
import Data.Text.Prettyprint.Doc
import Data.Vector.Primitive (Prim)
import Foreign

import Glean.Types (Id, fIRST_FREE_ID)
import Glean.Angle.Types hiding (Type)
import Glean.Schema.Util

-- | 'Pid' holds the glean Id corresponding to the definition of a predicate.
--
-- As a Fid that Id points to the glean fact that holds the predicate name and
-- key type and value type).
--
-- Querying for this Id (as prefix of key) gives all facts for this predicate.
newtype Pid = Pid { fromPid :: Int64 }
  deriving(Eq,Ord,Enum,Hashable,Storable,Show,NFData,Prim)

instance Pretty Pid where
  pretty (Pid n) = "{" <> pretty n <> "}"

invalidPid :: Pid
invalidPid = Pid 0

lowestPid :: Pid
lowestPid = Pid fIRST_FREE_ID

-- | Id of a fact that points to an entry in the database.
-- If this entry happens to be a predicate definition then this
-- is also logically a Pid.  If this entry is fact about some
-- predicate 'p' then this is also logically an 'IdOf p'.
newtype Fid = Fid { fromFid :: Id }
  deriving(Eq,Ord,Enum,Hashable,Storable,Show,NFData,Prim)

instance Pretty Fid where
  pretty (Fid n) = "{" <> pretty n <> "}"

invalidFid :: Fid
invalidFid = Fid 0

lowestFid :: Fid
lowestFid = Fid fIRST_FREE_ID

-- -----------------------------------------------------------------------------
-- Runtime types

data PidRef = PidRef Pid PredicateId
  deriving Show

instance Eq PidRef where
  PidRef a _ == PidRef b _ = a == b

instance Ord PidRef where
  compare (PidRef a _) (PidRef b _) = compare a b

instance Pretty PidRef where
  pretty (PidRef _ ref) = pretty ref

data ExpandedType = ExpandedType TypeId Type
  deriving (Show, Eq)

instance Pretty ExpandedType where
  pretty (ExpandedType ref _) = pretty ref

type Type = Type_ PidRef ExpandedType
type FieldDef = FieldDef_ PidRef ExpandedType


-- | Construct the representation of a Type
repType :: Type -> Rep Pid
repType ByteTy = ByteRep
repType NatTy = NatRep
repType StringTy = StringRep
repType (ArrayTy ty) = ArrayRep (repType ty)
repType (RecordTy fields) =
  TupleRep [ repType ty | FieldDef _ ty <- fields ]
repType (SumTy fields) =
  SumRep [ repType ty | FieldDef _ ty <- fields ]
repType (PredicateTy (PidRef pid _)) = PredicateRep pid
repType (NamedTy (ExpandedType _ ty)) = repType ty
repType (MaybeTy ty) = repType (lowerMaybe ty)
repType (EnumeratedTy names) = repType (lowerEnum names)
repType BooleanTy = repType lowerBool

sumLike :: Type -> Maybe [Glean.RTS.Types.FieldDef]
sumLike (SumTy fs) = Just fs
sumLike (MaybeTy ty) = Just (maybeFields ty)
sumLike (EnumeratedTy names) = Just (enumFields names)
sumLike BooleanTy = Just boolFields
sumLike _ = Nothing

-- | Compare types for (structural) equality
eqType :: Type -> Type -> Bool
eqType a b = case (a,b) of
  (ByteTy, ByteTy) -> True
  (NatTy, NatTy) -> True
  (StringTy, StringTy) -> True
  (ArrayTy a, ArrayTy b) -> a `eqType` b
  (RecordTy as, RecordTy bs) ->
    -- structural equality for records, so that e.g. tuples work as records
    length as == length bs &&
    and [ eqType a b | (FieldDef _ a, FieldDef _ b) <- zip as bs ]
  (SumTy as, SumTy bs) ->
    length as == length bs &&
    and [ eqType a b | (FieldDef _ a, FieldDef _ b) <- zip as bs ]
  (PredicateTy (PidRef p _), PredicateTy (PidRef q _)) -> p == q
  (NamedTy (ExpandedType n t), NamedTy (ExpandedType m u)) ->
    n == m || t `eqType` u
  (NamedTy (ExpandedType _ t), u) -> t `eqType` u
  (t, NamedTy (ExpandedType _ u)) -> t `eqType` u
  (MaybeTy t, MaybeTy u) -> t `eqType` u
  (MaybeTy t, u) -> lowerMaybe t `eqType` u
  (t, MaybeTy u) -> t `eqType` lowerMaybe u
  (EnumeratedTy xs, EnumeratedTy ys) -> xs == ys
  (EnumeratedTy xs, t) -> lowerEnum xs `eqType` t
  (t, EnumeratedTy xs) -> t `eqType` lowerEnum xs
  (BooleanTy, BooleanTy) -> True
  (BooleanTy, t) -> lowerBool `eqType` t
  (t, BooleanTy) -> t `eqType` lowerBool
  _ -> False

-- | dereference NamedTy on the outside of a Type
derefType :: Type -> Type
derefType (NamedTy (ExpandedType _ ty)) = derefType ty
derefType ty = ty

-- | Type describing the raw representation of a value. This makes
-- fewer distinctions than Type, in particular: there are no field
-- names or named types, and sugar like maybe, bool and enumerations
-- are expanded into their low-level representations.
data Rep id
  = ByteRep
  | NatRep
  | ArrayRep (Rep id)
  | TupleRep [Rep id]
  | SumRep [Rep id]
  | StringRep
  | PredicateRep id
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Pretty id => Pretty (Rep id) where
  pretty ByteRep = "byte#"
  pretty NatRep = "nat#"
  pretty (ArrayRep ty) = "[" <> pretty ty <> "]"
  pretty (TupleRep ty) = align $ encloseSep "(" ")" "," $ map pretty ty
  pretty (SumRep ty) = align $ encloseSep "(|" "|)" "," $ map pretty ty
  pretty StringRep = "string#"
  pretty (PredicateRep id) = pretty id
