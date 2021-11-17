{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
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

data PidRef = PidRef Pid PredicateRef
  deriving Show

instance Eq PidRef where
  PidRef a _ == PidRef b _ = a == b

instance Ord PidRef where
  compare (PidRef a _) (PidRef b _) = compare a b

instance Pretty PidRef where
  pretty (PidRef _ ref) = pretty ref

data ExpandedType = ExpandedType TypeRef Type
  deriving (Show, Eq)

instance Pretty ExpandedType where
  pretty (ExpandedType ref _) = pretty ref

type Type = Type_ PidRef ExpandedType
type FieldDef = FieldDef_ PidRef ExpandedType


-- | Construct the representation of a Type
repType :: Type -> Rep Pid
repType Byte = ByteRep
repType Nat = NatRep
repType String = StringRep
repType (Array ty) = ArrayRep (repType ty)
repType (Record fields) =
  TupleRep [ repType ty | FieldDef _ ty <- fields ]
repType (Sum fields) =
  SumRep [ repType ty | FieldDef _ ty <- fields ]
repType (Predicate (PidRef pid _)) = PredicateRep pid
repType (NamedType (ExpandedType _ ty)) = repType ty
repType (Maybe ty) = repType (lowerMaybe ty)
repType (Enumerated names) = repType (lowerEnum names)
repType Boolean = repType lowerBool

sumLike :: Type -> Maybe [Glean.RTS.Types.FieldDef]
sumLike (Sum fs) = Just fs
sumLike (Maybe ty) = Just (maybeFields ty)
sumLike (Enumerated names) = Just (enumFields names)
sumLike Boolean = Just boolFields
sumLike _ = Nothing

-- | Compare types for (structural) equality
eqType :: Type -> Type -> Bool
eqType a b = case (a,b) of
  (Byte, Byte) -> True
  (Nat, Nat) -> True
  (String, String) -> True
  (Array a, Array b) -> a `eqType` b
  (Record as, Record bs) ->
    -- structural equality for records, so that e.g. tuples work as records
    length as == length bs &&
    and [ eqType a b | (FieldDef _ a, FieldDef _ b) <- zip as bs ]
  (Sum as, Sum bs) ->
    length as == length bs &&
    and [ eqType a b | (FieldDef _ a, FieldDef _ b) <- zip as bs ]
  (Predicate (PidRef p _), Predicate (PidRef q _)) -> p == q
  (NamedType (ExpandedType n t), NamedType (ExpandedType m u)) ->
    n == m || t `eqType` u
  (NamedType (ExpandedType _ t), u) -> t `eqType` u
  (t, NamedType (ExpandedType _ u)) -> t `eqType` u
  (Maybe t, Maybe u) -> t `eqType` u
  (Maybe t, u) -> lowerMaybe t `eqType` u
  (t, Maybe u) -> t `eqType` lowerMaybe u
  (Enumerated xs, Enumerated ys) -> xs == ys
  (Enumerated xs, t) -> lowerEnum xs `eqType` t
  (t, Enumerated xs) -> t `eqType` lowerEnum xs
  (Boolean, Boolean) -> True
  (Boolean, t) -> lowerBool `eqType` t
  (t, Boolean) -> t `eqType` lowerBool
  _ -> False

-- | dereference NamedType on the outside of a Type
derefType :: Type -> Type
derefType (NamedType (ExpandedType _ ty)) = derefType ty
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
