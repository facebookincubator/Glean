{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-star-is-type #-}

-- | Types representing a source-level schema
module Glean.Angle.Types
  ( -- * Names and references
    Name
  , FieldName
  , Version
  , PredicateRef(..)
  , TypeRef(..)
  , PredicateId(..)
  , tempPredicateId
  , TypeId(..)

  -- * Source locations
  , IsSrcSpan(..)
  , SrcSpan(..)
  , SrcLoc(..)
  , rmLocSchemas
  , rmLocSchema
  , rmLocEvolves
  , rmLocDecl
  , rmLocQuery
  , rmLocStatement
  , rmLocPat
  , rmLocField
  , rmLocTypeDef
  , rmLocType
  , rmLocPredDef
  , rmTypeLocPredDef

  -- * Types
  , Type_(..)
  , RecordOrSum(..)

  -- * Queries
  , SourceQuery_(..)
  , Ordered(..)
  , SourceStatement_(..)
  , SourcePat_(..)
  , PrimOp(..)
  , SeekSection(..)

  -- * Schemas and definitions
  , DerivingDef_(..)
  , TypeDef_(..)
  , FieldDef_(..)
  , PredicateDef_(..)
  , Statement_
  , Query_
  , Type
  , FieldDef
  , TypeDef
  , Field(..)
  , IsWild(..)
  , tupleField
  , sourcePatSpan
  , spanBetween
  , PredicateDef
  , DerivingInfo(..)
  , DeriveWhen(..)
  , SourceSchema
  , SourceSchemas
  , SourceEvolves
  , SourceDecl
  , SourceRef(..)
  , SourceType
  , SourceFieldDef
  , SourceTypeDef
  , SourcePredicateDef
  , SourceDerivingInfo
  , SourcePat
  , SourceStatement
  , SourceQuery
  , SourcePat'
  , SourceStatement'
  , SourceQuery'
  , SourceDerivingInfo'
  , DerivingDef'
  , SourceType'
  , Type'
  , FieldDef'
  , PredicateDef'
  , SourceSchemas_(..)
  , SourceEvolves_(..)
  , SourceSchema_(..)
  , SourceDecl_(..)

  -- * Versions of the Angle syntax
  , AngleVersion(..)
  , latestAngleVersion
  , latestSupportedAngleVersion

  -- * Pretty printing
  , displayStatement

  -- * Description
  , Describe(..)
  ) where

import qualified Data.Aeson as Aeson
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import Data.List
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Compat.Prettyprinter
import Data.Bifunctor
import Data.Bifoldable
import GHC.Generics

import Glean.Angle.Hash
import Glean.Display
import Glean.Types (PredicateRef(..), TypeRef(..), Version)


-- -----------------------------------------------------------------------------
-- Names and references

type Name = Text

type FieldName = Text

-- | A reference to a name or type in a source schema. Versions are
-- often omitted at this level, and will be later resolved by
-- resolveSchema.
data SourceRef = SourceRef
  { sourceRefName :: Name
  , sourceRefVersion :: Maybe Version
  }
  deriving (Show, Eq, Ord, Generic, Hashable)

instance Binary SourceRef

-- -----------------------------------------------------------------------------
-- Source locations

-- | A 'SrcSpan' delimits a portion of a text file.
-- The end position is the column /after/ the end of the span.
-- That is, a span of (1,1)-(1,2) is one character long, and a span of
-- (1,1)-(1,1) is zero characters long.
data SrcSpan = SrcSpan
  { spanStart :: {-# UNPACK #-} !SrcLoc -- inclusive
  , spanEnd   :: {-# UNPACK #-} !SrcLoc -- exclusive
  }
  deriving (Eq, Show)

-- | A point in a source text file.
data SrcLoc = SrcLoc
  { locLine :: {-# UNPACK #-} !Int
  , locCol  :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq)

-- -----------------------------------------------------------------------------
-- Queries

data SourceQuery_ s st p t = SourceQuery
  { srcQueryHead :: Maybe (SourcePat_ s st p t)
  , srcQueryStmts :: [SourceStatement_ s st p t]
  , srcQueryOrdered :: Ordered
  }
  deriving (Eq, Show, Generic, Functor)

data Ordered = Ordered | Unordered
  deriving (Eq, Show, Generic)

instance Binary Ordered

instance (Binary p, Binary t) => Binary (SourceQuery_ () () p t)

instance Bifunctor (SourceQuery_ s st) where
  bimap f g (SourceQuery h s o) =
    SourceQuery (fmap (bimap f g) h) (fmap (bimap f g) s) o

instance Bifoldable (SourceQuery_ s st ) where
  bifoldMap f g (SourceQuery h s _) =
    foldMap (bifoldMap f g) h <> foldMap (bifoldMap f g) s

data SourceStatement_ s st p t =
  SourceStatement (SourcePat_ s st p t) (SourcePat_ s st p t)
  deriving (Eq, Show, Generic, Functor)

instance (Binary p, Binary t) => Binary (SourceStatement_ () () p t)

instance Bifunctor (SourceStatement_ s st) where
  bimap f g (SourceStatement l r) = SourceStatement (bimap f g l) (bimap f g r)

instance Bifoldable (SourceStatement_ s st) where
  bifoldMap f g (SourceStatement l r) = bifoldMap f g l <> bifoldMap f g r

data SourcePat_ s st p t
  = Nat s Word64
  | String s Text
  | StringPrefix s Text
  | ByteArray s ByteString
    -- ^ There's no concrete syntax for this (yet), but it can be used
    -- via the DSL.
  | Array s [SourcePat_ s st p t]
  | ArrayPrefix s (NonEmpty (SourcePat_ s st p t))
  | Tuple s [SourcePat_ s st p t]
  | Struct s [Field s st p t]
  | App s (SourcePat_ s st p t) [SourcePat_ s st p t]
  | KeyValue s (SourcePat_ s st p t) (SourcePat_ s st p t)
  | Elements s (SourcePat_ s st p t)
  | All s (SourcePat_ s st p t)
  | Wildcard s
  | Variable s Name
  | ElementsOfArray s (SourcePat_ s st p t)
  | OrPattern s (SourcePat_ s st p t) (SourcePat_ s st p t)
  | NestedQuery s (SourceQuery_ s st p t)
  | Negation s (SourcePat_ s st p t)
  | FactId s (Maybe Text) Word64
  | TypeSignature s (SourcePat_ s st p t) (Type_ st p t)
  | Never s
  | IfPattern
      { span :: s
      , cond :: SourcePat_ s st p t
      , then_ :: SourcePat_ s st p t
      , else_ :: SourcePat_ s st p t
      }
  | FieldSelect s (SourcePat_ s st p t) FieldName RecordOrSum
  | Deref s (SourcePat_ s st p t)
  | Enum s Text

  -- The following forms are introduced by the resolver, and replace
  -- the Variable and App forms produced by the parser.
  | Clause s s p (SourcePat_ s st p t) SeekSection
  | Prim s PrimOp [SourcePat_ s st p t]
 deriving (Eq, Show, Generic, Functor)

-- | Should a `seek` call be restricted to a section of the database?
--
-- When performing a query over a stacked database we have three levels
--  * base db
--  * stacked db
--  * writable FactSet
--
-- The writable FactSet is there to store facts from derived predicates
-- generated during the querying.
--
-- This type specifies which of these sections should be be covered in a seek.
data SeekSection
  = SeekOnAllFacts -- ^ base + stacked + writable
  | SeekOnBase -- ^ base only
  | SeekOnStacked -- ^ stacked only
  deriving (Eq, Show, Generic)

instance Binary SeekSection

instance (Binary p, Binary t) => Binary (SourcePat_ () () p t)

instance Bifunctor (SourcePat_ s st) where
  bimap f g pat = case pat of
    Nat s w -> Nat s w
    String s t -> String s t
    StringPrefix s t -> StringPrefix s t
    ByteArray s t -> ByteArray s t
    Array s pats -> Array s (fmap (bimap f g) pats)
    ArrayPrefix s pats -> ArrayPrefix s (fmap (bimap f g) pats)
    Tuple s pats -> Tuple s (map (bimap f g) pats)
    Struct s fields -> Struct s (map (bimap f g) fields)
    App s l r -> App s (bimap f g l) (map (bimap f g) r)
    KeyValue s k v -> KeyValue s (bimap f g k) (bimap f g v)
    Elements s pat -> Elements s (bimap f g pat)
    All s query -> All s (bimap f g query)
    Wildcard s -> Wildcard s
    Variable s n -> Variable s n
    ElementsOfArray s pat -> ElementsOfArray s (bimap f g pat)
    OrPattern s l r -> OrPattern s (bimap f g l) (bimap f g r)
    NestedQuery s (SourceQuery head stmts ord) ->
      NestedQuery s (SourceQuery (fmap (bimap f g) head)
        (fmap (bimap f g) stmts) ord)
    Negation s pat -> Negation s (bimap f g pat)
    FactId s n w -> FactId s n w
    TypeSignature s pat ty -> TypeSignature s (bimap f g pat) (bimap f g ty)
    Never s -> Never s
    IfPattern s a b c -> IfPattern s (bimap f g a) (bimap f g b) (bimap f g c)
    FieldSelect s pat field q -> FieldSelect s (bimap f g pat) field q
    Deref s pat -> Deref s (bimap f g pat)
    Enum s f -> Enum s f
    Clause sPat sPred p pat rng -> Clause sPat sPred (f p) (bimap f g pat) rng
    Prim s p pats -> Prim s p (fmap (bimap f g) pats)

instance Bifoldable (SourcePat_ s st) where
  bifoldMap f g = \case
    Nat{} -> mempty
    String{} -> mempty
    StringPrefix{} -> mempty
    ByteArray{} -> mempty
    Array _ pats -> foldMap (bifoldMap f g) pats
    ArrayPrefix _ pats -> foldMap (bifoldMap f g) pats
    Tuple _ pats -> foldMap (bifoldMap f g) pats
    Struct _ fields -> foldMap (bifoldMap f g) fields
    App _ l r -> foldMap (bifoldMap f g) (l:r)
    KeyValue _ k v -> bifoldMap f g k <> bifoldMap f g v
    Elements _ pat -> bifoldMap f g pat
    All _ query -> bifoldMap f g query
    Wildcard{} -> mempty
    Variable{} -> mempty
    ElementsOfArray _ pat -> bifoldMap f g pat
    OrPattern _ l r -> bifoldMap f g l <> bifoldMap f g r
    NestedQuery _ (SourceQuery head stmts _) ->
      foldMap (bifoldMap f g) head <> foldMap (bifoldMap f g) stmts
    Negation _ pat -> bifoldMap f g pat
    FactId{} -> mempty
    TypeSignature _ pat ty -> bifoldMap f g pat <> bifoldMap f g ty
    Never{} -> mempty
    IfPattern _ a b c -> bifoldMap f g a <> bifoldMap f g b <> bifoldMap f g c
    FieldSelect _ pat _ _ -> bifoldMap f g pat
    Deref _ pat -> bifoldMap f g pat
    Enum _ _ -> mempty
    Clause _ _ p pat _ -> f p <> bifoldMap f g pat
    Prim _ _ pats -> foldMap (bifoldMap f g) pats

data Field s st p t = Field FieldName (SourcePat_ s st p t)
  deriving (Eq, Show, Generic, Functor)

instance (Binary p, Binary t) => Binary (Field () () p t)

instance Bifunctor (Field s st) where
  bimap f g (Field n pat) = Field n (bimap f g pat)

instance Bifoldable (Field s st) where
  bifoldMap f g (Field _ pat) = bifoldMap f g pat

-- | Primitive operations
data PrimOp
  = PrimOpToLower
  | PrimOpLength
  | PrimOpZip
  | PrimOpConcat
  | PrimOpReverse
  | PrimOpSize
  | PrimOpRelToAbsByteSpans
  | PrimOpUnpackByteSpans
  | PrimOpGtNat
  | PrimOpGeNat
  | PrimOpLtNat
  | PrimOpLeNat
  | PrimOpNeNat
  | PrimOpAddNat
  | PrimOpNeExpr
  deriving (Eq, Show, Generic)

instance Binary PrimOp

sourcePatSpan :: SourcePat_ s st p t -> s
sourcePatSpan = \case
  Nat s _ -> s
  String s _ -> s
  StringPrefix s _ -> s
  ByteArray s _ -> s
  Array s _ -> s
  ArrayPrefix s _ -> s
  Tuple s _ -> s
  Struct s _ -> s
  App s _ _ -> s
  KeyValue s _ _ -> s
  Elements s _ -> s
  All s _ -> s
  Wildcard s -> s
  Variable s _ -> s
  ElementsOfArray s _ -> s
  OrPattern s _ _ -> s
  IfPattern s _ _ _ -> s
  NestedQuery s _ -> s
  Negation s _ -> s
  FactId s _ _ -> s
  TypeSignature s _ _ -> s
  Never s -> s
  FieldSelect s _ _ _ -> s
  Deref s _ -> s
  Enum s _ -> s
  Clause s _ _ _ _ -> s
  Prim s _ _ -> s

-- -----------------------------------------------------------------------------
-- Types

-- | A Glean Type
data Type_ srcspan pref tref
  -- Native types
  = ByteTy
  | NatTy
  | StringTy
  | ArrayTy (Type_ srcspan pref tref)
  | RecordTy [FieldDef_ srcspan pref tref]
  | SumTy [FieldDef_ srcspan pref tref]
  | SetTy (Type_ srcspan pref tref)
  | PredicateTy srcspan pref
  | NamedTy srcspan tref

  -- Derived types. These appear in the source language, but are
  -- translated into the native types above internally. They also
  -- act as hints to the code generators, e.g. maybe translates to
  -- Thrift's optional and enum translates to Thrift's enum.
  | MaybeTy (Type_ srcspan pref tref)
    -- maybe T  => { nothing | just : T }
  | EnumeratedTy [Name]
    -- enum { a | b } => { a : {}, b : {} }
  | BooleanTy
    -- bool => { false : {} | true : {} }

  -- These are used during typechecking only
  | TyVar {-# UNPACK #-}!Int
  | HasTy (Map Name (Type_ srcspan pref tref))
      !(Maybe RecordOrSum) {-# UNPACK #-}!Int
    -- HasTy { field:type .. } R X
    --   Constrains X to be a record or sum type containing at least
    --   the given fields/types. X can only be instantiated
    --   with a type containing a superset of those fields: either
    --   a bigger HasTy or a RecordTy/SumTy.
    --   R is
    --     Just Record -> type must be a record
    --     Just Sum -> type must be a sum type
    --     Nothing -> type can be either a RecordTy or a SumTy
  | HasKey (Type_ srcspan pref tref) {-# UNPACK #-}!Int
    -- HasKey K X
    --   A type variable X that is constrained to be a predicate
    --   type with key K
  | ElementsOf (Type_ srcspan pref tref) {-# UNPACK #-}!Int
    -- ElementsOf T X
    --   A type variable that is either set T or [T]
  deriving (Eq, Show, Functor, Foldable, Generic)

data RecordOrSum = Record | Sum
  deriving (Eq, Show, Generic)

instance Binary RecordOrSum

instance (Binary pref, Binary tref) => Binary (Type_ () pref tref)

instance Bifunctor (Type_ s) where
  bimap f g = \case
    ByteTy -> ByteTy
    NatTy -> NatTy
    StringTy -> StringTy
    ArrayTy ty -> ArrayTy $ bimap f g ty
    RecordTy xs -> RecordTy $ bimap f g <$> xs
    SumTy xs -> SumTy $ bimap f g <$> xs
    SetTy ty -> SetTy $ bimap f g ty
    PredicateTy s pref -> PredicateTy s (f pref)
    NamedTy s tref -> NamedTy s (g tref)
    MaybeTy ty -> MaybeTy (bimap f g ty)
    EnumeratedTy xs -> EnumeratedTy xs
    BooleanTy -> BooleanTy
    TyVar x -> TyVar x
    HasTy m r x -> HasTy (bimap f g <$> m) r x
    HasKey ty x -> HasKey (bimap f g ty) x
    ElementsOf ty x -> ElementsOf (bimap f g ty) x

instance Bifoldable (Type_ s) where
  bifoldr f g r = \case
    ByteTy -> r
    NatTy -> r
    StringTy -> r
    ArrayTy ty -> bifoldr f g r ty
    RecordTy xs -> foldr (flip $ bifoldr f g) r xs
    SumTy xs -> foldr (flip $ bifoldr f g) r xs
    SetTy ty -> bifoldr f g r ty
    PredicateTy _ pref -> f pref r
    NamedTy _ tref -> g tref r
    MaybeTy ty -> bifoldr f g r ty
    EnumeratedTy _ -> r
    BooleanTy -> r
    TyVar _ -> r
    HasTy m _ _ -> foldr (flip $ bifoldr f g) r m
    HasKey ty _ -> bifoldr f g r ty
    ElementsOf ty _ -> bifoldr f g r ty

{- Note [Types]

There are three main instantiations of `Type_` used throughout
Glean:

> type SourceType = Type_ SourceRef SourceRef

These are the types from the parser. Predicates and types may be
unqualified and don't necessarily have versions.

> type Type = Type_ PredicateId TypeId

These are the types after name resolution
(Glean.Schema.Resolve). Predicates and Types are fully qualified and
refer to specific hashes.

> type RTS.Type = Type_ PidRef ExpandedType

(see 'Glean.RTS.Type')

These are the types that relate to a particular DB, and are
constructed when a schema is loaded in `Glean.Database.Schema`.

* Predicates have a particular 'Pid'
* Named types are expanded (but we can still see them in the AST)
* A 'RTS.Type' can be converted into a pure representation ('Rep')
  using 'repType'
-}

-- -----------------------------------------------------------------------------
-- Schemas and definitions

data FieldDef_ st pref tref = FieldDef
  { fieldDefName :: Name
  , fieldDefType :: Type_ st pref tref
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

instance (Binary pref, Binary tref)
  => Binary (FieldDef_ () pref tref)

instance Bifunctor (FieldDef_ st) where
  bimap f g (FieldDef n ty)  = FieldDef n $ bimap f g ty

instance Bifoldable (FieldDef_ st) where
  bifoldMap f g (FieldDef _ ty) = bifoldMap f g ty

tupleField :: Text
tupleField = "tuplefield"

-- | A definition of a named type
data TypeDef_ st pref tref = TypeDef
  { typeDefRef :: tref
  , typeDefType :: Type_ st pref tref
  , typeDefSrcSpan :: st
  }
  deriving Eq

-- | A definition of a predicate
data PredicateDef_ s st pref tref = PredicateDef
  { predicateDefRef :: pref
  , predicateDefKeyType :: Type_ st pref tref
  , predicateDefValueType :: Type_ st pref tref
  , predicateDefDeriving :: DerivingInfo (SourceQuery_ s st pref tref)
  , predicateDefSrcSpan :: s
  }
  deriving Eq

data DerivingDef_ s st pref tref = DerivingDef
  { derivingDefRef :: pref
  , derivingDefDeriveInfo :: DerivingInfo (SourceQuery_ s st pref tref)
  , derivingDefSrcSpan :: s
  }
  deriving Eq

-- | Globally unique identifier for a predicate. This is not the same
-- as a Pid, which is unique only within a particular DB.
data PredicateId = PredicateId
  { predicateIdRef :: PredicateRef  -- ^ e.g. python.Name.1
  , predicateIdHash :: {-# UNPACK #-} !Hash
  } deriving (Ord, Generic, Show)

instance Hashable PredicateId where
  hashWithSalt s (PredicateId _ h) = hashFingerprint s h

instance Eq PredicateId where
  PredicateId _ a == PredicateId _ b = a == b

instance Binary PredicateRef where
  put (PredicateRef a b) = put a >> put b
  get = PredicateRef <$> get <*> get

instance Binary PredicateId
  -- TODO maybe just serialize the hash?

-- | Globally unique identifier for a type.
data TypeId = TypeId
  { typeIdRef :: TypeRef -- ^ e.g. python.Declaration.1
  , typeIdHash :: {-# UNPACK #-} !Hash
  } deriving (Ord, Generic, Show)

instance Hashable TypeId where
  hashWithSalt s (TypeId _ h) = hashFingerprint s h

instance Eq TypeId where
  TypeId _ a == TypeId _ b = a == b

-- Used by query compilation
tempPredicateId :: PredicateId
tempPredicateId = PredicateId (PredicateRef "_tmp_" 0) hash0

instance Binary TypeRef where
  put (TypeRef a b) = put a >> put b
  get = TypeRef <$> get <*> get

instance Binary TypeId
  -- TODO maybe just serialize the hash?

-- | How to derive a predicate
data DerivingInfo q
  = NoDeriving
  | Derive DeriveWhen q
  deriving (Eq, Functor, Foldable, Show, Generic)

instance Binary q => Binary (DerivingInfo q)

data DeriveWhen
  = DeriveOnDemand
    -- ^ derived whenever this predicate is queried. Facts of this
    -- predicate are not expected to be stored in the DB.
  | DerivedAndStored
    -- ^ derived eagerly and stored in the DB. By default, queries for
    -- this predicate will search the DB, just like underived
    -- predicates.
  | DeriveIfEmpty
    -- ^ derived only when the DB has no facts of this predicate.
    -- This is used to add derivations for backwards compatibility
    -- during a schema migration.
  deriving (Eq, Show, Generic)

instance Binary DeriveWhen

-- Source (parsed) abstract syntax

type SourcePat' s st = SourcePat_ s st SourceRef SourceRef
type SourceStatement' s st = SourceStatement_ s st SourceRef SourceRef
type SourceQuery' s st = SourceQuery_ s st SourceRef SourceRef
type SourceDerivingInfo' s = DerivingInfo (SourceQuery' s s)
type SourcePredicateDef' s = PredicateDef_ s s SourceRef SourceRef
type SourceTypeDef' s = TypeDef_ s SourceRef SourceRef
type DerivingDef' s = DerivingDef_ s s SourceRef SourceRef
type SourceType' st = Type_ st SourceRef SourceRef
type Type' st = Type_ st PredicateId TypeId

type SourcePat = SourcePat' SrcSpan SrcSpan
type SourceStatement = SourceStatement' SrcSpan SrcSpan
type SourceQuery = SourceQuery' SrcSpan SrcSpan
type SourceType = Type_ SrcSpan SourceRef SourceRef
type SourceFieldDef = FieldDef_ SrcSpan SourceRef SourceRef
type SourceTypeDef = TypeDef_ SrcSpan SourceRef SourceRef
type SourcePredicateDef = SourcePredicateDef' SrcSpan
type SourceDerivingInfo = SourceDerivingInfo' SrcSpan
type SourceSchemas = SourceSchemas_ SrcSpan
type SourceSchema = SourceSchema_ SrcSpan
type SourceEvolves = SourceEvolves_ SrcSpan
type SourceDecl = SourceDecl_ SrcSpan

type Statement_ p t = SourceStatement_ SrcSpan SrcSpan p t
type Query_ p t = SourceQuery_ SrcSpan SrcSpan p t

data SourceEvolves_ s = SourceEvolves
  { evolvesSpan :: s
  , evolvesNew :: SourceRef
  , evolvesOld :: SourceRef
  }
  deriving (Eq)

-- | A 'schema' declaration
data SourceSchema_ s = SourceSchema
  { schemaName :: SourceRef
  , schemaInherits :: [SourceRef]
  , schemaDecls :: [SourceDecl_ s]
  , schemaSrcSpan :: s
  }
  deriving (Eq)

data SourceSchemas_ s = SourceSchemas
  { srcAngleVersion :: AngleVersion
  , srcSchemas :: [SourceSchema_ s]
  , srcEvolves :: [SourceEvolves_ s]
  }
  deriving (Eq)

data SourceDecl_ s
  = SourceImport SourceRef s
  | SourcePredicate (SourcePredicateDef' s)
  | SourceType (SourceTypeDef' s)
  | SourceDeriving (DerivingDef' s)
  deriving (Eq)

-- Abstract syntax with global Ids

type Type = Type_ SrcSpan PredicateId TypeId
type FieldDef = FieldDef_ SrcSpan PredicateId TypeId
type TypeDef = TypeDef_ SrcSpan PredicateId TypeId
type PredicateDef = PredicateDef_ SrcSpan SrcSpan PredicateId TypeId

type PredicateDef' s st = PredicateDef_ s st PredicateId TypeId
type FieldDef' st = FieldDef_ st PredicateId TypeId

-- -----------------------------------------------------------------------------

-- | Version of the syntax. This is required so that we can change the
-- syntax while still allowing DBs that contain a schema with the old
-- syntax to be understood.
newtype AngleVersion = AngleVersion Int
  deriving (Eq, Ord)
  deriving newtype (Pretty)

-- version 8:
--   - added set constructs
-- version 9:
--   - added dot syntax
-- version 10:
--   - optional versions in `import`, `evolves`, inheritance
-- version 11:
--   - size primop
-- version 12:
--   - remove support for deriving default

latestSupportedAngleVersion :: AngleVersion
latestSupportedAngleVersion = AngleVersion 5

latestAngleVersion :: AngleVersion
latestAngleVersion = AngleVersion 12

-- -----------------------------------------------------------------------------
-- Pretty-printing

instance Display SourceRef where
  display _ (SourceRef name ver) = pretty name <> case ver of
    Nothing -> mempty
    Just ver -> "." <> pretty ver

instance Display PredicateRef where
  display _ = pretty

instance Display TypeRef where
  display _ = pretty

instance Display PredicateId where
  display opts (PredicateId name hash) =
    case predicateStyle opts of
      PredicateWithHash -> pretty name <> "." <> pretty (show hash)
      PredicateWithoutHash -> pretty name

instance Display TypeId where
  display opts (TypeId name hash) =
    case predicateStyle opts of
      PredicateWithHash -> pretty name <> "." <> pretty (show hash)
      PredicateWithoutHash -> pretty name

instance Display (SourceEvolves_ s) where
  display opts (SourceEvolves _ new old) =
    "schema " <> display opts new <> " evolves " <> display opts old

instance (Display pref, Display tref) => Display (Type_ st pref tref) where
  display _ ByteTy = "byte"
  display _ NatTy = "nat"
  display _ StringTy = "string"
  display opts (ArrayTy ty) = "[" <> display opts ty <> "]"
  display opts (RecordTy fields) =
    sep
      [ nest 2 $ vsep $ "{" :  punctuate "," (map (display opts) fields)
      , "}" ]
  display opts (SumTy fields) =
    sep
      [ nest 2 $ vsep $ "{" :  map ((<> " |") . display opts) fields
      , "}" ]
  display opts (SetTy ty) = "set " <> displayAtom opts ty
  display opts (PredicateTy _ p) = display opts p
  display opts (NamedTy _ t) = display opts t
  display opts (MaybeTy t) = "maybe" <+> displayAtom opts t
  display _ (EnumeratedTy names) =
    sep
      [ nest 2 $ vsep $ "enum {" :  map (<> " |") (map pretty names)
      , "}" ]
  display _ BooleanTy = "bool"
  display _ (TyVar n) = "T" <> pretty n
  display opts (HasTy m rec x)
    | Just Sum <- rec =
    sep
      [ nest 2 $ vsep $ "{" :  intersperse "|" (map doField (Map.toList m))
      , "|", "T" <> pretty x, "}" ]
    | otherwise =
    sep
      [ nest 2 $ vsep $ "{" :  map (<> ",") (map doField (Map.toList m))
      , "T" <> pretty x, "}" ]
    where
    doField (n, ty) = pretty n <> " : " <> display opts ty
  display opts (HasKey ty x) =
    "T" <> pretty x <> parens (display opts ty)
  display opts (ElementsOf ty x) =
    "T" <> pretty x <> brackets (display opts ty)
  displayAtom opts t = case t of
    MaybeTy{} -> parens $ display opts t
    EnumeratedTy{} -> parens $ display opts t
    SetTy{} -> parens $ display opts t
    _other -> display opts t

instance (Display pref, Display tref) => Display (FieldDef_ st pref tref) where
  display opts (FieldDef n ty) = pretty n <> " : " <> displayAtom opts ty

instance (Display pref, Display tref) =>
    Display (PredicateDef_ s st pref tref) where
  display opts PredicateDef{..} =
    hang 2 $ sep $
      [ "predicate" <+> display opts predicateDefRef <+> ":"
      , display opts predicateDefKeyType
      ] ++
      (case predicateDefValueType of
         RecordTy [] -> []
         _other -> [ "->" <+> display opts predicateDefValueType ]) ++
      (case predicateDefDeriving of
         Derive DerivedAndStored query -> [ "stored", display opts query ]
         Derive _ query -> [ display opts query ]
         _other -> [])

instance (Display pref, Display tref) => Display (TypeDef_ s pref tref) where
  display opts TypeDef{..} =
    hang 2 $ sep
      [ "type" <+> display opts typeDefRef <+> "="
      , display opts typeDefType
      ]

instance Display SourceSchemas where
  display opts SourceSchemas{..} = vcat $
    ("version:" <+> pretty srcAngleVersion)
    : map (display opts) srcSchemas <> map (display opts) srcEvolves

instance Display SourceSchema where
  display opts SourceSchema{..} = vcat
    [ "schema" <+> display opts schemaName <>
        case schemaInherits of
          [] -> mempty
          _ -> " : " <> hcat (punctuate "," (map (display opts) schemaInherits))
        <> " {"
    , vcat (map (display opts) schemaDecls)
    , "}"
    ]

instance Display SourceDecl where
  display opts (SourceImport name _) = "import " <> display opts name
  display opts (SourcePredicate def) = display opts def
  display opts (SourceType def) = display opts def
  display opts (SourceDeriving DerivingDef{..}) =
    hang 2 $ sep ["derive "
      <> display opts derivingDefRef, display opts derivingDefDeriveInfo]

instance Display q => Display (DerivingInfo q) where
  display _ NoDeriving = mempty
  display opts (Derive DeriveOnDemand q) = display opts q
  display opts (Derive DerivedAndStored q) = "stored" <+> display opts q
  display opts (Derive DeriveIfEmpty q) = "default" <+> display opts q

-- ---------------------------------------------------------------------------
-- Display printing queries

class IsWild pat where
  isWild :: pat -> Bool

instance IsWild (SourcePat_ s st p t) where
  isWild Wildcard{} = True
  isWild _ = False

-- | Types that represent the source location of a term in the AST
class (Pretty a, Pretty (Loc a)) => IsSrcSpan a where
  type Loc a :: *
  startLoc   :: a -> Loc a
  endLoc     :: a -> Loc a
  mkSpan     :: Loc a -> Loc a -> a

-- space encompassing from start of first span to end of second.
spanBetween :: IsSrcSpan a => a -> a-> a
spanBetween x y = mkSpan (startLoc x) (endLoc y)

instance IsSrcSpan SrcSpan where
  type Loc SrcSpan = SrcLoc
  startLoc = spanStart
  endLoc = spanEnd
  mkSpan = SrcSpan

instance Display SrcSpan where
  display opts s =
    display opts (spanStart s)
    <> pretty (" - " :: String)
    <> display opts (spanEnd s)

instance Pretty SrcSpan where
  pretty = displayDefault

instance Display SrcLoc where
  display _ (SrcLoc line col) =
    "line " <> pretty line <> ", column " <> pretty col

instance Pretty SrcLoc where
  pretty = displayDefault

instance (Display p, Display t) => Pretty (SourcePat_ s st p t) where
  pretty = displayDefault

instance (Display p, Display t) => Display (SourcePat_ s st p t) where
  display _ (Nat _ w) = pretty w
  display _ (String _ str) =
    pretty (Text.decodeUtf8 (BL.toStrict (Aeson.encode (Aeson.String str))))
  display opts (StringPrefix s str) =
    display opts (String s str :: SourcePat_ s st p t) <> ".."
  display _ (ByteArray _ b) = pretty (show b)
  display opts (Array _ pats) =
    brackets $ hsep (punctuate "," (map (display opts) pats))
  display opts (ArrayPrefix _ pats) =
    encloseSep "[" ", ..]" "," (map (display opts) $ toList pats)
  display opts (Tuple _ pats) =
    braces $ hsep (punctuate "," (map (display opts) pats))
  display opts (Struct _ fs) = cat [ nest 2 $ cat [ "{", fields fs], "}"]
    where
    fields = sep . punctuate "," . map field
    field (Field name pat) = pretty name <+> "=" <+> display opts pat
  display opts (App _ l pats) =
    display opts l <+> hsep (punctuate " " (map (displayAtom opts) pats))
  display opts (KeyValue _ k v) =
    displayAtom opts k <+> "->" <+> displayAtom opts v
  display opts (Elements _ pat) =
    "elements" <+> parens (display opts pat)
  display opts (All _ query) =
    "all" <+> parens (display opts query)
  display _ (Wildcard _) = "_"
  display _ (Variable _ name) = pretty name
  display opts (ElementsOfArray _ pat) = displayAtom opts pat <> "[..]"
  display opts (OrPattern _ lhs rhs) =
    sep [displayAtom opts lhs <+> "|", displayAtom opts rhs]
  display opts (IfPattern _ cond then_ else_) = sep
    [ nest 2 $ sep ["if", displayAtom opts cond ]
    , nest 2 $ sep ["then", displayAtom opts then_]
    , nest 2 $ sep ["else", displayAtom opts else_]
    ]
  display opts (NestedQuery _ q) = parens $ display opts q
  display opts (Negation _ q) = "!" <> parens (display opts q)
  display _ (FactId _ Nothing n) = "$" <> pretty n
  display _ (FactId _ (Just p) n) = "$" <> pretty p <+> pretty n
  display opts (TypeSignature _ p t) =
    displayAtom opts p <+> ":" <+> display opts t
  display _ (Never _) = "never"
  display opts (Clause _ _ p pat rng) =
    display opts p <> prng <+> displayAtom opts pat
    where prng = case rng of
            SeekOnBase -> "#old"
            SeekOnStacked -> "#new"
            _ -> mempty
  display opts (Prim _ p pats) =
    display opts p <+> hsep (punctuate " " (map (displayAtom opts) pats))
  display opts (FieldSelect _ pat field q) =
    displayAtom opts pat <> "." <> pretty field <>
      case q of
        Sum -> "?"
        Record -> mempty
  display opts (Deref _ pat) =
    displayAtom opts pat <> ".*"
  display _ (Enum _ f) = pretty f

  displayAtom opts pat = case pat of
    App{} -> parens $ display opts pat
    KeyValue{} -> parens $ display opts pat
    OrPattern{} -> parens $ display opts pat
    IfPattern{} -> parens $ display opts pat
    TypeSignature{} -> parens $ display opts pat
    Nat{} -> display opts pat
    String{} -> display opts pat
    StringPrefix{} -> display opts pat
    ByteArray{} -> display opts pat
    Array{} -> display opts pat
    ArrayPrefix{} -> display opts pat
    Tuple{} -> display opts pat
    Struct{} -> display opts pat
    ElementsOfArray{} -> parens $ display opts pat
    Elements{} -> display opts pat
    All{} -> parens $ display opts pat
    Wildcard{} -> display opts pat
    Variable{} -> display opts pat
    NestedQuery{} -> display opts pat
    Negation{} -> display opts pat
    FactId{} -> display opts pat
    Never{} -> display opts pat
    Clause{} -> parens $ display opts pat
    Prim{} -> parens $ display opts pat
    FieldSelect{} -> display opts pat
    Deref{} -> display opts pat
    Enum _ _ -> display opts pat

instance (Display p, Display t) => Display (SourceQuery_ s st p t) where
  display opts (SourceQuery maybeHead stmts _ord) = case stmts of
    [] -> maybe mempty (display opts) maybeHead
    _ -> case maybeHead of
      Just head -> hang 2 (sep (display opts head <+> "where" : pstmts))
      Nothing -> sep pstmts
    where
    pstmts = punctuate ";" (map (display opts) stmts)

instance (Display p, Display t) => Display (SourceStatement_ s st p t) where
  display opts (SourceStatement lhs rhs) = displayStatement opts lhs rhs

displayStatement
  :: (IsWild pat, Display pat)
  => DisplayOpts
  -> pat
  -> pat
  -> Doc ann
displayStatement opts lhs rhs
  | isWild lhs = display opts rhs
  | otherwise = hang 2 $ sep [display opts lhs <+> "=", display opts rhs]

instance Display PrimOp where
  display _ PrimOpToLower = "prim.toLower"
  display _ PrimOpLength = "prim.length"
  display _ PrimOpZip = "prim.zip"
  display _ PrimOpConcat = "prim.concat"
  display _ PrimOpReverse = "prim.reverse"
  display _ PrimOpSize = "prim.size"
  display _ PrimOpRelToAbsByteSpans = "prim.relToAbsByteSpans"
  display _ PrimOpUnpackByteSpans = "prim.unpackByteSpans"
  display _ PrimOpGtNat = "prim.gtNat"
  display _ PrimOpGeNat = "prim.geNat"
  display _ PrimOpLtNat = "prim.ltNat"
  display _ PrimOpLeNat = "prim.leNat"
  display _ PrimOpNeNat = "prim.neNat"
  display _ PrimOpAddNat = "prim.addNat"
  display _ PrimOpNeExpr = "prim.neExpr"

-- -----------------------------------------------------------------------------
-- Removing source locations from the AST

rmLocSchemas :: SourceSchemas_ a -> SourceSchemas_ ()
rmLocSchemas (SourceSchemas version schemas evolves) =
  SourceSchemas version (rmLocSchema <$> schemas) (rmLocEvolves <$> evolves)

rmLocSchema :: SourceSchema_ a -> SourceSchema_ ()
rmLocSchema (SourceSchema name inherits decls _) =
  SourceSchema name inherits (rmLocDecl <$> decls) ()

rmLocEvolves :: SourceEvolves_ a -> SourceEvolves_ ()
rmLocEvolves (SourceEvolves _ a b) = SourceEvolves () a b

rmLocDecl :: SourceDecl_ s -> SourceDecl_ ()
rmLocDecl = \case
  SourceImport name _ -> SourceImport name ()
  SourcePredicate pred -> SourcePredicate $ rmLocPredDef pred
  SourceType typeDef -> SourceType $ rmLocTypeDef typeDef
  SourceDeriving DerivingDef{..} -> SourceDeriving $ DerivingDef
    derivingDefRef (rmLocQuery <$> derivingDefDeriveInfo) ()

rmLocPredDef :: PredicateDef_ s st p t -> PredicateDef_ () () p t
rmLocPredDef PredicateDef{..} = PredicateDef {
  predicateDefRef = predicateDefRef,
  predicateDefKeyType = rmLocType predicateDefKeyType,
  predicateDefValueType = rmLocType predicateDefValueType,
  predicateDefDeriving = rmLocQuery <$> predicateDefDeriving,
  predicateDefSrcSpan = ()
}

rmLocQuery :: SourceQuery_ s st p t -> SourceQuery_ () () p t
rmLocQuery (SourceQuery mhead stmts ord) =
  SourceQuery (rmLocPat <$> mhead) (rmLocStatement <$> stmts) ord

rmLocStatement :: SourceStatement_ s st p t -> SourceStatement_ () () p t
rmLocStatement (SourceStatement x y) =
  SourceStatement (rmLocPat x) (rmLocPat y)

rmLocTypeDef :: TypeDef_ s p t -> TypeDef_ () p t
rmLocTypeDef (TypeDef tref ty _) = TypeDef tref (rmLocType ty) ()

rmLocType :: Type_ st p t -> Type_ () p t
rmLocType = \case
  ByteTy -> ByteTy
  NatTy -> NatTy
  StringTy -> StringTy
  ArrayTy ty -> ArrayTy $ rmLocType ty
  RecordTy xs -> RecordTy $ rmLocFieldDef <$> xs
  SumTy xs -> SumTy $ rmLocFieldDef <$> xs
  SetTy ty -> SetTy $ rmLocType ty
  PredicateTy _ pref -> PredicateTy () pref
  NamedTy _ tref -> NamedTy () tref
  MaybeTy ty -> MaybeTy $ rmLocType ty
  EnumeratedTy xs -> EnumeratedTy xs
  BooleanTy -> BooleanTy
  TyVar x -> TyVar x
  HasTy m r x -> HasTy (rmLocType <$> m) r x
  HasKey ty x -> HasKey (rmLocType ty) x
  ElementsOf ty x -> ElementsOf (rmLocType ty) x

rmLocFieldDef :: FieldDef_ s p t -> FieldDef_ () p t
rmLocFieldDef (FieldDef n ty) = FieldDef n $ rmLocType ty

rmLocPat :: SourcePat_ s st p t -> SourcePat_ () () p t
rmLocPat = \case
  Nat _ x -> Nat () x
  String _ x -> String () x
  StringPrefix _ x -> StringPrefix () x
  ByteArray _ x -> ByteArray () x
  Array _ xs -> Array () (rmLocPat <$> xs)
  ArrayPrefix _ xs -> ArrayPrefix () (rmLocPat <$> xs)
  Tuple _ xs -> Tuple () (rmLocPat <$> xs)
  Struct _ xs -> Struct () (rmLocField <$> xs)
  App _ x xs -> App () (rmLocPat x) (rmLocPat <$> xs)
  KeyValue _ x y -> KeyValue () (rmLocPat x) (rmLocPat y)
  Elements _ pat -> Elements () (rmLocPat pat)
  All _ query -> All () (rmLocPat query)
  Wildcard _ -> Wildcard ()
  Never _ -> Never ()
  Variable _ v -> Variable () v
  ElementsOfArray _ x -> ElementsOfArray () (rmLocPat x)
  OrPattern _ x y -> OrPattern () (rmLocPat x) (rmLocPat y)
  IfPattern _ x y z -> IfPattern () (rmLocPat x) (rmLocPat y) (rmLocPat z)
  Negation _ x -> Negation () (rmLocPat x)
  NestedQuery _ query -> NestedQuery () $ rmLocQuery query
  FactId _ x y -> FactId () x y
  TypeSignature _ x t -> TypeSignature () (rmLocPat x) (rmLocType t)
  Clause _ _ x y rng -> Clause () () x (rmLocPat y) rng
  Prim _ p ps -> Prim () p (rmLocPat <$> ps)
  FieldSelect _ pat field q -> FieldSelect () (rmLocPat pat) field q
  Deref _ pat -> Deref () (rmLocPat pat)
  Enum _ f -> Enum () f

rmLocField :: Field s st p t -> Field () () p t
rmLocField (Field name pat) =
  Field name (rmLocPat pat)

-- Remove spans from type refs only. Useful when
-- the two span types are different (s != st), e.g., s = SrcSpan, st = ()
rmTypeLocPredDef :: PredicateDef_ s st p t -> PredicateDef_ s () p t
rmTypeLocPredDef pred = pred
    { predicateDefKeyType = rmLocType $ predicateDefKeyType pred
    , predicateDefValueType = rmLocType $ predicateDefValueType pred
    , predicateDefDeriving = rmTypeLocQuery <$> predicateDefDeriving pred}

rmTypeLocQuery :: SourceQuery_ s st p t -> SourceQuery_ s () p t
rmTypeLocQuery (SourceQuery mhead stmts ord) =
  SourceQuery (rmTypeLocPat <$> mhead) (rmTypeLocStatement <$> stmts) ord

rmTypeLocStatement :: SourceStatement_ s st p t -> SourceStatement_ s () p t
rmTypeLocStatement (SourceStatement x y) =
  SourceStatement (rmTypeLocPat x) (rmTypeLocPat y)

rmTypeLocPat :: SourcePat_ s st p t -> SourcePat_ s () p t
rmTypeLocPat pat = case pat of
  Nat s x -> Nat s x
  String s x -> String s x
  StringPrefix s x -> StringPrefix s x
  ByteArray s x -> ByteArray s x
  TypeSignature s x t -> TypeSignature s (rmTypeLocPat x) (rmLocType t)
  Array s xs -> Array s (rmTypeLocPat <$> xs)
  ArrayPrefix s xs -> ArrayPrefix s (rmTypeLocPat <$> xs)
  Tuple s xs -> Tuple s (rmTypeLocPat <$> xs)
  Struct s xs -> Struct s (rmTypeLocField <$> xs)
  App s x xs -> App s (rmTypeLocPat x) (rmTypeLocPat <$> xs)
  KeyValue s x y -> KeyValue s (rmTypeLocPat x) (rmTypeLocPat y)
  Elements s pat -> Elements s (rmTypeLocPat pat)
  All s query -> All s (rmTypeLocPat query)
  Wildcard s -> Wildcard s
  Never s -> Never s
  Variable s v -> Variable s v
  ElementsOfArray s x -> ElementsOfArray s (rmTypeLocPat x)
  OrPattern s x y -> OrPattern s (rmTypeLocPat x) (rmTypeLocPat y)
  IfPattern s x y z ->
    IfPattern s (rmTypeLocPat x) (rmTypeLocPat y) (rmTypeLocPat z)
  Negation s x -> Negation s (rmTypeLocPat x)
  NestedQuery s query -> NestedQuery s $ rmTypeLocQuery query
  FactId s x y -> FactId s x y
  Clause s ss x y rng -> Clause s ss x (rmTypeLocPat y) rng
  Prim s p ps -> Prim s p (rmTypeLocPat <$> ps)
  FieldSelect s pat field q -> FieldSelect s (rmTypeLocPat pat) field q
  Deref s pat -> Deref s (rmTypeLocPat pat)
  Enum s f -> Enum s f

rmTypeLocField :: Field s st p t -> Field s () p t
rmTypeLocField (Field name pat) =
  Field name (rmTypeLocPat pat)

-- -----------------------------------------------------------------------------
-- Describing the kind of pattern

class Describe a where
  describe :: a -> Doc ann

instance Describe (SourcePat_ s st p t) where
  describe = \case
    Nat {} -> "a nat"
    String {} -> "a string"
    StringPrefix {} -> "a string prefix"
    ByteArray {} -> "a byte array"
    Array {} -> "an array"
    ArrayPrefix {} -> "an array prefix"
    Tuple {} -> "a tuple"
    Struct {} -> "a struct"
    App {} -> "an application"
    KeyValue {} -> "a key value pattern"
    All {} -> "a set with all the results of its argument"
    Wildcard {} -> "a wildcard pattern"
    Never {} -> "a never pattern"
    Variable {} -> "a variable"
    ElementsOfArray {} -> "an element of an array"
    Elements {} -> "an element of a set"
    OrPattern {} -> "an or pattern"
    IfPattern {} -> "an if pattern"
    Negation {} -> "a negation"
    NestedQuery {} -> "a nested query"
    FactId {} -> "a fact id"
    TypeSignature {} -> "a type signature"
    Clause {} -> "a clause"
    Prim {} -> "a primitive function"
    FieldSelect{} -> "a record field"
    Deref{} -> "a dereference"
    Enum{} -> "an enum value"

instance Describe SrcSpan where
  describe _ = "a source span"

instance Describe SrcLoc where
  describe _ = "a source location"
