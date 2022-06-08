{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types representing a source-level schema
module Glean.Angle.Types
  ( -- * Names and references
    Name
  , FieldName
  , Version
  , PredicateRef(..)
  , TypeRef(..)
  , PredicateId(..)
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

  -- * Types
  , Type_(..)

  -- * Queries
  , SourceQuery_(..)
  , SourceStatement_(..)
  , SourcePat_(..)
  , PrimOp(..)

  -- * Schemas and definitions
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
  , SourceSchemas_(..)
  , SourceEvolves_(..)
  , SourceSchema_(..)
  , SourceDecl_(..)

  -- * Versions of the Angle syntax
  , AngleVersion
  , latestAngleVersion
  , latestSupportedAngleVersion

  -- * Pretty printing
  , prettyStatement
  ) where

import qualified Data.Aeson as Aeson
import Data.Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc
import Data.Bifunctor
import Data.Bifoldable
import GHC.Generics

import Glean.Angle.Hash
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

data SourceQuery_ s p t = SourceQuery
  { srcQueryHead :: Maybe (SourcePat_ s p t)
  , srcQueryStmts :: [SourceStatement_ s p t]
  }
  deriving (Eq, Show, Generic)

instance (Binary p, Binary t) => Binary (SourceQuery_ () p t)

instance Bifunctor (SourceQuery_ s) where
  bimap f g (SourceQuery h s) =
    SourceQuery (fmap (bimap f g) h) (fmap (bimap f g) s)

instance Bifoldable (SourceQuery_ s) where
  bifoldMap f g (SourceQuery h s) =
    foldMap (bifoldMap f g) h <> foldMap (bifoldMap f g) s

data SourceStatement_ s p t =
  SourceStatement (SourcePat_ s p t) (SourcePat_ s p t)
  deriving (Eq, Show, Generic)

instance (Binary p, Binary t) => Binary (SourceStatement_ () p t)

instance Bifunctor (SourceStatement_ s) where
  bimap f g (SourceStatement l r) = SourceStatement (bimap f g l) (bimap f g r)

instance Bifoldable (SourceStatement_ s) where
  bifoldMap f g (SourceStatement l r) = bifoldMap f g l <> bifoldMap f g r

data SourcePat_ s p t
  = Nat s Word64
  | String s Text
  | StringPrefix s Text
  | ByteArray s ByteString
    -- ^ There's no concrete syntax for this (yet), but it can be used
    -- via the DSL.
  | Array s [SourcePat_ s p t]
  | ArrayPrefix s (NonEmpty (SourcePat_ s p t))
  | Tuple s [SourcePat_ s p t]
  | Struct s [Field s p t]
  | App s (SourcePat_ s p t) [SourcePat_ s p t]
  | KeyValue s (SourcePat_ s p t) (SourcePat_ s p t)
  | Wildcard s
  | Variable s Name
  | ElementsOfArray s (SourcePat_ s p t)
  | OrPattern s (SourcePat_ s p t) (SourcePat_ s p t)
  | NestedQuery s (SourceQuery_ s p t)
  | Negation s (SourcePat_ s p t)
  | FactId s (Maybe Text) Word64
  | TypeSignature s (SourcePat_ s p t) (Type_ p t)
  | Never s
  | IfPattern
      { span :: s
      , cond :: SourcePat_ s p t
      , then_ :: SourcePat_ s p t
      , else_ :: SourcePat_ s p t
      }

  -- The following forms are introduced by the resolver, and replace
  -- the Variable and App forms produced by the parser.
  | Clause s p (SourcePat_ s p t)
  | Prim s PrimOp [SourcePat_ s p t]
 deriving (Eq, Show, Generic)

instance (Binary p, Binary t) => Binary (SourcePat_ () p t)

instance Bifunctor (SourcePat_ s) where
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
    Wildcard s -> Wildcard s
    Variable s n -> Variable s n
    ElementsOfArray s pat -> ElementsOfArray s (bimap f g pat)
    OrPattern s l r -> OrPattern s (bimap f g l) (bimap f g r)
    NestedQuery s (SourceQuery head stmts) ->
      NestedQuery s (SourceQuery (fmap (bimap f g) head)
        (fmap (bimap f g) stmts))
    Negation s pat -> Negation s (bimap f g pat)
    FactId s n w -> FactId s n w
    TypeSignature s pat ty -> TypeSignature s (bimap f g pat) (bimap f g ty)
    Never s -> Never s
    IfPattern s a b c -> IfPattern s (bimap f g a) (bimap f g b) (bimap f g c)
    Clause s p pat -> Clause s (f p) (bimap f g pat)
    Prim s p pats -> Prim s p (fmap (bimap f g) pats)

instance Bifoldable (SourcePat_ s) where
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
    Wildcard{} -> mempty
    Variable{} -> mempty
    ElementsOfArray _ pat -> bifoldMap f g pat
    OrPattern _ l r -> bifoldMap f g l <> bifoldMap f g r
    NestedQuery _ (SourceQuery head stmts) ->
      foldMap (bifoldMap f g) head <> foldMap (bifoldMap f g) stmts
    Negation _ pat -> bifoldMap f g pat
    FactId{} -> mempty
    TypeSignature _ pat ty -> bifoldMap f g pat <> bifoldMap f g ty
    Never{} -> mempty
    IfPattern _ a b c -> bifoldMap f g a <> bifoldMap f g b <> bifoldMap f g c
    Clause _ p pat -> f p <> bifoldMap f g pat
    Prim _ _ pats -> foldMap (bifoldMap f g) pats

data Field s p t = Field FieldName (SourcePat_ s p t)
  deriving (Eq, Show, Generic)

instance (Binary p, Binary t) => Binary (Field () p t)

instance Bifunctor (Field s) where
  bimap f g (Field n pat) = Field n (bimap f g pat)

instance Bifoldable (Field s) where
  bifoldMap f g (Field _ pat) = bifoldMap f g pat

-- | Primitive operations
data PrimOp
  = PrimOpToLower
  | PrimOpLength
  | PrimOpRelToAbsByteSpans
  | PrimOpGtNat
  | PrimOpGeNat
  | PrimOpLtNat
  | PrimOpLeNat
  | PrimOpNeNat
  | PrimOpAddNat
  | PrimOpNeExpr
  deriving (Eq, Show, Generic)

instance Binary PrimOp

sourcePatSpan :: SourcePat_ s p t -> s
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
  Clause s _ _ -> s
  Prim s _ _ -> s

-- -----------------------------------------------------------------------------
-- Types

-- | A Glean Type
data Type_ pref tref
  -- Native types
  = ByteTy
  | NatTy
  | StringTy
  | ArrayTy (Type_ pref tref)
  | RecordTy [FieldDef_ pref tref]
  | SumTy [FieldDef_ pref tref]
  | PredicateTy pref
  | NamedTy tref

  -- Derived types. These appear in the source language, but are
  -- translated into the native types above internally. They also
  -- act as hints to the code generators, e.g. maybe translates to
  -- Thrift's optional and enum translates to Thrift's enum.
  | MaybeTy (Type_ pref tref)
    -- maybe T  => { nothing | just : T }
  | EnumeratedTy [Name]
    -- enum { a | b } => { a : {}, b : {} }
  | BooleanTy
    -- bool => { false : {} | true : {} }
  deriving (Eq, Show, Functor, Foldable, Generic)

instance (Binary pref, Binary tref) => Binary (Type_ pref tref)

instance Bifunctor Type_ where
  bimap f g = \case
    ByteTy -> ByteTy
    NatTy -> NatTy
    StringTy -> StringTy
    ArrayTy ty -> ArrayTy $ bimap f g ty
    RecordTy xs -> RecordTy $ bimap f g <$> xs
    SumTy xs -> SumTy $ bimap f g <$> xs
    PredicateTy pref -> PredicateTy (f pref)
    NamedTy tref -> NamedTy (g tref)
    MaybeTy ty -> MaybeTy (bimap f g ty)
    EnumeratedTy xs -> EnumeratedTy xs
    BooleanTy -> BooleanTy

instance Bifoldable Type_ where
  bifoldMap f g = \case
    ByteTy -> mempty
    NatTy -> mempty
    StringTy -> mempty
    ArrayTy ty -> bifoldMap f g ty
    RecordTy xs -> foldMap (bifoldMap f g) xs
    SumTy xs -> foldMap (bifoldMap f g) xs
    PredicateTy pref -> f pref
    NamedTy tref -> g tref
    MaybeTy ty -> bifoldMap f g ty
    EnumeratedTy _ -> mempty
    BooleanTy -> mempty

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

data FieldDef_ pref tref = FieldDef
  { fieldDefName :: Name
  , fieldDefType :: Type_ pref tref
  }
  deriving (Eq, Show, Functor, Foldable, Generic)

instance (Binary pref, Binary tref) => Binary (FieldDef_ pref tref)

instance Bifunctor FieldDef_ where
  bimap f g (FieldDef n ty)  = FieldDef n $ bimap f g ty

instance Bifoldable FieldDef_ where
  bifoldMap f g (FieldDef _ ty) = bifoldMap f g ty

-- | A definition of a named type
data TypeDef_ pref tref = TypeDef
  { typeDefRef :: tref
  , typeDefType :: Type_ pref tref
  }
  deriving Eq

-- | A definition of a predicate
data PredicateDef_ s pref tref = PredicateDef
  { predicateDefRef :: pref
  , predicateDefKeyType :: Type_ pref tref
  , predicateDefValueType :: Type_ pref tref
  , predicateDefDeriving :: DerivingInfo (SourceQuery_ s pref tref)
  }
  deriving Eq

-- | Globally unique identifier for a predicate. This is not the same
-- as a Pid, which is unique only within a particular DB.
data PredicateId = PredicateId
  { predicateIdName :: Text  -- ^ e.g. python.Name
  , predicateIdHash :: Hash
  } deriving (Eq, Ord, Generic, Hashable, Show)

instance Binary PredicateId
  -- TODO maybe just serialize the hash?

-- | Globally unique identifier for a type.
data TypeId = TypeId
  { typeIdName :: Text -- ^ e.g. python.Declaration
  , typeIdHash :: Hash
  } deriving (Eq, Ord, Generic, Hashable, Show)

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

type SourcePat' s = SourcePat_ s SourceRef SourceRef
type SourceStatement' s = SourceStatement_ s SourceRef SourceRef
type SourceQuery' s = SourceQuery_ s SourceRef SourceRef
type SourceDerivingInfo' s = DerivingInfo (SourceQuery' s)
type SourcePredicateDef' s = PredicateDef_ s SourceRef SourceRef

type SourcePat = SourcePat' SrcSpan
type SourceStatement = SourceStatement' SrcSpan
type SourceQuery = SourceQuery' SrcSpan
type SourceType = Type_ SourceRef SourceRef
type SourceFieldDef = FieldDef_ SourceRef SourceRef
type SourceTypeDef = TypeDef_ SourceRef SourceRef
type SourcePredicateDef = SourcePredicateDef' SrcSpan
type SourceDerivingInfo = SourceDerivingInfo' SrcSpan
type SourceSchemas = SourceSchemas_ SrcSpan
type SourceSchema = SourceSchema_ SrcSpan
type SourceEvolves = SourceEvolves_ SrcSpan
type SourceDecl = SourceDecl_ SrcSpan

type Statement_ p t = SourceStatement_ SrcSpan p t
type Query_ p t = SourceQuery_ SrcSpan p t

data SourceEvolves_ s = SourceEvolves
  { evolvesSpan :: s
  , evolvesNew :: Name
  , evolvesOld :: Name
  }
  deriving (Eq)

-- | A 'schema' declaration
data SourceSchema_ s = SourceSchema
  { schemaName :: Name
  , schemaInherits :: [Name]
  , schemaDecls :: [SourceDecl_ s]
  }
  deriving (Eq)

data SourceSchemas_ s = SourceSchemas
  { srcAngleVersion :: AngleVersion
  , srcSchemas :: [SourceSchema_ s]
  , srcEvolves :: [SourceEvolves_ s]
  }
  deriving (Eq)

data SourceDecl_ s
  = SourceImport Name
  | SourcePredicate (SourcePredicateDef' s)
  | SourceType SourceTypeDef
  | SourceDeriving SourceRef (SourceDerivingInfo' s)
  deriving (Eq)

-- Abstract syntax with global Ids

type Type = Type_ PredicateId TypeId
type FieldDef = FieldDef_ PredicateId TypeId
type TypeDef = TypeDef_ PredicateId TypeId
type PredicateDef = PredicateDef_ SrcSpan PredicateId TypeId

-- -----------------------------------------------------------------------------

-- | Version of the syntax. This is required so that we can change the
-- syntax while still allowing DBs that contain a schema with the old
-- syntax to be understood.
type AngleVersion = Int

latestSupportedAngleVersion :: AngleVersion
latestSupportedAngleVersion = 5

latestAngleVersion :: AngleVersion
latestAngleVersion = 5

-- -----------------------------------------------------------------------------
-- Pretty-printing

instance Pretty SourceRef where
  pretty (SourceRef name ver) = pretty name <> case ver of
    Nothing -> mempty
    Just ver -> "." <> pretty ver

instance Pretty PredicateId where
  pretty (PredicateId name hash) = pretty name <> "." <> pretty (show hash)

instance Pretty TypeId where
  pretty (TypeId name hash) = pretty name <> "." <> pretty (show hash)

instance Pretty (SourceEvolves_ s) where
  pretty (SourceEvolves _ new old) =
    "schema " <> pretty new <> " evolves " <> pretty old

instance (Pretty pref, Pretty tref) => Pretty (Type_ pref tref) where
  pretty ByteTy = "byte"
  pretty NatTy = "nat"
  pretty StringTy = "string"
  pretty (ArrayTy ty) = "[" <> pretty ty <> "]"
  pretty (RecordTy fields) =
    sep
      [ nest 2 $ vsep $ "{" :  punctuate "," (map pretty fields)
      , "}" ]
  pretty (SumTy fields) =
    sep
      [ nest 2 $ vsep $ "{" :  map (<> " |") (map pretty fields)
      , "}" ]
  pretty (PredicateTy p) = pretty p
  pretty (NamedTy t) = pretty t
  pretty (MaybeTy t) = "maybe" <+> pretty t
  pretty (EnumeratedTy names) =
    sep
      [ nest 2 $ vsep $ "enum {" :  map (<> " |") (map pretty names)
      , "}" ]
  pretty BooleanTy = "bool"

instance (Pretty pref, Pretty tref) => Pretty (FieldDef_ pref tref) where
  pretty (FieldDef n ty) = pretty n <> " : " <> pretty ty

instance (Pretty pref, Pretty tref) =>
    Pretty (PredicateDef_ s pref tref) where
  pretty PredicateDef{..} =
    hang 2 $ sep $
      [ "predicate" <+> pretty predicateDefRef <+> ":"
      , pretty predicateDefKeyType
      ] ++
      (case predicateDefValueType of
         RecordTy [] -> []
         _other -> [ "->" <+> pretty predicateDefValueType ]) ++
      (case predicateDefDeriving of
         Derive DerivedAndStored query -> [ "stored", pretty query ]
         Derive _ query -> [ pretty query ]
         _other -> [])

instance (Pretty pref, Pretty tref) => Pretty (TypeDef_ pref tref) where
  pretty TypeDef{..} =
    hang 2 $ sep
      [ "type" <+> pretty typeDefRef <+> "="
      , pretty typeDefType
      ]

instance Pretty SourceSchemas where
  pretty SourceSchemas{..} = vcat $
    ("version:" <+> pretty srcAngleVersion)
    : map pretty srcSchemas <> map pretty srcEvolves

instance Pretty SourceSchema where
  pretty SourceSchema{..} = vcat
    [ "schema" <+> pretty schemaName <>
        case schemaInherits of
          [] -> mempty
          _ -> " : " <> hcat (punctuate "," (map pretty schemaInherits))
        <> " {"
    , vcat (map pretty schemaDecls)
    , "}"
    ]

instance Pretty SourceDecl where
  pretty (SourceImport name) = "import " <> pretty name
  pretty (SourcePredicate def) = pretty def
  pretty (SourceType def) = pretty def
  pretty (SourceDeriving ref der) =
    hang 2 $ sep ["derive " <> pretty ref, pretty der]

instance Pretty q => Pretty (DerivingInfo q) where
  pretty NoDeriving = mempty
  pretty (Derive DeriveOnDemand q) = pretty q
  pretty (Derive DerivedAndStored q) = "stored" <+> pretty q
  pretty (Derive DeriveIfEmpty q) = "default" <+> pretty q

-- ---------------------------------------------------------------------------
-- Pretty printing queries

class IsWild pat where
  isWild :: pat -> Bool

instance IsWild (SourcePat_ s p t) where
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

instance Pretty SrcSpan where
  pretty s =
    pretty (spanStart s)
    <> pretty (" - " :: String)
    <> pretty (spanEnd s)

instance Pretty SrcLoc where
  pretty (SrcLoc line col) =
    "line " <> pretty  line <> ", column " <> pretty col

instance (Pretty p, Pretty t) => Pretty (SourcePat_ s p t) where
  pretty (Nat _ w) = pretty w
  pretty (String _ str) =
    pretty (Text.decodeUtf8 (BL.toStrict (Aeson.encode (Aeson.String str))))
  pretty (StringPrefix s str) =
    pretty (String s str :: SourcePat_ s p t) <> ".."
  pretty (ByteArray _ b) = pretty (show b)
  pretty (Array _ pats) = brackets $ hsep (punctuate "," (map pretty pats))
  pretty (ArrayPrefix _ pats) =
    encloseSep "[" "..]" "," (map pretty $ toList pats)
  pretty (Tuple _ pats) = braces $ hsep (punctuate "," (map pretty pats))
  pretty (Struct _ fs) = cat [ nest 2 $ cat [ "{", fields fs], "}"]
    where
    fields = sep . punctuate "," . map field
    field (Field name pat) = pretty name <+> "=" <+> pretty pat
  pretty (App _ l pats) = pretty l <+> hsep (punctuate " " (map prettyArg pats))
  pretty (KeyValue _ k v) = prettyArg k <+> "->" <+> prettyArg v
  pretty (Wildcard _) = "_"
  pretty (Variable _ name) = pretty name
  pretty (ElementsOfArray _ pat) = pretty pat <> "[..]"
  pretty (OrPattern _ lhs rhs) = sep [prettyArg lhs <+> "|", prettyArg rhs]
  pretty (IfPattern _ cond then_ else_) = sep
    [ nest 2 $ sep ["if", prettyArg cond ]
    , nest 2 $ sep ["then", prettyArg then_]
    , nest 2 $ sep ["else", prettyArg else_]
    ]
  pretty (NestedQuery _ q) = parens $ pretty q
  pretty (Negation _ q) = "!" <> parens (pretty q)
  pretty (FactId _ Nothing n) = "$" <> pretty n
  pretty (FactId _ (Just p) n) = "$" <> pretty p <+> pretty n
  pretty (TypeSignature _ p t) = prettyArg p <+> ":" <+> pretty t
  pretty (Never _) = "never"
  pretty (Clause _ p pat) = pretty p <+> prettyArg pat
  pretty (Prim _ p pats) =
    pretty p <+> hsep (punctuate " " (map prettyArg pats))

instance (Pretty p, Pretty t) => Pretty (SourceQuery_ s p t) where
  pretty (SourceQuery maybeHead stmts) = case stmts of
    [] -> pretty maybeHead
    _ -> case maybeHead of
      Just head -> hang 2 (sep (pretty head <+> "where" : pstmts))
      Nothing -> sep pstmts
    where
    pstmts = punctuate ";" (map pretty stmts)

instance (Pretty p, Pretty t) => Pretty (SourceStatement_ s p t) where
  pretty (SourceStatement lhs rhs) = prettyStatement lhs rhs

prettyStatement :: Pretty pat => pat -> pat -> Doc ann
prettyStatement lhs rhs = hang 2 $ sep [pretty lhs <+> "=", pretty rhs]

prettyArg :: (Pretty p, Pretty t) => SourcePat_ s p t -> Doc ann
prettyArg pat = case pat of
  App{} -> parens $ pretty pat
  KeyValue{} -> parens $ pretty pat
  OrPattern{} -> parens $ pretty pat
  IfPattern{} -> parens $ pretty pat
  TypeSignature{} -> parens $ pretty pat
  Nat{} -> pretty pat
  String{} -> pretty pat
  StringPrefix{} -> pretty pat
  ByteArray{} -> pretty pat
  Array{} -> pretty pat
  ArrayPrefix{} -> pretty pat
  Tuple{} -> pretty pat
  Struct{} -> pretty pat
  ElementsOfArray{} -> pretty pat
  Wildcard{} -> pretty pat
  Variable{} -> pretty pat
  NestedQuery{} -> pretty pat
  Negation{} -> pretty pat
  FactId{} -> pretty pat
  Never{} -> pretty pat
  Clause{} -> parens $ pretty pat
  Prim{} -> parens $ pretty pat

instance Pretty PrimOp where
  pretty PrimOpToLower = "prim.toLower"
  pretty PrimOpLength = "prim.length"
  pretty PrimOpRelToAbsByteSpans = "prim.relToAbsByteSpans"
  pretty PrimOpGtNat = "prim.gtNat"
  pretty PrimOpGeNat = "prim.geNat"
  pretty PrimOpLtNat = "prim.ltNat"
  pretty PrimOpLeNat = "prim.leNat"
  pretty PrimOpNeNat = "prim.neNat"
  pretty PrimOpAddNat = "prim.addNat"
  pretty PrimOpNeExpr = "prim.neExpr"

-- -----------------------------------------------------------------------------
-- Removing source locations from the AST

rmLocSchemas :: SourceSchemas_ a -> SourceSchemas_ ()
rmLocSchemas (SourceSchemas version schemas evolves) =
  SourceSchemas version (rmLocSchema <$> schemas) (rmLocEvolves <$> evolves)

rmLocSchema :: SourceSchema_ a -> SourceSchema_ ()
rmLocSchema (SourceSchema name inherits decls) =
  SourceSchema name inherits $ rmLocDecl <$> decls

rmLocEvolves :: SourceEvolves_ a -> SourceEvolves_ ()
rmLocEvolves (SourceEvolves _ a b) = SourceEvolves () a b

rmLocDecl :: SourceDecl_ a -> SourceDecl_ ()
rmLocDecl = \case
  SourceImport name -> SourceImport name
  SourcePredicate pred -> SourcePredicate $ pred
    { predicateDefDeriving = rmLocQuery <$> predicateDefDeriving pred }
  SourceType typeDef -> SourceType typeDef
  SourceDeriving ref deriv -> SourceDeriving ref $ rmLocQuery <$> deriv

rmLocQuery :: SourceQuery_ s p t -> SourceQuery_ () p t
rmLocQuery (SourceQuery mhead stmts) =
  SourceQuery (rmLocPat <$> mhead) (rmLocStatement <$> stmts)

rmLocStatement :: SourceStatement_ s p t -> SourceStatement_ () p t
rmLocStatement (SourceStatement x y) =
  SourceStatement (rmLocPat x) (rmLocPat y)

rmLocPat :: SourcePat_ s p t -> SourcePat_ () p t
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
  Wildcard _ -> Wildcard ()
  Never _ -> Never ()
  Variable _ v -> Variable () v
  ElementsOfArray _ x -> ElementsOfArray () (rmLocPat x)
  OrPattern _ x y -> OrPattern () (rmLocPat x) (rmLocPat y)
  IfPattern _ x y z -> IfPattern () (rmLocPat x) (rmLocPat y) (rmLocPat z)
  Negation _ x -> Negation () (rmLocPat x)
  NestedQuery _ query -> NestedQuery () $ rmLocQuery query
  FactId _ x y -> FactId () x y
  TypeSignature _ x t -> TypeSignature () (rmLocPat x) t
  Clause _ x y -> Clause () x (rmLocPat y)
  Prim _ p ps -> Prim () p (rmLocPat <$> ps)

rmLocField :: Field s p t -> Field () p t
rmLocField (Field name pat) =
  Field name (rmLocPat pat)
