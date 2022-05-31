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

  -- * Source locations
  , IsSrcSpan(..)
  , SrcSpan(..)
  , SrcLoc(..)

  -- * Types
  , Type_(..)

  -- * Queries
  , SourceQuery_(..)
  , SourceStatement_(..)
  , SourcePat_(..)

  -- * Schemas and definitions
  , TypeDef_(..)
  , FieldDef_(..)
  , PredicateDef_(..)
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
  , checkReservedWordsInFieldNames
  , caseRestriction
  , varBinding

  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Hashable
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc
import Data.Word
import Data.Bifunctor
import Data.Bifoldable
import GHC.Generics

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

data SourceQuery_ head stmt = SourceQuery
  { srcQueryHead :: Maybe head
  , srcQueryStmts :: [stmt]
  }
  deriving (Eq, Show)

data SourceStatement_ pat = SourceStatement pat pat
  deriving (Eq, Show)

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
  | NestedQuery s
      (SourceQuery_ (SourcePat_ s p t) (SourceStatement_ (SourcePat_ s p t)))
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
 deriving (Eq, Show)

data Field s p t = Field FieldName (SourcePat_ s p t)
  deriving (Eq, Show)

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
  deriving (Eq, Show, Functor, Foldable)

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

> type Type = Type_ PredicateRef TypeRef

These are the types after name resolution
(Glean.Schema.Resolve). Predicates and Types are fully qualified and
refer to specific versions

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
  deriving (Eq, Show, Functor, Foldable)

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
data PredicateDef_ pref tref query = PredicateDef
  { predicateDefRef :: pref
  , predicateDefKeyType :: Type_ pref tref
  , predicateDefValueType :: Type_ pref tref
  , predicateDefDeriving :: DerivingInfo query
  }
  deriving (Eq, Functor)

-- | How to derive a predicate
data DerivingInfo q
  = NoDeriving
  | Derive DeriveWhen q
  deriving (Eq, Functor)

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
  deriving Eq

type SourcePat' s = SourcePat_ s SourceRef SourceRef
type SourceStatement' s = SourceStatement_ (SourcePat' s)
type SourceQuery' s = SourceQuery_ (SourcePat' s) (SourceStatement' s)
type SourceDerivingInfo' s = DerivingInfo (SourceQuery' s)
type SourcePredicateDef' s = PredicateDef_ SourceRef SourceRef (SourceQuery' s)

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

type Type = Type_ PredicateRef TypeRef
type FieldDef = FieldDef_ PredicateRef TypeRef
type TypeDef = TypeDef_ PredicateRef TypeRef
type PredicateDef = PredicateDef_ PredicateRef TypeRef SourceQuery

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

-- -----------------------------------------------------------------------------

-- | Version of the syntax. This is required so that we can change the
-- syntax while still allowing DBs that contain a schema with the old
-- syntax to be understood.
type AngleVersion = Int

checkReservedWordsInFieldNames :: AngleVersion -> Bool
checkReservedWordsInFieldNames = (> 2)

-- | Variables must begin with an upper-case letter, field names with
-- a lower-case letter.
caseRestriction :: AngleVersion -> Bool
caseRestriction = (> 3)

-- | New variable binding rules
varBinding :: AngleVersion -> Bool
varBinding = (> 4)

latestAngleVersion :: AngleVersion
latestAngleVersion = 5

-- -----------------------------------------------------------------------------
-- Pretty-printing

instance Pretty SourceRef where
  pretty (SourceRef name ver) = pretty name <> case ver of
    Nothing -> mempty
    Just ver -> "." <> pretty ver

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

instance (Pretty pref, Pretty tref, Pretty query) =>
    Pretty (PredicateDef_ pref tref query) where
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

instance (Pretty pat, Pretty stmt) => Pretty (SourceQuery_ pat stmt) where
  pretty (SourceQuery maybeHead stmts) = case stmts of
    [] -> pretty maybeHead
    _ -> case maybeHead of
      Just head -> hang 2 (sep (pretty head <+> "where" : pstmts))
      Nothing -> sep pstmts
    where
    pstmts = punctuate ";" (map pretty stmts)

instance Pretty pat => Pretty (SourceStatement_ pat) where
  pretty (SourceStatement lhs rhs) =
    hang 2 $ sep [pretty lhs <+> "=", pretty rhs]

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
