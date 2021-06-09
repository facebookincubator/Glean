{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Types representing a source-level schema
module Glean.Angle.Types
  ( Name
  , Version
  , AngleVersion
  , latestAngleVersion
  , checkReservedWordsInFieldNames
  , caseRestriction
  , varBinding
  , PredicateRef(..)
  , TypeRef(..)
  , Type_(..)
  , TypeDef_(..)
  , FieldDef_(..)
  , PredicateDef_(..)
  , Type
  , FieldDef
  , TypeDef
  , PredicateDef
  , DerivingInfo(..)
  , DeriveWhen(..)
  , SourceSchema
  , SourceSchemas
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
  , SourceSchema_(..)
  , SourceDecl_(..)
  ) where

import Data.Text.Prettyprint.Doc

import Glean.Query.Types hiding (Nat, String, Array)
import Glean.Types (PredicateRef(..), TypeRef(..), Version)

-- | A Glean Type
data Type_ pref tref
  -- Native types
  = Byte
  | Nat
  | String
  | Array (Type_ pref tref)
  | Record [FieldDef_ pref tref]
  | Sum [FieldDef_ pref tref]
  | Predicate pref
  | NamedType tref

  -- Derived types. These appear in the source language, but are
  -- translated into the native types above internally. They also
  -- act as hints to the code generators, e.g. maybe translates to
  -- Thrift's optional and enum translates to Thrift's enum.
  | Maybe (Type_ pref tref)  -- maybe T  => { nothing | just : T }
  | Enumerated [Name]        -- enum { a | b } => { a : {}, b : {} }
  | Boolean                  -- bool => { false : {} | true : {} }
  deriving (Eq, Show)

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

-- | A reference to a name or type in a source schema. Versions are
-- often omitted at this level, and will be later resolved by
-- resolveSchema.
data SourceRef = SourceRef
  { sourceRefName :: Name
  , sourceRefVersion :: Maybe Version
  }
  deriving (Show, Eq, Ord)

data FieldDef_ pref tref = FieldDef
  { fieldDefName :: Name
  , fieldDefType :: Type_ pref tref
  }
  deriving (Eq, Show)

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

type SourcePat' s = SourcePat_ s Name SourceType
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
type SourceDecl = SourceDecl_ SrcSpan

type Type = Type_ PredicateRef TypeRef
type FieldDef = FieldDef_ PredicateRef TypeRef
type TypeDef = TypeDef_ PredicateRef TypeRef
type PredicateDef = PredicateDef_ PredicateRef TypeRef SourceQuery

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

instance (Pretty pref, Pretty tref) => Pretty (Type_ pref tref) where
  pretty Byte = "byte"
  pretty Nat = "nat"
  pretty String = "string"
  pretty (Array ty) = "[" <> pretty ty <> "]"
  pretty (Record fields) =
    sep
      [ nest 2 $ vsep $ "{" :  punctuate "," (map pretty fields)
      , "}" ]
  pretty (Sum fields) =
    sep
      [ nest 2 $ vsep $ "{" :  map (<> " |") (map pretty fields)
      , "}" ]
  pretty (Predicate p) = pretty p
  pretty (NamedType t) = pretty t
  pretty (Maybe t) = "maybe" <+> pretty t
  pretty (Enumerated names) =
    sep
      [ nest 2 $ vsep $ "enum {" :  map (<> " |") (map pretty names)
      , "}" ]
  pretty Boolean = "bool"

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
         Record [] -> []
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
    ("version:" <+> pretty srcAngleVersion) : map pretty srcSchemas

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
