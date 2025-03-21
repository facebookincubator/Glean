{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{- |
This module focuses on determining which predicate should be transformed
into which by looking at the contents of schemas and at the evolution
relationships between schemas.
-}
{-# LANGUAGE CPP #-}
module Glean.Schema.Evolve
  ( validateResolvedEvolutions
  , directSchemaEvolutions
  , calcEvolutions
  , validateEvolutions
  , visiblePredicates
  , mapVisible
  , VisiblePredicates
  , visibleDefined
  , canBeStored
  ) where

import Control.Applicative
import Control.Monad.Except
#if !MIN_VERSION_base(4,16,0)
import Data.Foldable
#endif
import Data.Graph
import Data.Hashable
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.List.Extra (nubOrdOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Glean.Angle.Types
import Glean.Display
import Glean.Schema.Types
import Glean.Schema.Util

{- Note [Schema Evolutions]

An 'schema evolution' is a non-commuting transitive relationship between two
schemas. It is established by a source code annotation saying:

    schema A evolves B

It establishes a 'predicate evolution' relationship between all predicates
exported by B and predicates exported by A if they have the same name. Schema
evolutions must form a graph with no cycles.

A 'predicate evolution' is a reflexive, commutative, non-transitive
relationship between two predicates. It means that facts from one predicate can
be transformed into facts from the other.

It is established in two cases:

  - predicate P evolves predicate Q if the schema containing P evolves the
  schema containing Q and both P and Q have the same name.
  This is caused by a source code annotation.

  - predicate P evolves predicate Q if they are defined in schemas with the
  same SchemaRef and have the same name but different hashes.
  This is caused by a version-less schema migration and the two instances
  live in different ProcessedSchema.

NB: Although 'predicate evolution' is not a transitive relationship, we
expect it to work in all transitively evolved predicates because we want
'schema evolution' to be transitive.

Example of why predicate evolution is not transitive:
  predicate P.1 = { a : maybe nat }
  predicate P.2 = {}
  predicate P.3 = { a : maybe string }

P.2 can evolve P.1 and P.3 can evolve P.2, but P.3 cannot evolve P.1.
-}

-- | As specified by a `schema x evolves y` line.
-- Does not take the db's content into account.
-- value evolves key
directSchemaEvolutions
  :: [ResolvedSchemaRef]
  -> Either Text (Map SchemaRef SchemaRef)
directSchemaEvolutions schemas =
  Map.traverseWithKey checkMultipleEvolves $
  Map.fromListWith (++)
    [ (evolved, [schemaRef resolved])
    | resolved <- schemas
    , evolved <- Set.toList $ resolvedSchemaEvolves resolved
    ]
  where
  checkMultipleEvolves old newList =
    case nub newList of
      [new] -> Right new
      _     -> Left $ "multiple schemas evolve "
        <> showSchemaRef old
        <> ": "
        <> Text.unwords (map showSchemaRef newList)

-- | Visible predicates which can be evolved.
data VisiblePredicates p = VisiblePredicates
  { visibleDefined :: HashSet p
  , visibleReexported :: HashSet p
  }

visiblePredicates :: ResolvedSchemaRef -> VisiblePredicates PredicateRef
visiblePredicates ResolvedSchema{..} = VisiblePredicates
  { visibleDefined = evolvable resolvedSchemaPredicates
  , visibleReexported = evolvable resolvedSchemaReExportedPredicates
  }
  where
  evolvable :: HashMap PredicateRef ResolvedPredicateDef -> HashSet PredicateRef
  evolvable = HashSet.fromList . filter stored . HashMap.keys

  stored ref =
    maybe True canBeStored $ HashMap.lookup ref resolvedSchemaDeriving

canBeStored :: DerivingInfo a -> Bool
canBeStored = \case
  Derive DeriveOnDemand _ -> False
  Derive DerivedAndStored _ -> True
  Derive DeriveIfEmpty _ -> True
  NoDeriving -> True

mapVisible
  :: (Eq q, Hashable q)
  => (p -> q)
  -> VisiblePredicates p
  -> VisiblePredicates q
mapVisible f (VisiblePredicates d r) =
  VisiblePredicates (HashSet.map f d) (HashSet.map f r)

instance (Eq p, Hashable p) => Semigroup (VisiblePredicates p) where
  VisiblePredicates d r <> VisiblePredicates d' r' =
    VisiblePredicates (d <> d') (r <> r')

instance (Eq p, Hashable p) => Monoid (VisiblePredicates p) where
  mappend = (<>)
  mempty = VisiblePredicates mempty mempty

-- | Given evolutions between schemas and evolutions between instances of the
-- same predicate, calculate the final instance-to-instance predicate
-- evolutions.
calcEvolutions
  :: forall p. (Eq p, Ord p, Hashable p, ShowRef p)
  => (p -> PredicateRef)
  -> Map PredicateRef [p]          -- ^ all predicates by ref
  -> Map SchemaRef (VisiblePredicates p) -- ^ all predicates by schema ref
  -> Map SchemaRef SchemaRef       -- ^ evolutions from src code directives
  -> Map PredicateRef p            -- ^ evolutions from versionless migrations
  -> Either Text (HashMap p p)
calcEvolutions toRef byPredRef bySchemaRef schemaEvolutions autoEvolutions =
  case cycles evolutions of
    [] -> Right evolutions
    []:_ -> error "calcEvolutions"
    (x:xs):_ -> Left $ "found a cycle in predicate evolutions: " <>
      Text.intercalate " -> " (map showRef $ x:xs ++ [x])
  where
  cycles :: Ord a => HashMap a a -> [[a]]
  cycles hm = [ cycle | CyclicSCC cycle <- scc]
    where
    scc = stronglyConnComp [ (k,k,[v]) | (k, v) <- HashMap.toList hm ]

  evolutions = fromSourceAnnotations `HashMap.union` fromVersionlessMigrations

  fromVersionlessMigrations :: HashMap p p
  fromVersionlessMigrations = HashMap.fromList
    [ (old, new)
    | (ref, preds) <- Map.toList byPredRef
    , old <- preds
    , Just new <- [Map.lookup ref autoEvolutions]
    , new /= old
    ]

  fromSourceAnnotations :: HashMap p p
  fromSourceAnnotations =
    HashMap.unions
    $ map concrete
    $ map (uncurry match)
    $ Map.toList schemaEvolutions
    where
      concrete :: HashMap PredicateRef PredicateRef -> HashMap p p
      concrete = HashMap.fromList . concatMap expand . HashMap.toList

      expand :: (PredicateRef, PredicateRef) -> [(p,p)]
      expand (from, to) =
        [ (from', to')
        | from' <- allInstances from
        , Just to' <- [mostEvolvedInstance to]
        ]

      allInstances :: PredicateRef -> [p]
      allInstances ref = Map.findWithDefault [] ref byPredRef

      mostEvolvedInstance :: PredicateRef -> Maybe p
      mostEvolvedInstance ref = evolved <|> anyInstance
        where
        evolved = Map.lookup ref autoEvolutions
        anyInstance = listToMaybe =<< Map.lookup ref byPredRef

  match :: SchemaRef -> SchemaRef -> HashMap PredicateRef PredicateRef
  match old new = HashMap.fromList
    [ (rold, rnew)
    | rold <- predicates old
    , Just rnew <- [Map.lookup (name rold) newByName]
    , rnew /= rold -- avoid re-exports evolving themselves
    ]
    where
    newByName :: Map Name PredicateRef
    newByName = Map.fromList [ (name ref, ref) | ref <- predicates new ]

  predicates :: SchemaRef -> [PredicateRef]
  predicates sref =
    -- defined override reexported
    nubOrdOn name
      $ map toRef
      $ HashSet.toList defined <> HashSet.toList reexported
    where
      VisiblePredicates defined reexported =
        Map.findWithDefault mempty sref bySchemaRef

  name :: PredicateRef -> Name
  name = predicateRef_name

-- | Check schemas for compatibility and map each predicate to their
-- evolved counterpart in the the evolvedBy map.
--
-- This is abstracted over the type of predicates and types, because
-- we use it in two different ways:
--
-- 1. To check the validity of "schema A evolves B" declarations.  This
--    check is performa at schema resolution time, so
--       p = PredicateRef
--       t = TypeRef
--
-- 2. To check compatibility between complete schemas when a new
--    schema instance is added to the SchemaIndex. We're dealing with
--    complete hashed schemas in this case, so
--       p = PredicateId
--       t = TypeId
--
validateEvolutions
  :: (Eq p, Eq t, ShowRef p, ShowRef t,
      Hashable p, Hashable t, Display p, Display t)
  => Maybe (p -> Bool)               -- ^ does the db has facts of p
  -> HashMap t (TypeDef_ p t)        -- ^ types to their definitions
  -> HashMap p (PredicateDef_ s p t) -- ^ predicates to their definitions
  -> HashMap p p                     -- ^ predicate evolutions
  -> Either Text ()
validateEvolutions mHasFacts types preds evolutions =
  void $ HashMap.traverseWithKey validate evolutions
  where
    validate old new =
      let PredicateDef _ oldKey oldVal _ _ = preds HashMap.! old
          PredicateDef _ newKey newVal _ _ = preds HashMap.! new
          keyErr = newKey `canEvolve'` oldKey
          valErr = newVal `canEvolve'` oldVal
      in
      case keyErr <|> valErr of
        Nothing -> Right ()
        Just err -> Left $
          "cannot evolve predicate " <> showRef old <>
          " into " <> showRef new <> ": " <> err

    canEvolve' = canEvolve types compatible

    compatible new old =
      case mHasFacts of
        Nothing ->
          -- Here we are validating an evolution graph without a database.
          -- Compatible predicates must ultimately evolve to the same
          -- PredicateRef. We ignore hashes because auto-evolutions will make
          -- all predicates with the same PredicateRef evolve to the same thing
          -- in a concrete database.
          ref (evolved new) == ref (evolved old)
        Just hasFacts
            -- if there are no facts, there will be no transformation so all
            -- types are considered compatible.
          | not (hasFacts old) -> True
            -- there will be a transformation so the evolutions map should have
            -- an exact match.
          | otherwise ->
            HashMap.lookupDefault old old evolutions
              ==  HashMap.lookupDefault new new evolutions

    ref p = predicateDefRef $ preds HashMap.! p

    -- get most evolved version of a predicate
    evolved p = case HashMap.lookup p evolutions of
      Nothing -> p
      Just p' -> if p' == p then p else evolved p'


-- | Create a mapping from a schema to the schema that evolves it. This will
--  - check if any schema is evolved by multiple schemas.
--  - check if all schema evolutions are legal.
--
-- If 'B evolves A' and 'C evolves A', when a query for A comes we won't
-- know whether to serve facts from C or from B. Therefore we disallow
-- multiple schemas to evolve a single one.
validateResolvedEvolutions :: [ResolvedSchemaRef] -> Either Text ()
validateResolvedEvolutions resolved = do
  schemaEvolutions <- directSchemaEvolutions resolved
  let autoEvolutions = mempty
  evolutions <- calcEvolutions
    id
    byRef
    bySchemaRef
    schemaEvolutions
    autoEvolutions

  validateEvolutions Nothing types preds evolutions
  where
    byRef :: Map PredicateRef [PredicateRef]
    byRef = Map.fromList [ (ref, [ref]) | ref <- HashMap.keys preds ]

    bySchemaRef :: Map SchemaRef (VisiblePredicates PredicateRef)
    bySchemaRef = Map.fromList
      [ (schemaRef schema, visiblePredicates schema)
      | schema <- resolved
      ]

    -- Later definitions override earlier ones in case of db overrides
    -- (is this really the case or are overrides added to a new ProcessedSchema?)
    preds :: HashMap PredicateRef
      (PredicateDef_ SrcSpan PredicateRef TypeRef)
    preds = HashMap.unions $ reverse $ map resolvedSchemaPredicates resolved

    types :: HashMap TypeRef (TypeDef_ PredicateRef TypeRef)
    types = HashMap.unions $ reverse $ map resolvedSchemaTypes resolved

data Opt = Option | FieldOpt

-- | Check if a type is backward and forward compatible.
--
-- For backward and forward compatibility the rules are:
--  - only add or remove fields with defaultable values
--  - if a field referencing a predicate is changed, it must be
--    to a compatible predicate reference.
--
-- Two predicate references are compatible if they evolve to the same predicate
-- or have the same PredicateRef.
canEvolve
  :: (Eq p, Eq t, ShowRef p, ShowRef t,
      Hashable p, Hashable t, Display p, Display t)
  => HashMap t (TypeDef_ p t) -- ^ type definitions
  -> (p -> p -> Bool)         -- ^ whether two predicates are compatible
  -> Type_ p t                -- ^ updated type
  -> Type_ p t                -- ^ old type
  -> Maybe Text               -- ^ compatibility error
canEvolve types compatible new old = go new old
  where
    get ty = case HashMap.lookup ty types of
      Just v -> typeDefType v
      Nothing -> error $ "unknown type " <> Text.unpack (showRef ty)

    go (NamedTy new) (NamedTy old)
      | new == old = Nothing
      | otherwise = go (get new) (get old)
    go (NamedTy t) old = go (get t) old
    go new (NamedTy t) = go new (get t)
    go (MaybeTy new) (MaybeTy old) = go new old
    go ByteTy ByteTy = Nothing
    go NatTy NatTy = Nothing
    go StringTy StringTy = Nothing
    go BooleanTy BooleanTy = Nothing
    go (ArrayTy new) (ArrayTy old) = go new old
    go (SetTy new) (SetTy old) = go new old
    go (PredicateTy new) (PredicateTy old)
      | not (compatible new old) = Just
          $ "type changed from " <> showRef old
          <> " to " <> showRef new
      | otherwise = Nothing
    go (EnumeratedTy new) (EnumeratedTy old) =
      compareFieldList Option new' old'
      where
        new' = map unitOpt new
        old' = map unitOpt old
        unitOpt name = FieldDef name (RecordTy [])
    go (SumTy new) (SumTy old) = compareFieldList Option new old
    go (RecordTy new) (RecordTy old) = compareFieldList FieldOpt new old
    go (SetTy new) (ArrayTy old) = go new old
    go (ArrayTy new) (SetTy old) = go new old
    go new old = Just $ Text.pack $
      "type changed from " <>
        show (displayDefault old) <> " to " <>
        show (displayDefault new)

    compareFieldList optName new old =
      removedFieldsError <|> newRequiredFieldsError <|>
      asum (map compareField matchingFields)
      where
        names = map fieldDefName
        oldByName = Map.fromList (zip (names old) old)
        newByName = Map.fromList (zip (names new) new)
        matchingFields =
          [ (name, fNew, fOld)
          | FieldDef name fNew <- new
          , Just (FieldDef _ fOld) <- [Map.lookup name oldByName]
          ]
        compareField (name, new, old) = addLocation <$> go new old
          where
            addLocation err =
              "in " <> showOpt optName <> " '" <> name <> "', " <> err

        addedFields = Map.difference newByName oldByName
        removedFields = Map.difference oldByName newByName
        required fields = Map.filter (not . hasDefault . fieldDefType) fields
        hasDefault ty = case ty of
          MaybeTy _ -> True
          NatTy -> True
          StringTy -> True
          BooleanTy -> True
          ByteTy -> True
          ArrayTy _ -> True
          SetTy _ -> True
          RecordTy fields -> all (hasDefault . fieldDefType) fields
          EnumeratedTy{} -> True
          SumTy (first : _) -> hasDefault (fieldDefType first)
          NamedTy ty -> hasDefault (get ty)
          _ -> False

        newRequiredFields = Map.keys (required addedFields)
        removedRequiredFields = Map.keys (required removedFields)

        removedFieldsError = case optName of
          Option -> Nothing
          FieldOpt -> case removedRequiredFields  of
            [] -> Nothing
            fields -> Just $ "missing required " <> plural optName fields
              <> ": " <> Text.unwords fields

        newRequiredFieldsError = case optName of
          Option -> Nothing
          FieldOpt -> case newRequiredFields of
            [] -> Nothing
            _ -> Just $ Text.unlines
              [ Text.unwords [ "required" , plural optName newRequiredFields
                , "added:" , Text.unwords newRequiredFields ]
              , "For backward and forward compatibility, predicate evolutions"
                <> " require that all new fields are non-predicate types"
              ]

    plural s [_] = showOpt s
    plural s _ = showOpt s <> "s"
    showOpt Option = "option"
    showOpt FieldOpt = "field"
