{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Schema.Evolve
  ( resolveEvolves
  , evolveOneSchema
  , canEvolve
  , VisiblePredicates(..)
  ) where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Extra (whenJust)
import Data.Bifunctor
import Data.Foldable
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Glean.Angle.Types
import Glean.Schema.Types
import Glean.Schema.Util

-- | Create a mapping from a schema to the schema that evolves it. This will
--  - check if any schema is evolved by multiple schemas.
--  - check if all schema evolutions are legal.
--
-- If 'B evolves A' and 'C evolves A', when a query for A comes we won't
-- know whether to serve facts from C or from B. Therefore we disallow
-- multiple schemas to evolve a single one.
resolveEvolves
  :: [ResolvedSchemaRef] -- ^ in dependency order
  -> Either Text (HashMap SchemaRef SchemaRef)
resolveEvolves resolved = do
  checkLawfulEvolves
  HashMap.traverseWithKey checkMultipleEvolves
    $ HashMap.fromListWith (++)
    $ concatMap evolvedByResolved resolved
  where
    evolvedByResolved new =
        [ (oldRef, [schemaRef new])
        | oldRef <- Set.toList $ resolvedSchemaEvolves new ]

    checkMultipleEvolves old newList =
      case nub newList of
        [new] -> Right new
        _     -> Left $ "multiple schemas evolve "
          <> showSchemaRef old
          <> ": "
          <> Text.unwords (map showSchemaRef newList)

    resolvedByRef :: Map SchemaRef ResolvedSchemaRef
    resolvedByRef = Map.fromList [(schemaRef s, s) | s <- resolved ]


    -- Later definitions override earlier ones in case of db overrides
    types = HashMap.unions $ reverse $ map resolvedSchemaTypes resolved

    checkLawfulEvolves =
      foldM_ (evolveOneSchema types predicateRef_name) mempty
        [ (VisiblePredicates
            (resolvedSchemaPredicates new)
            (resolvedSchemaReExportedPredicates new),
           VisiblePredicates
            (resolvedSchemaPredicates old)
            (resolvedSchemaReExportedPredicates old))
        | new <- resolved
        , oldRef <- Set.toList $ resolvedSchemaEvolves new
        , Just old <- [Map.lookup oldRef resolvedByRef]
        ]

data VisiblePredicates p t = VisiblePredicates
  { visiblePredicates :: HashMap p (PredicateDef_ SrcSpan p t)
  , visibleReexported :: HashMap p (PredicateDef_ SrcSpan p t)
  }

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
--       name = PredicateName
--
-- 2. To check compatibility between complete schemas when a new
--    schema instance is added to the SchemaIndex. We're dealing with
--    complete hashed schemas in this case, so
--       p = PredicateId
--       t = TypeId
--       name = PredicateRef

evolveOneSchema
  :: forall p t name.
     (Eq p, ShowRef p, Hashable p,
      Eq t, ShowRef t, Hashable t,
      Eq name, Ord name, Hashable name)
  => HashMap t (TypeDef_ p t)
       -- ^ all type definitions
  -> (p -> name)
      -- ^ Extract the "name" from a predicate. Predicates with the same
      -- name in the new/old schema must be compatible.
  -> HashMap p p
      -- ^ Existing predicate evolutions (key evolves value)
  -> (VisiblePredicates p t, VisiblePredicates p t)
      -- ^ (new, old) schemas
  -> Either Text (HashMap p p)
evolveOneSchema types predicateName evolvedBy
    (VisiblePredicates newPreds newReExp,
     VisiblePredicates oldPreds oldReExp) = do
  checkBackCompatibility
  return evolvedBy'
  where
    checkBackCompatibility :: Either Text ()
    checkBackCompatibility =
      forM_ oldPreds $ \oldDef -> do
        whenJust (matchNew oldDef) $ \newDef ->
          evolveDef newDef oldDef

    matchNew oldDef = HashMap.lookup name newPredsByName
      where name = predicateName $ predicateDefRef oldDef

    evolveDef
      (PredicateDef ref key val _)
      (PredicateDef _ oldkey oldval _) =
      let
          keyErr = key `canEvolve'` oldkey
          valErr = val `canEvolve'` oldval
      in case keyErr <|> valErr of
        Nothing -> return ()
        Just err -> throwError $
          "cannot evolve predicate " <> showRef ref <> ": " <> err

    canEvolve' :: Type_ p t -> Type_ p t -> Maybe Text
    canEvolve' = canEvolve types evolvedBy'

    -- add evolutions from current schema
    evolvedBy' :: HashMap p p
    evolvedBy' = foldr addEvolution evolvedBy oldPredKeys
      where
        oldPredKeys = HashMap.keys (oldPreds <> oldReExp)
        addEvolution oldPred acc =
          case HashMap.lookup (predicateName oldPred) newPredsByName of
            Nothing -> acc
            Just newPred -> HashMap.insert oldPred (predicateDefRef newPred) acc

    newPredsByName :: HashMap name (PredicateDef_ SrcSpan p t)
    newPredsByName = mapKeys predicateName (newPreds <> newReExp)

    mapKeys f = HashMap.fromList . map (first f) . HashMap.toList

data Opt = Option | FieldOpt

-- | Check if a type is backward and forward compatible.
--
-- For backward compatibility to work if predicate A depends on predicate
-- B, evolved A must depend on evolved B.  That is, the following diagram must
-- commute
--
--     A --------> evolved(A)
--     |           |
--     |           | depends-on
--     ∨           ∨
--     B --------> evolved(B)
--      evolved-by
--
canEvolve
  :: (Eq p, Eq t, ShowRef p, ShowRef t, Hashable p, Hashable t)
  => HashMap t (TypeDef_ p t) -- ^ type definitions
  -> HashMap p p              -- ^ current evolutions map
  -> Type_ p t                -- ^ updated type
  -> Type_ p t                -- ^ old type
  -> Maybe Text               -- ^ compatibility error
canEvolve types evolvedBy new old = go new old
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
    go (PredicateTy new) (PredicateTy old)
      | evolved new /= evolved old = Just
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
    go _ _ = Just "type changed"

    -- get most evolved version of a predicate
    evolved p = case HashMap.lookup p evolvedBy of
      Nothing -> p
      Just p' -> if p' == p then p else evolved p'

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
