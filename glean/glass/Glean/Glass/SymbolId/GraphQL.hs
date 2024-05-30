{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.GraphQL
  ({- instances -})
  where

import Glean.Glass.SymbolId.Class
import Glean.Glass.Utils
import Glean.Glass.Types (Name(..))

import qualified Glean.Schema.CodeGraphql.Types as GraphQL
import qualified Glean.Schema.Graphql.Types as GraphQL
import qualified Glean.Schema.Src.Types as Src

import qualified Glean
import Glean.Angle as Angle

instance Symbol GraphQL.Entity where
    toSymbol x = case x of
        GraphQL.Entity_decl d -> toSymbol d
        GraphQL.Entity_EMPTY -> return []

instance Symbol GraphQL.Declaration where
    toSymbol x = case x of
        GraphQL.Declaration_operation_ o -> toSymbolPredicate o
        GraphQL.Declaration_fragment_ f -> toSymbolPredicate f
        GraphQL.Declaration_field_ f -> toSymbolPredicate f
        GraphQL.Declaration_enum_ e -> toSymbolPredicate e
        GraphQL.Declaration_directive_ d -> toSymbolPredicate d
        GraphQL.Declaration_EMPTY{} -> return []

instance Symbol GraphQL.Operation_key where
    toSymbol GraphQL.Operation_key{..} = do
        name <- Glean.keyOf operation_key_name
        return ["o", name]

instance Symbol GraphQL.Fragment_key where
    toSymbol GraphQL.Fragment_key{..} = do
        name <- Glean.keyOf fragment_key_name
        return [name]

instance Symbol GraphQL.FieldDef_key where
    toSymbol GraphQL.FieldDef_key{..} = do
        name <- Glean.keyOf fieldDef_key_name
        return ["f", name]

instance Symbol GraphQL.EnumTypeDef_key where
    toSymbol GraphQL.EnumTypeDef_key{..} = do
        name <- Glean.keyOf enumTypeDef_key_name
        return ["e", name]

instance Symbol GraphQL.DirectiveDef_key where
    toSymbol GraphQL.DirectiveDef_key{..} = do
        name <- Glean.keyOf directiveDef_key_name
        return ["d", name]

instance ToQName GraphQL.Entity where
    toQName e = case e of
        GraphQL.Entity_decl d -> toQName d
        GraphQL.Entity_EMPTY -> return $ Left "unknown GraphQL.Entity"

instance ToQName GraphQL.Declaration where
    toQName e = case e of
        GraphQL.Declaration_operation_ o -> Glean.keyOf o >>= toQName
        GraphQL.Declaration_fragment_ f -> Glean.keyOf f >>= toQName
        GraphQL.Declaration_field_ f -> Glean.keyOf f >>= toQName
        GraphQL.Declaration_enum_ e -> Glean.keyOf e >>= toQName
        GraphQL.Declaration_directive_ d -> Glean.keyOf d >>= toQName
        GraphQL.Declaration_EMPTY{} ->
            return $ Left "unknown GraphQL.Declaration"

instance ToQName GraphQL.Operation_key where
    toQName GraphQL.Operation_key{..} = do
        name <- Glean.keyOf operation_key_name
        let Src.FileLocation{..} = operation_key_loc
        mconfig <- fetchData (buildConfig (Glean.getId fileLocation_file))
        case mconfig of
            Just c -> do
                config <- Glean.keyOf c
                return $ Right (Name name, Name config)
            Nothing -> return $ Right (Name name, Name "")

instance ToQName GraphQL.Fragment_key where
    toQName GraphQL.Fragment_key{..} = do
        name <- Glean.keyOf fragment_key_name
        let Src.FileLocation{..} = fragment_key_loc
        mconfig <- fetchData (buildConfig (Glean.getId fileLocation_file))
        case mconfig of
            Just c -> do
                config <- Glean.keyOf c
                return $ Right (Name name, Name config)
            Nothing -> return $ Right (Name name, Name "")

instance ToQName GraphQL.FieldDef_key where
    toQName GraphQL.FieldDef_key{..} = do
        name <- Glean.keyOf fieldDef_key_name
        return $ Right (Name name, Name "")

instance ToQName GraphQL.EnumTypeDef_key where
    toQName GraphQL.EnumTypeDef_key{..} = do
        name <- Glean.keyOf enumTypeDef_key_name
        return $ Right (Name name, Name "")

instance ToQName GraphQL.DirectiveDef_key where
    toQName GraphQL.DirectiveDef_key{..} = do
        name <- Glean.keyOf directiveDef_key_name
        return $ Right (Name name, Name "")

buildConfig :: Glean.IdOf Src.File -> Angle GraphQL.Value
buildConfig fileId = var $ \config ->
    config `where_` [
        wild .= predicate @GraphQL.BelongToConfig (
            rec $
                field @"file" (asPredicate (factId fileId)) $
                field @"buildConfig" (asPredicate config)
            end)
    ]
