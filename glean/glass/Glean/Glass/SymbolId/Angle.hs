{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Angle (
    {- instances and -}
  ) where

import qualified Glean

import Glean.Glass.SymbolId.Class
import Control.Monad.Catch ( throwM )
import Glean.Glass.Types (Path(..), Name(..))
import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Anglelang.Types as A
import qualified Glean.Schema.CodeAnglelang.Types as A
import Data.Text (Text)
import qualified Data.Text as Text

{-
  Angle symbol ids:
    <path>/<qualified_name>
    <path>/<attribute>/<qualified_name>

  Note that searchSymbol will assume that the last token is qualified_name
  if any extra attributes are added to the symbolid,
  they should be added before the qualified name.
-}

instance Symbol A.Entity where
  toSymbol _ = throwM $ SymbolError "Angle.Entity: use toSymbolWithPath"
  toSymbolWithPath e (Path p) = do
    syms <- toSymbol $ A.entity_decl e
    return $ case syms of
      [] ->  []
      (x:xs) -> Text.splitOn "/" p <> (x:xs)

instance Symbol A.Declaration where
  toSymbol d = case d of
        A.Declaration_pred x -> toSymbolPredicate x
        A.Declaration_ty x -> toSymbolPredicate x
        A.Declaration_schema x -> toSymbolPredicate x
        A.Declaration_imp x -> toSymbol x
        A.Declaration_derive_ x -> toSymbolPredicate x
        A.Declaration_evolve x -> toSymbolPredicate x
        A.Declaration_EMPTY -> return []

instance Symbol A.PredicateDecl_key where
  toSymbol A.PredicateDecl_key{..} = toSymbol predicateDecl_key_name

instance Symbol A.TypeDecl_key where
  toSymbol A.TypeDecl_key{..} = toSymbol typeDecl_key_name

instance Symbol A.SchemaDecl_key where
  toSymbol A.SchemaDecl_key{..} = toSymbol schemaDecl_key_name

instance Symbol A.DerivingDecl_key where
   toSymbol (A.DerivingDecl_key n _) = ("derived" :) <$> toSymbol n

instance Symbol A.EvolveDecl_key where
  toSymbol A.EvolveDecl_key{..} = do
  new <- Glean.keyOf evolveDecl_key_new_
  old <- Glean.keyOf evolveDecl_key_new_
  return [Text.intercalate "_evolves_" [new, old]]

instance Symbol A.Name where
  toSymbol k = do
    v <- Glean.keyOf k
    return [v]


-- Searching for Angle Entities
instance ToQName A.Entity where
  toQName e = toQName $ A.entity_decl e

instance ToQName A.Declaration where
  toQName d = case d of
    A.Declaration_pred x -> Glean.keyOf x >>= toQName
    A.Declaration_ty x -> Glean.keyOf x >>= toQName
    A.Declaration_schema x -> Glean.keyOf x >>= toQName
    A.Declaration_imp x -> toQNameEmptyNs x
    A.Declaration_evolve x -> Glean.keyOf x >>= toQName
    A.Declaration_derive_ x -> Glean.keyOf x >>= toQName
    A.Declaration_EMPTY -> return $ Left "unknown Declaration"

instance ToQName A.PredicateDecl_key where
  toQName A.PredicateDecl_key{..} = toQNameEmptyNs predicateDecl_key_name

instance ToQName A.TypeDecl_key where
  toQName A.TypeDecl_key{..} = toQNameEmptyNs typeDecl_key_name

instance ToQName A.SchemaDecl_key where
  toQName A.SchemaDecl_key{..} = toQNameEmptyNs schemaDecl_key_name

instance ToQName A.DerivingDecl_key where
  toQName A.DerivingDecl_key{..} = toQNameEmptyNs derivingDecl_key_name

instance ToQName A.EvolveDecl_key where
  toQName A.EvolveDecl_key{..} = do
  new <- Glean.keyOf evolveDecl_key_new_
  old <- Glean.keyOf evolveDecl_key_old_
  let qname = Text.intercalate "_evolves_" [new, old]
  return $ Right (Name qname, Name mempty)

toQNameEmptyNs :: A.Name -> Glean.RepoHaxl u w (Either Text (Name, Name))
toQNameEmptyNs name = do
  nameStr <- Glean.keyOf name
  return $ Right (Name nameStr, Name mempty)
