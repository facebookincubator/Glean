{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Hs ({- instances -}) where

import Data.Char
import qualified Data.Text as Text
import TextShow

import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))
import Glean.Schema.CodeHs.Types as Hs (Entity)
import qualified Glean
import qualified Glean.Schema.Hs.Types as Hs
import qualified Glean.Schema.Src.Types as Src

-- REPO/hs/containers/Data/Map/{var|datacon|tyvar|tycon}/toList[/START/END]

instance Symbol Hs.Entity where
  toSymbol = toSymbolPredicate

instance Symbol Hs.Name_key where
  toSymbol (Hs.Name_key occ mod sort) = do
    m <- toSymbol mod
    o <- toSymbol occ
    s <- toSymbol sort
    return $ m <> o <> s

instance Symbol Hs.Module where
  toSymbol = toSymbolPredicate

instance Symbol Hs.OccName where
  toSymbol = toSymbolPredicate

instance Symbol Hs.Module_key where
  toSymbol (Hs.Module_key name unit) = do
    u <- Glean.keyOf unit
    n <- Glean.keyOf name
    -- unit names are things like glean-0.1.0.0-inplace-core
    -- let's strip the version and everything after it
    let pkg = Text.intercalate "-" (fst (break isVer (Text.splitOn "-" u)))
    return (pkg : Text.splitOn "." n)
    where
    isVer t
      | Just (d, _) <- Text.uncons t = isDigit d
      | otherwise = False

instance Symbol Hs.OccName_key where
  toSymbol (Hs.OccName_key name namespace) = do
    let sp = case namespace of
          Hs.Namespace_var_ -> "var"
          Hs.Namespace_datacon -> "con"
          Hs.Namespace_tyvar -> "tyvar"
          Hs.Namespace_tycon -> "ty"
          _ -> error "namespace"
    return [sp,name]

instance Symbol Hs.NameSort where
  toSymbol Hs.NameSort_external{} = return []
  toSymbol (Hs.NameSort_internal (Src.ByteSpan start end)) =
    return [showt (Glean.fromNat start), showt (Glean.fromNat end)]

instance ToQName Hs.Entity where
  toQName n = Glean.keyOf n >>= toQName

instance ToQName Hs.Name_key where
  toQName (Hs.Name_key occ mod sort) = do
    Hs.Module_key m _ <- Glean.keyOf mod
    modname <- Glean.keyOf m
    Hs.OccName_key n _ <- Glean.keyOf occ
    return $ Right (Name modname, Name n)
