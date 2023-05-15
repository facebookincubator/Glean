{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.SCIP ({- instances -}) where

import qualified Data.Text as Text

import qualified Glean
import qualified Glean.Schema.Scip.Types as Scip
import Glean.Glass.Types ( Name(Name) )

import Glean.Glass.SymbolId.Class

instance Symbol Scip.SomeEntity where
  toSymbol (Scip.SomeEntity defn) = toSymbolPredicate defn

instance Symbol Scip.Definition_key where
  toSymbol (Scip.Definition_key symbol _filerange) = do
    str <- Glean.keyOf symbol
    return (Text.splitOn " " str)

instance ToQName Scip.SomeEntity where
  toQName (Scip.SomeEntity defn) = Glean.keyOf defn >>= toQName

instance ToQName Scip.Definition_key where
  toQName (Scip.Definition_key symbol _range) = do
    str <- Glean.keyOf symbol
    return $ Right $ case reverse (Text.splitOn " " str) of
      [] -> (Name "", Name "")
      (x:_) -> (Name x, Name "")
