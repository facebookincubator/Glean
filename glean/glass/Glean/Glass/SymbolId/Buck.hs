{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Buck
  ({- instances -})
  where

import Glean.Glass.SymbolId.Class ( Symbol(..), toSymbolPredicate )
import Glean ( keyOf )

import qualified Glean.Schema.Buck.Types as Buck
import Glean.Glass.Utils as Utils

import Glean.Schema.CodeBuck.Types as CodeBuck
    ( Entity(..) )

instance Symbol CodeBuck.Entity where
  toSymbol e = case e of
    CodeBuck.Entity_locator locator -> toSymbolPredicate locator
    CodeBuck.Entity_file file ->
       ("f" :) <$> (Utils.pathFragments <$> Glean.keyOf file)
    CodeBuck.Entity_definition definition -> toSymbolPredicate definition
    CodeBuck.Entity_EMPTY -> return []

-- to avoid ambiguity when encoding locator, we add a tag between
-- the subdir and the path
instance Symbol Buck.Locator_key where
  toSymbol (Buck.Locator_key subdir path name) =
    let prefix = maybe [] Utils.pathFragments subdir in
    return $ concat [["t"], prefix, ["PATH"], Utils.pathFragments path,
                     [name]]

instance Symbol Buck.Definition_key where
  toSymbol (Buck.Definition_key module_ name) = do
    path <- Utils.pathFragments <$> Glean.keyOf module_
    return $ "d" : path ++ [ name ]
