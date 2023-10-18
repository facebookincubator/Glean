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

import Glean.Glass.SymbolId.Class
import qualified Glean

import qualified Glean.Schema.Buck.Types as Buck
import Glean.Glass.Utils as Utils
import Glean.Glass.Types

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

instance ToQName CodeBuck.Entity where
  toQName e = case e of
    CodeBuck.Entity_locator locator -> do
      Buck.Locator_key _subdir path name <- Glean.keyOf locator
      return $ Right (Name name, Name path)

    CodeBuck.Entity_file file -> do
      path <- Glean.keyOf file
      return $ case reverse (pathFragments path) of
        [] -> Left "QName not supported for empty Buck file path"
        (h:t) -> Right (Name h, Name (joinFragments (reverse t)))

    CodeBuck.Entity_definition definition -> do
      Buck.Definition_key module_ name <- Glean.keyOf definition
      path <- Glean.keyOf module_
      return (Right (Name name, Name path))

    CodeBuck.Entity_EMPTY -> pure $ Left "ToQName: Unknown Buck entity"
