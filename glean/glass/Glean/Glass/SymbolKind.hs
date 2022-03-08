{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.SymbolKind
  (
  -- * searching by prefix
    findSymbolKind
  ) where

import Data.Text (Text)

import qualified Glean.Haxl.Repos as Glean
import qualified Glean.Schema.Code.Types as Code

import Glean.Glass.Attributes.SymbolKind as Glass
    ( symbolKindToSymbolKind )
import Glean.Glass.SymbolId ( entityToAngle )
import Glean.Glass.Query as Query
import Glean.Glass.Types as Glass
import Glean.Glass.Utils as Utils

-- | Pointwise lookup of a symbol kind by entity
findSymbolKind
  :: Code.Entity
  -> Glean.RepoHaxl u w (Either Text Glass.SymbolKind)
findSymbolKind e = case entityToAngle e of
  Left err -> return $ Left $ "ToSymbolKind: " <> err
  Right ent -> do
    r <- Utils.searchWithLimit (Just 1) $ Query.symbolKind ent
    return $ case r of
      [] -> Left "No kind found"
      (kind:_) -> Right $ Glass.symbolKindToSymbolKind kind
