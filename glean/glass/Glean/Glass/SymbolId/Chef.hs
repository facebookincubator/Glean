{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Glass.SymbolId.Chef (
    {- instances and -}
  ) where

import qualified Glean
import Glean.Glass.SymbolId.Class
import Glean.Glass.Types (Name(..))
import qualified Data.Text as T

import qualified Glean.Schema.Chef.Types as Chef
import Glean.Schema.CodeChef.Types as CodeChef
    ( Entity(..) )


instance Symbol CodeChef.Entity where
  toSymbol e = case e of
    CodeChef.Entity_symbol x -> toSymbolPredicate x
    CodeChef.Entity_EMPTY -> return []

instance Symbol Chef.Symbol_key where
  toSymbol (Chef.Symbol_key identifier qualifiedName) = do
    qNames <- mapM Glean.keyOf qualifiedName
    name <- Glean.keyOf identifier
    return $ qNames ++ [ name ]

instance ToQName CodeChef.Entity where
  toQName e = case e of
    CodeChef.Entity_symbol definition -> do
      Chef.Symbol_key identifier qNameList <- Glean.keyOf definition
      qNameText <- mapM Glean.keyOf qNameList
      let path = T.intercalate "::" qNameText
      name <- Glean.keyOf identifier
      return (Right (Name name, Name path))
    CodeChef.Entity_EMPTY -> pure $ Left "ToQName: Unknown Chef entity"
