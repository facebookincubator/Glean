{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Glass.Search
  ( searchEntity
  , SearchResult(..)
  , SearchEntity(..)
  , CodeEntityLocation(..)
  , prefixSearchEntity
  ) where

import Data.Text ( Text )
import Control.Monad.Catch (throwM)

import Glean.Glass.Repos (Language(..) )
import Glean.Glass.SymbolId ( toShortCode )

import Glean.Glass.Search.Class as Search
    ( Search(symbolSearch),
      SearchResult(..),
      SearchEntity(..),
      PrefixSearch(..),
      CodeEntityLocation(..),
      ResultLocation)
import qualified Glean.Glass.Search.Buck ({- instances -})
import qualified Glean.Glass.Search.Cxx ({- instances -})
import qualified Glean.Glass.Search.Erlang ({- instances -})
import qualified Glean.Glass.Search.Flow ({- instances -})
import qualified Glean.Glass.Search.Hack ({- instances -})
import qualified Glean.Glass.Search.Haskell ({- instances -})
import qualified Glean.Glass.Search.LSIF ({- instances -})
import qualified Glean.Glass.Search.SCIP ({- instances -})
import qualified Glean.Glass.Search.Python ({- instances -})
import qualified Glean.Glass.Search.Thrift ({- instances -})
import Glean.Glass.Types (ServerException(ServerException))
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Haxl.Repos as Glean

--
-- | Entity search: decodes a symbol id to a code.Entity fact
--
-- Note: this is different to e.g. approximate string search, as we
-- should _always_ be able to decode valid symbol ids back to their (unique*)
-- entity. Unlike searchSymbol() we typically have full entity scope information
-- sufficient to uniquely identify the symbol in an index.
--
-- There are cases where symbol ids are not unique:
--
-- - weird code
-- - hack namespaces
-- - bugs/approximations in our encoder
--
-- We log the duplicates to glass_errors
--
searchEntity
  :: Language
  -> [Text]
  -> Glean.ReposHaxl u w (SearchResult Code.Entity)
searchEntity lang toks = case lang of
  Language_Cpp -> fmap Code.Entity_cxx <$> Search.symbolSearch toks
  Language_Hack -> fmap Code.Entity_hack <$> Search.symbolSearch toks
  Language_Python -> fmap Code.Entity_python <$> Search.symbolSearch toks
  Language_JavaScript -> fmap Code.Entity_flow <$> Search.symbolSearch toks
  Language_Haskell -> fmap Code.Entity_hs <$> Search.symbolSearch toks
  Language_Erlang -> fmap Code.Entity_erlang <$> Search.symbolSearch toks
  Language_Buck -> fmap Code.Entity_buck <$> Search.symbolSearch toks
  Language_Thrift -> fmap Code.Entity_thrift <$> Search.symbolSearch toks
  -- limited set via lsif
  Language_TypeScript -> fmap Code.Entity_lsif <$> Search.symbolSearch toks
  Language_Java -> fmap Code.Entity_lsif <$> Search.symbolSearch toks
  -- scip-based indexers
  Language_Rust -> fmap Code.Entity_scip <$> Search.symbolSearch toks
  Language_Go -> fmap Code.Entity_scip <$> Search.symbolSearch toks
  lang ->
    return $ None $ "searchEntity: language not supported: " <> toShortCode lang

prefixSearchEntity
  :: Language
  -> Int
  -> [Text]
  -> Glean.RepoHaxl u w [ResultLocation Code.Entity]
prefixSearchEntity lang lim toks = case lang of
  Language_Hack ->
    fmap (mapFst Code.Entity_hack) <$> Search.prefixSearch lim toks
  Language_Python ->
    fmap (mapFst Code.Entity_python) <$> Search.prefixSearch lim toks
  lang -> throwM $ ServerException $
    "prefixSearchEntity: language not supported: " <> toShortCode lang
  where mapFst f (x, y, z, a) = (f x, y, z, a)
