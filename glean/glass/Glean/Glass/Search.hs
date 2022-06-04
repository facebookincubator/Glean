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
      ResultLocation)
import qualified Glean.Glass.Search.Cxx ({- instances -})
import qualified Glean.Glass.Search.Flow ({- instances -})
import qualified Glean.Glass.Search.Hack ({- instances -})
import qualified Glean.Glass.Search.Haskell ({- instances -})
import qualified Glean.Glass.Search.Python ({- instances -})
import qualified Glean.Glass.Search.Erlang ({- instances -})
import qualified Glean.Glass.Search.LSIF ({- instances -})
import Glean.Glass.Types (ServerException(ServerException))
import qualified Glean.Schema.Code.Types as Code
import qualified Glean.Haxl.Repos as Glean

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
  -- limited set via lsif
  Language_Go -> fmap Code.Entity_lsif <$> Search.symbolSearch toks
  Language_TypeScript -> fmap Code.Entity_lsif <$> Search.symbolSearch toks
  Language_Rust -> fmap Code.Entity_lsif <$> Search.symbolSearch toks
  Language_Java -> fmap Code.Entity_lsif <$> Search.symbolSearch toks
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
  where mapFst f (x, y, z) = (f x, y, z)
