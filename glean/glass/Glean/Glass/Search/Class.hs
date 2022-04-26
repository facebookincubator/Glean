{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module Glean.Glass.Search.Class
  ( Search(..)
  , SearchResult(..)
  , SearchEntity(..)
  , PrefixSearch(..)
  , ResultLocation
  , runSearch
  , resultToDecl
  ) where

import Data.Text (Text, intercalate)
import Util.Text (textShow)

import Data.Typeable (Typeable)

import qualified Glean
import Glean.Typed.Binary (Type)

import Glean.Angle ( Angle )
import qualified Glean.Angle as Angle
import Glean.Haxl.Repos as Glean

import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code

-- Search-based inverse of Symbol.toSymbol :: a -> [Text]
class Search t where
  symbolSearch :: [Text] -> ReposHaxl u w (SearchResult t)

-- | We have zero, one or multiple matches for entities
data SearchResult t
  = None Text
  | One (SearchEntity t)
  | Many (SearchEntity t) Text

data SearchEntity t =
  SearchEntity {
    entityRepo :: Glean.Repo,
    decl :: t,
    file :: Src.File,
    rangespan :: Code.RangeSpan
  }

-- Synonym for SearchEntity, used in Angle data queries
type ResultLocation t = (t, Src.File, Code.RangeSpan)

resultToDecl :: [(d, a, b)] -> [d]
resultToDecl = map (\(x, _, _) -> x)

-- We tend to map over the decl building up layers
instance Functor SearchResult where
  fmap _ (None t) = None t
  fmap f (One e) = One (e { decl = f (decl e) })
  fmap f (Many e t) = Many (e { decl = f (decl e) }) t

-- | In Haxl, run a search that returns results for an object and its location
runSearch :: (Typeable t, Show t, Glean.Typed.Binary.Type t)
  => [Text]
  -> Angle (ResultLocation t)
  -> ReposHaxl u w (SearchResult t)
runSearch toks query = do
  results <- Glean.queryAllRepos $ do
    repo <- Glean.haxlRepo
    results <- Glean.search_ $ Angle.query query
    return $ map (repo,) results
  let toksText = intercalate "/" toks
  return $ case results of
    [] -> None $ "runSearch: No results found for " <> toksText
    [(entityRepo, (decl, file, rangespan))] -> One SearchEntity{..}
    ((entityRepo, (decl, file, rangespan)):_) -> Many SearchEntity{..}
          ("runSearch: " <> textShow (length results) <>
            " results found for " <> toksText)

-- | Search for entities based on the prefix of their symbol ids
class PrefixSearch t where
  prefixSearch :: Int -> [Text] -> RepoHaxl u w [t]
