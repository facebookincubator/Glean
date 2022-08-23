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
  , searchSymbolId
  , resultToDecl
  , CodeEntityLocation(..)
  ) where

import Data.Text (Text, intercalate)
import Util.Text (textShow)

import Data.Typeable (Typeable)

import qualified Glean
import Glean.Typed.Binary (Type)

import Glean.Angle ( Angle )
import Glean.Haxl.Repos as Glean

import qualified Glean.Schema.Src.Types as Src
import qualified Glean.Schema.CodemarkupTypes.Types as Code
import qualified Glean.Schema.Code.Types as Code

import Glean.Glass.Utils ( searchRecursiveWithLimit )

-- Search-based inverse of Symbol.toSymbol :: a -> [Text]
-- Decodes a symbol id to a code.Entity fact
--
-- Note: this is different to e.g. approximate string search, as we
-- should _always_ be able to decode valid symbol ids back to their (unique*)
-- entity.
--
-- There are cases where symbol ids are not unique:
-- - weird code
-- - hack namespaces
-- - bugs/approximations in our encoder
--
-- We log the duplicates to glass_errors
--
class Search t where
  symbolSearch :: [Text] -> ReposHaxl u w (SearchResult t)

-- | We have zero, one or multiple matches for entities
data SearchResult t
  = None !Text -- no result found
  | One (SearchEntity t) -- preicsely one entity, yay.
  | Many -- oh dear, several
     { initial :: SearchEntity t
     , rest :: [SearchEntity t]
     , message :: !Text }

data SearchEntity t =
  SearchEntity {
    entityRepo :: !Glean.Repo, -- vital to know which repo this came from
    decl :: !t,
    file :: !Src.File,
    rangespan :: !Code.RangeSpan,
    name :: !Text
  }

-- | Summary form used for describe()
data CodeEntityLocation =
  CodeEntityLocation {
    entity :: !Code.Entity,
    entityFile :: !Src.File,
    entityRange :: !Code.RangeSpan,
    entityName :: !Text
  }

-- Similar to SearchEntity , used in Angle data queries. Searches return this
type ResultLocation t = (t, Src.File, Code.RangeSpan, Text)

resultToDecl :: [(d, a, b)] -> [d]
resultToDecl = map (\(x, _, _) -> x)

-- We tend to map over the decl building up layers
instance Functor SearchResult where
  fmap _ (None t) = None t
  fmap f (One e) = One (e { decl = f (decl e) })
  fmap f m@Many { initial = e, rest = es } =
    m { initial = e { decl = f (decl e)}
      , rest = map (\e -> e { decl = f (decl e) }) es }

-- | Find matching code.Entity values by repo and language for the
-- symbol id tokens.
--
-- symbol ids are "mostly" unique. This code checks explicitly if the
-- symbol id search generated 0, 1 or >1 result. We always return the first
-- match.
--
-- There are some scenarios where we might want to return all matches.
--
searchSymbolId :: (Typeable t, Show t, Glean.Typed.Binary.Type t)
  => [Text]
  -> Angle (ResultLocation t)
  -> ReposHaxl u w (SearchResult t)
searchSymbolId toks query = do
  results <- Glean.queryAllRepos $ do
    repo <- Glean.haxlRepo
    results <- searchRecursiveWithLimit (Just max_symbolid_matches) query
    return $ map (repo,) results
  let toksText = intercalate "/" toks
  return $ case results of
    [] -> None $ "runSearch: No results found for " <> toksText
    [(entityRepo, (decl, file, rangespan, name))] -> One SearchEntity{..}
    (firstResult:moreResults) ->
      Many { initial = uncurry mkSearchEntity firstResult
           , rest = map (uncurry mkSearchEntity) moreResults
           , message = "runSearch: " <> textShow (length results) <>
            " results found for " <> toksText
      }
  where
    mkSearchEntity entityRepo (decl, file, rangespan, name) = SearchEntity{..}

    -- symbol ids do have collisions, but they should be rare. If they are
    -- expensive it means we have a bad query from symbold id to entity
    max_symbolid_matches = 10

-- | Search for entities based on the prefix of their symbol ids
class PrefixSearch t where
  prefixSearch :: Int -> [Text] -> RepoHaxl u w [ResultLocation t]
