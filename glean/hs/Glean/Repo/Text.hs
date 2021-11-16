{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Converting between Repo and String/Text
module Glean.Repo.Text
  ( showRepo
  , showRepoSep
  , repoToText
  , repoToTextSep
  , readRepo
  , parseRepo
  , parseRepoText
  , parseRepoTextSep
  ) where

import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text

import Glean.Types

-- | Show a repo in NAME/HASH format to String
showRepo :: Repo -> String
showRepo = showRepoSep "/"

showRepoSep :: String -> Repo -> String
showRepoSep sep repo =
  Text.unpack (repo_name repo) <> sep <> Text.unpack (repo_hash repo)

-- | Render the Repo in NAME/HASH format to Text
repoToText :: Repo -> Text
repoToText Repo{..} = repo_name <> "/" <> repo_hash

repoToTextSep :: Text -> Repo -> Text
repoToTextSep sep Repo{..} = repo_name <> sep <> repo_hash

readRepo :: String -> String -> Repo
readRepo sep s = fromMaybe (Repo t "") (parseRepoTextSep tsep t)
  where
    tsep = Text.pack sep
    t = Text.pack s

-- | Attempt to parse a repo of the form <name>/<hash> as commonly
-- used by Glean tools.
parseRepo :: String -> Maybe Repo
parseRepo = parseRepoTextSep "/" . Text.pack

-- | Attempt to parse a repo of the form <name>/<hash> as commonly
-- used by Glean tools.
parseRepoText :: Text -> Maybe Repo
parseRepoText = parseRepoTextSep "/"

-- | Parse a repository spec with the given separator. Everything before the
-- *last* separator is the repo name, everything after is the hash. Yields
-- @Nothing@ if the separator doesn't occur in the spec.
--
-- > parseRepo "." "a.b.c.d" == Just (Repo "a.b.c" "d")
-- > parseRepo "/" "a.b.c.d" == Nothing
parseRepoTextSep
  :: Text  -- ^ separator
  -> Text  -- ^ spec
  -> Maybe Repo
parseRepoTextSep sep s = case Text.breakOnEnd sep s of
  ("", _) -> Nothing
  (pfx, sfx) -> Just Repo
    { repo_name = Text.dropEnd (Text.length sep) pfx
    , repo_hash = sfx }
