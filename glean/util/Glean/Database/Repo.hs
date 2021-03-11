module Glean.Database.Repo
  ( Repo(..)
  , showRepo
  , readRepo
  , showtRepo
  , parseRepo
  , databaseSubdir
  , databasePath
  , inRepo
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath

import Glean.Types (Repo(..))

showRepo :: String -> Repo -> String
showRepo sep repo = concat
  [Text.unpack $ repo_name repo
  ,sep
  ,Text.unpack $ repo_hash repo
  ]

readRepo :: String -> String -> Repo
readRepo sep s = fromMaybe (Repo t "") (parseRepo tsep t)
  where
    tsep = Text.pack sep
    t = Text.pack s

-- | Parse a repository spec with the given separator. Everything before the
-- *last* separator is the repo name, everything after is the hash. Yields
-- @Nothing@ if the separator doesn't occur in the spec.
--
-- > parseRepo "." "a.b.c.d" == Just (Repo "a.b.c" "d")
-- > parseRepo "/" "a.b.c.d" == Nothing
parseRepo
  :: Text  -- ^ separator
  -> Text  -- ^ spec
  -> Maybe Repo
parseRepo sep s = case Text.breakOnEnd sep s of
  ("", _) -> Nothing
  (pfx, sfx) -> Just Repo
    { repo_name = Text.dropEnd (Text.length sep) pfx
    , repo_hash = sfx }

showtRepo :: Text -> Repo -> Text
showtRepo sep Repo{..} = repo_name <> sep <> repo_hash

databaseSubdir :: Repo -> String
databaseSubdir = showRepo "/"

inRepo :: Repo -> String -> String
inRepo repo s = databaseSubdir repo ++ ": " ++ s

databasePath :: FilePath -> Repo -> FilePath
databasePath root repo = root </> databaseSubdir repo
