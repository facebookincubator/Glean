module Glean.Database.Repo
  ( databaseSubdir
  , databasePath
  , inRepo
  ) where

import System.FilePath

import Glean.Repo.Text
import Glean.Types (Repo(..))

databaseSubdir :: Repo -> String
databaseSubdir = showRepoSep "/"

inRepo :: Repo -> String -> String
inRepo repo s = databaseSubdir repo ++ ": " ++ s

databasePath :: FilePath -> Repo -> FilePath
databasePath root repo = root </> databaseSubdir repo
