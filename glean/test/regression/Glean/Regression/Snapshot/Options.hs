{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Regression.Snapshot.Options
  ( Config(..)
  , options
  , optionsWith
  ) where

import Control.Monad (unless)
import qualified Options.Applicative as O
import System.Directory
import System.FilePath
import System.Process (readProcess)

data Config = Config
  { cfgProjectRoot :: FilePath
    -- ^ path to root of source tree
  , cfgRoot :: FilePath
    -- ^ parent path of all sources, *.query, golden *.out
  , cfgOutput :: Maybe FilePath
    -- ^ parent path of *.query results
  , cfgReplace :: Maybe FilePath
    -- ^ when True overwrite golden *.out with query result
  , cfgSchemaVersion :: Maybe Int
    -- ^ version of 'all' schema to use
  , cfgTests :: [String]
    -- ^ specific directories of tests we want to run (ignoring other
    --   directories)
  , cfgOmitTests :: [String]
    -- ^ specific directories of tests we don't want to run
  } deriving Show

options :: O.ParserInfo (IO Config)
options = fst <$> optionsWith (pure ())

optionsWith :: O.Parser a -> O.ParserInfo (IO Config,a)
optionsWith other = O.info (O.helper <*> ((,) <$> parser <*> other)) O.fullDesc
  where
    parser = do
      cfgProjectRoot <- O.strOption
        $ O.long "project-root" <> O.metavar "PATH" <> O.value ""
      cfgRoot <- O.strOption $ O.long "root" <> O.metavar "PATH" <> O.value ""
      cfgOutput <- O.optional $ O.strOption $
        O.short 'o' <> O.long "output" <> O.metavar "PATH"
      cfgReplace <- O.optional $ O.strOption $
        O.long "replace" <> O.metavar "PATH" <>
        O.help "Generate (overwrite) golden *.out files instead of testing"
      replaceAll <- O.switch $
        O.long "replace-all" <>
        O.help "Generate (overwrite) all golden *.out files instead of testing"
      cfgSchemaVersion <- O.optional $ O.option O.auto $
        O.long "schema-version" <> O.metavar "INT" <>
        O.help "version of 'all' schema to use for unversioned queries"
      cfgTests <- O.many $ O.strOption $
        O.long "only" <> O.metavar "DIR" <>
        O.help "Run tests from DIR only"
      cfgOmitTests <- O.many $ O.strOption $
        O.long "omit" <> O.metavar "DIR" <>
        O.help "Do not run tests from DIR"
      return $ resolve replaceAll Config{..}

    resolve replaceAll cfg@Config{..} = do
      projectRoot <- if null cfgProjectRoot
        then getCurrentDirectory
        else makeAbsolute cfgProjectRoot
      -- We do `equalFilePath root` in the code that searches for test cases,
      -- which requires `canonicalizePath` for correctness
      root <- canonicalizePath cfgRoot
      replace <-
        if replaceAll
        then do
          src <- sourcePath root
          putStrLn src
          return (Just src)
        else mapM makeAbsolute cfgReplace
      return $ cfg {
        cfgProjectRoot = projectRoot,
        cfgRoot = root,
        cfgReplace = replace
      }

-- | Simple heuristics to get the path of the source files
-- from the buck-out path. Will take a path like
--
--   /data/users/unixname/fbsource/buck-out/v2/gen/fbcode/778788177e01483d/glean/lang/codemarkup/tests/python/__sources__/sources/
--
--  and transform it into
--
--   /data/users/unixname/fbsource/fbcode/glean/lang/codemarkup/tests/python/
--
sourcePath :: FilePath -> IO FilePath
sourcePath path = do
  root <- head . lines <$> readProcess "buck" ["root"] ""
  let
    sourcePath = root </> path'
    path' = joinPath
      $ takeWhile (/= "__sources__") -- drop __sources__/sources
      $ tail -- drop hash
        -- drop buck-out bit
      $ go (splitDirectories root) (splitDirectories path)
  exists <- doesDirectoryExist sourcePath
  putStrLn sourcePath
  unless exists $
    error "unable to find tests path. Specify it with --replace PATH"
  return sourcePath
  where
    go [] ys = ys
    go _  [] = error "test path is not a subdirectory of root"
    go (x:xs) (y:ys) =
      if x == y
      then go xs ys
      else go (x:xs) ys

