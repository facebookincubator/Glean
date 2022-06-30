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

import qualified Options.Applicative as O
import System.Directory

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
  }

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
      cfgSchemaVersion <- O.optional $ O.option O.auto $
        O.long "schema-version" <> O.metavar "INT" <>
        O.help "version of 'all' schema to use for unversioned queries"
      cfgTests <- O.many $ O.strOption $
        O.long "only" <> O.metavar "DIR" <>
        O.help "Run tests from DIR only"
      cfgOmitTests <- O.many $ O.strOption $
        O.long "omit" <> O.metavar "DIR" <>
        O.help "Do not run tests from DIR"
      return $ resolve Config{..}

    resolve cfg = do
      projectRoot <- if null (cfgProjectRoot cfg)
        then getCurrentDirectory
        else makeAbsolute $ cfgProjectRoot cfg
      -- We do `equalFilePath root` in the code that searches for test cases,
      -- which requires `canonicalizePath` for correctness
      root <- canonicalizePath $ cfgRoot cfg
      replace <- mapM makeAbsolute $ cfgReplace cfg
      return $ cfg {
        cfgProjectRoot = projectRoot,
        cfgRoot = root,
        cfgReplace = replace
      }
