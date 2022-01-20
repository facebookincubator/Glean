-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

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
  , cfgReplace :: Bool
    -- ^ when True overwrite golden *.out with query result
  , cfgSchemaVersion :: Maybe Int
    -- ^ version of 'all' schema to use
  , cfgTests :: [String]
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
      cfgReplace <- O.switch $ O.long "replace" <>
        O.help "Generate (overwrite) golden *.out files instead of testing"
      cfgSchemaVersion <- O.optional $ O.option O.auto $
        O.long "schema-version" <> O.metavar "INT"
      cfgTests <- O.many $ O.strOption $
        O.long "only" <> O.metavar "DIR" <>
        O.help "Run tests from DIR only"
      return $ resolve Config{..}

    resolve cfg = do
      projectRoot <- if null (cfgProjectRoot cfg)
        then getCurrentDirectory
        else makeAbsolute $ cfgProjectRoot cfg
      root <- makeAbsolute $ cfgRoot cfg
      return $ cfg { cfgProjectRoot = projectRoot, cfgRoot = root }
