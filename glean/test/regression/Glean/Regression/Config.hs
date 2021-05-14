{-# LANGUAGE ApplicativeDo #-}
module Glean.Regression.Config
  ( TestConfig(..)
  , Transform(..)
  , Transforms
  , Generator
  , MakeOuts
  , Driver(..)
  , generate
  , Config(..)
  , options
  , optionsWith
  ) where

import Data.Default
import qualified Options.Applicative as O
import System.Directory

import Glean.Database.Types (Env)
import Glean.Regression.Transform (Transform(..), Transforms)
import qualified Glean.Types as Thrift

-- | Test configuration
data TestConfig = TestConfig
  { testRepoName :: String
  , testRepoHash :: String
  , testOutput :: FilePath
      -- ^ directory in which to store *all* output and temporary files
  , testRoot :: FilePath
      -- ^ directory with test sources
  , testProjectRoot :: FilePath
      -- ^ top-level directory (fbsource/fbcode)
  , testGroup :: String
      -- ^ test group (cf. 'driverGroups' - "" if driver has no groups)
  , testSchemaVersion :: Maybe Int
      -- ^ version of 'all' schema to use in test DB
  } deriving (Show)

-- | Test data generator, for 'driverGenerator' of 'Driver'
type Generator = TestConfig -> Env -> Thrift.Repo -> IO ()

-- | How to make the ".out" files, under 'testOutput' of 'TestConfig',
-- for 'driverMakeOuts' of 'Driver'.
type MakeOuts = TestConfig -> Env -> Thrift.Repo -> IO [FilePath]

-- | Test driver
data Driver = Driver
  { driverGenerator :: Generator
      -- ^ test data generator
  , driverGroups :: [String]
      -- ^ groups - Test will be executed once for each group, with 'testGroup'
      -- set appropriately. If empty, test will be executed once with
      -- 'testGroup' set to "".
  , driverTransforms :: Transforms
      -- ^ Additional query result transformers.
  , driverMakeOuts :: Maybe MakeOuts
      -- ^ How to make ".out" files, defaults to interpreting ".query" files
  }

instance Default Driver where
  def = Driver
    { driverGenerator = \_ _ _ -> return ()
    , driverGroups = []
    , driverTransforms = mempty
    , driverMakeOuts = Nothing
    }

generate :: Generator -> Driver
generate gen = def { driverGenerator = gen }

data Config = Config
  { cfgProjectRoot :: FilePath  -- ^ example: path to fbsource/fcode
  , cfgRoot :: FilePath  -- ^ parent path of all sources, *.query, golden *.out
  , cfgOutput :: Maybe FilePath  -- ^ parent path of *.query results
  , cfgReplace :: Bool   -- ^ when True overwrite golden *.out with query result
  , cfgSchemaVersion :: Maybe Int  -- ^ version of 'all' schema to use
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
      cfgReplace <- O.switch $ O.long "replace"
        <> O.help "Generate (overwrite) golden *.out files instead of testing"
      cfgSchemaVersion <- O.optional $ O.option O.auto $
        O.long "schema-version" <> O.metavar "INT"
      cfgTests <- O.many $ O.strArgument $ O.metavar "TEST"
      return $ resolve Config{..}

    resolve cfg = do
      projectRoot <- if null (cfgProjectRoot cfg)
        then getCurrentDirectory
        else makeAbsolute $ cfgProjectRoot cfg
      root <- makeAbsolute $ cfgRoot cfg
      return $ cfg { cfgProjectRoot = projectRoot, cfgRoot = root }
