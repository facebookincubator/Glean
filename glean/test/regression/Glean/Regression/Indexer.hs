-- (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

module Glean.Regression.Indexer
  ( Indexer(..)
  , RunIndexer
  , indexerWithNoOptions
  , indexerThen
  , withTestDatabase
  , withTestEnvDatabase
  ) where

import Options.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import System.Exit
import System.FilePath

import Glean (fillDatabase)
import Glean.Backend (Backend)
import qualified Glean.Backend as Backend
import Glean.Database.Test
import Glean.Database.Types (Env)
import Glean.Regression.Config
import Glean.Types
import Glean.Util.Some


-- | An 'Indexer' tells the test framework how to index some source
-- code.
--
-- Indexers have command-line options, and a function to index the
-- code specified by the 'TestConfig' using a given 'Env' as a
-- backend.
--
data Indexer opts = Indexer
  { indexerOptParser :: Parser opts
  , indexerRun :: opts -> TestConfig -> Env -> IO ()
  }

-- | An indexer composed of two separate indexing tasks. The left
-- indexer is run before the right indexer.  This is useful when we
-- have a base indexer and we want to add a deriving pass, for
-- example.
--
-- The right indexer doesn't have any options. It's possible to
-- generalise this, but this more restricted form was found to be more
-- convenient when used with 'Driver'.
indexerThen :: Indexer a -> RunIndexer -> Indexer a
indexerThen (Indexer parseOpts run1) run2 = Indexer
  { indexerOptParser = parseOpts
  , indexerRun = \opts test env -> run1 opts test env >> run2 test env
  }

type RunIndexer = TestConfig -> Env -> IO ()

indexerWithNoOptions :: RunIndexer -> Indexer ()
indexerWithNoOptions run = Indexer
  { indexerOptParser = pure ()
  , indexerRun = \() -> run
  }

-- | Run the supplied Indexer to populate a temporary DB
withTestDatabase
  :: RunIndexer
  -> TestConfig
  -> ((Some Backend, Repo) -> IO a)
  -> IO a
withTestDatabase gen cfg f =
  withTestEnvDatabase gen cfg (\env repo -> f (Some env, repo))

-- | Like 'withTestDatabase', but requires an 'Env' rather than an
-- arbitrary 'Backend'.
withTestEnvDatabase
  :: RunIndexer
  -> TestConfig
  -> (Env -> Repo -> IO a)
  -> IO a
withTestEnvDatabase indexer test action =
  withTestEnv settings $ \backend -> do
  let
    repo = testRepo test
    setDbVersion ver = void $ Backend.updateProperties backend repo
      (HashMap.fromList
        [("glean.schema_version", Text.pack (show ver))
        ]) []
  fillDatabase backend repo "" (die "repo already exists") $ do
    mapM_ setDbVersion $ testSchemaVersion test
    indexer test backend
  action backend (testRepo test)
  where
    settings =
        [ setRoot $ testOutput test </> "db"
        , setSchemaEnableEvolves True ]
