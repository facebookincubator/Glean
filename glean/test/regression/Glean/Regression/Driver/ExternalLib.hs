{-# LANGUAGE ApplicativeDo #-}
-- | Provide support for building 'Generator' for indexing into a Glean repo
-- for tests
module Glean.Regression.Driver.ExternalLib
  ( Ext(..)
  , extOptions
  , execExternal
  )
where

import Data.Char(isAlphaNum)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split (wordsBy)
import qualified Options.Applicative as O
import System.Process

import Glean.Database.Index (closeDatabase)
import Glean.Database.Types (envRoot)
import Glean.Regression.Config

-- | Collect partially decoded parameters from @glean_regression_style_test@
-- for the indexer subprocess that will populate the Glean repo.
data Ext = Ext
  { extBinary :: FilePath
  , extArgs :: [String]
  , extTransforms :: [String]
  , extGroups :: [String]
  }

-- | Expects parameters from buck rule @glean_regression_style_test@ defined in
--
-- > fbcode/glean/test/regression/regression_test.bzl
extOptions :: O.Parser Ext
extOptions = do
  extBinary <- O.strOption $ O.long "binary" <> O.metavar "PATH"
  extArgs <- fmap concat $ O.many $
    fmap (:[]) (O.strOption $ O.long "arg" <> O.metavar "ARG")
    O.<|>
    fmap words (O.strOption $ O.long "args" <> O.metavar "ARGS")
  extTransforms <- list <$> O.strOption
    (O.long "transforms" <> O.metavar "LIST" <> O.value "")
  extGroups <- list <$> O.strOption
    (O.long "groups" <> O.metavar "LIST" <> O.value "")
  return Ext{..}
  where
    list = wordsBy (==',')

-- | Finish decoding parameters from @glean_regression_style_test@ by
-- performing substitutions on @TEST_*@ variables, and return the
-- 'Generator' for @'Driver'{'driverGenerator'}@
execExternal :: Ext -> Generator
execExternal Ext{..} TestConfig{..} env repo = do
  closeDatabase env repo
  let vars = HashMap.fromList
        [ ("TEST_DB_ROOT", envRoot env)
        , ("TEST_REPO", testRepoName ++ '/' : testRepoHash)
        , ("TEST_REPO_NAME", testRepoName)
        , ("TEST_REPO_HASH", testRepoHash)
        , ("TEST_ROOT", testRoot)
        , ("TEST_PROJECT_ROOT", testProjectRoot)
        , ("TEST_OUTPUT", testOutput)
        , ("TEST_GROUP", testGroup) ]

      args = map (subst vars) extArgs
  callProcess extBinary args
  where
    subst vars ('$':'{':s)
      | (var,'}':rest) <- break (=='}') s
      , all (\c -> isAlphaNum c || c == '_') var =
          HashMap.lookupDefault ("${" <> var <> "}") var vars ++ subst vars rest
    subst vars (c:s) = c : subst vars s
    subst _ "" = ""
