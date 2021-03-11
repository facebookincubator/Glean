-- Copyright 2004-present Facebook. All Rights Reserved.
module ExternalControllerTest (main) where

import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Glean.Database.Test
  ( withTestEnv
  , kickOffTestDB
  , waitUntilComplete
  , setRecipes
  , Setting
  )
import Glean.Database.Types (Env(..))
import Glean.Database.Work (workFinished)
import Glean.Database.Work.Controller
import Glean.Init (withUnitTest)
import Glean.Recipes.Types
  ( Recipe(..)
  , Executor(..)
  )
import Glean.Types
  ( Repo(..)
  , ParcelState
  )
import System.IO
import System.IO.Temp
import Test.HUnit
import TestDB (dbTestCaseWritable)
import TestRunner
import Util.Defer

repo :: Repo
repo =
  Repo { repo_name = "glean-binarycontroller-test", repo_hash = "faceb00c" }

recipesWithSettings :: Map Text Text -> Setting
recipesWithSettings settings =
  setRecipes $ Map.fromList [(repo_name repo, recipes)]
  where
    recipes =
      Map.fromList
        [ ( repo_name repo
          , def
              { recipe_parcels = 1
              , recipe_executor = Executor_External
              , recipe_settings = settings
              }
          )
        ]

runBinaryControllerTest :: Map Text Text -> IO a -> IO a
runBinaryControllerTest settings action =
  withTestEnv [recipesWithSettings settings] $ \env -> do
    kickOffTestDB env repo id
    waitUntilComplete env repo
    action

spawnBinaryController :: Env -> Repo -> Map Text Text -> IO (Vector ParcelState)
spawnBinaryController env repo settings =
  immediately $ controller scheduleTask Context
    { ctxEnv = env
    , ctxRepo = repo
    , ctxTask = ""
    , ctxRecipe = def
        { recipe_parcels = 1
        , recipe_executor = Executor_External
        , recipe_settings = settings
        }
    , ctxWorkFinished = workFinished
    }

echo :: Test
echo = TestLabel "run echo" $ dbTestCaseWritable $ \env repo -> do
  ret <- spawnBinaryController env repo $
    Map.fromList [("command", "/bin/echo hello")]
  assertEqual "One parcel is scheduled" 1 $ Vector.length ret

writeToFile :: Test
writeToFile =
  TestLabel "write to a file"
    $ TestCase
    $ withSystemTempFile "glean-binarycontroller-test"
    $ \filePath fileHandle -> do
      let recipes = Map.fromList
              [("command", "/bin/echo -n hello > " <> Text.pack filePath)]
      runBinaryControllerTest recipes $ do
        actual <- hGetContents fileHandle
        assertEqual "hello is written to the file" "hello" actual

substitutions :: Test
substitutions =
  TestLabel "Substitutions are applied"
    $ TestCase
    $ withSystemTempFile "glean-binarycontroller-test"
    $ \filePath fileHandle -> do
      let recipes = Map.fromList
            [ ( "command"
              , "/bin/echo -n x${somevar}x ${REPO_NAME} > " <> Text.pack filePath
              )
            , ("somevar", "hello")
            ]
      runBinaryControllerTest recipes $ do
        actual <- hGetContents fileHandle
        let expected = Text.unpack $ "xhellox " <> repo_name repo
        assertEqual "substitutions are applied" expected actual

main :: IO ()
main =
  withUnitTest
    $ testRunner
    $ TestList [echo, writeToFile, substitutions]
