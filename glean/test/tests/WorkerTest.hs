-- Copyright (c) Facebook, Inc. and its affiliates.

module WorkerTest (main) where

import Control.Concurrent.STM (atomically)
import Control.Monad
import Data.Default
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import TestRunner
import Util.Testing

import qualified Glean.Database.Catalog as Catalog
import Glean.Database.Index (kickOffDatabase)
import Glean.Database.Storage.Memory (newStorage)
import Glean.Database.Test (withTestEnv, setRecipes)
import Glean.Database.Types (Env(..))
import Glean.Database.Work
import Glean.Init
import Glean.Recipes.Types (Recipe(..), Recipes)
import Glean.Types (Repo(..))
import qualified Glean.Types as Thrift

type Index = Int

-- | An action for each step of the trace
data Action
  = Get Text Index
  | Cancel Index
  | Succeed Index
  | Fail Index
  deriving(Show)

-- | A trace is a sequence of actions and the DB status we expect after
-- executing that sequence
data Trace = Trace [Action] Thrift.DatabaseStatus
  deriving(Show)

data TaskState
  = Waiting [Text] Int
  | Running
      Int -- number of shards in queue
      IntSet -- running shards
  | Finished

data State
  = Complete
  | Broken
  | Incomplete (HashMap Text TaskState)

fbsource_recipes :: Map Text Recipe
fbsource_recipes = Map.fromList
  [ ("targets.fbcode", def
      { recipe_parcels = 1
      , recipe_dependencies = mempty
      , recipe_retries = 0
      })
  , ("index.fbcode", def
      { recipe_parcels = 4
      , recipe_dependencies = Set.fromList ["targets.fbcode"]
      , recipe_retries = 0
      })
  , ("targets.fbobjc", def
      { recipe_parcels = 1
      , recipe_dependencies = mempty
      , recipe_retries = 0
      })
  , ("index.fbobjc", def
      { recipe_parcels = 2
      , recipe_dependencies = Set.fromList ["targets.fbobjc"]
      , recipe_retries = 0
      })
  , ("derive", def
      { recipe_parcels = 1
      , recipe_dependencies = Set.fromList ["index.fbcode", "index.fbobjc"]
      , recipe_retries = 0
      })
  ]

initialState :: Recipes -> State
initialState recipes = schedule $ HashMap.fromList
  [ (name, Waiting
      (Set.toList $ recipe_dependencies r)
      (fromIntegral $ recipe_parcels r))
    | (name, r) <- Map.toList recipes ]

schedule :: HashMap Text TaskState -> State
schedule tasks
  | all finished $ HashMap.elems tasks = Complete
  | otherwise = Incomplete $ fmap go tasks
  where
    go (Waiting ts n)
      | all finished $ mapMaybe (`HashMap.lookup` tasks) ts =
          Running n IntSet.empty
    go x = x

    finished Finished = True
    finished _ = False

possibleActions
  :: Index
  -> HashMap Text TaskState
  -> [(Int, Gen (Action, State))]
possibleActions k tasks =
  concatMap taskActions $ HashMap.toList tasks
  where
    taskActions (task, Running queued working) =
      [(freq_Get,
          return
            (Get (taskPat task) k
            ,run task (queued-1) $ IntSet.insert k working))
        | queued /= 0]
      ++
      [(freq_Cancel, do
          i <- elements $ IntSet.toList working
          return
            (Cancel i
            ,run task (queued+1) $ IntSet.delete i working))
        | not $ IntSet.null working]
      ++
      [(freq_Succeed, do
          i <- elements $ IntSet.toList working
          return
            (Succeed i
            ,if queued == 0 && IntSet.size working == 1
              then finish task
              else run task queued $ IntSet.delete i working))
        | not $ IntSet.null working]
      ++
      [(freq_Fail, do
          i <- elements $ IntSet.toList working
          return (Fail i, Broken))
        | not $ IntSet.null working]

    taskActions _ = []

    -- Test regex functionality: if a 3-char prefix still uniquely
    -- identifies the task, then use that.
    taskPat task
      | [_one] <-
          [ task | (task, Running queued _) <- HashMap.toList tasks
          , queued /= 0, pat `Text.isPrefixOf` task ] = pat
      | otherwise = task
      where
      pat = Text.take 3 task

    run task queued working =
      Incomplete $ HashMap.insert task (Running queued working) tasks

    finish task = schedule $ HashMap.insert task Finished tasks

    freq_Get = 10
    freq_Cancel = 2
    freq_Succeed = 10
    freq_Fail = 1

genSizedTrace :: Int -> Index -> [Action] -> State -> Gen Trace
genSizedTrace !_ !_ actions Complete =
  return $ Trace (reverse actions) Thrift.DatabaseStatus_Complete
genSizedTrace _ _ actions Broken =
  return $ Trace (reverse actions) Thrift.DatabaseStatus_Broken
genSizedTrace 0 _ actions (Incomplete _) =
  return $ Trace (reverse actions) Thrift.DatabaseStatus_Incomplete
genSizedTrace n k actions (Incomplete tasks) = do
  (action, state) <- frequency $ possibleActions k tasks
  genSizedTrace (n-1) (k+1) (action:actions) state

genTrace :: State -> Gen Trace
genTrace state = sized $ \n -> genSizedTrace n 0 [] state

execTrace :: Env -> Repo -> Trace -> IO ()
execTrace env repo (Trace actions expected) = do
  foldM_ exec mempty actions
  status <- (Thrift.database_status <=< fmap Thrift.getDatabaseResult_database)
    <$> atomically (Catalog.getLocalDatabase (envCatalog env) repo)
  assertEqual "status" (Just expected) status
  where
    exec ws (Get task index) = do
      Thrift.GetWorkResponse_available
        Thrift.WorkAvailable{workAvailable_work=work}
        <- getWork env def
            { Thrift.getWork_timeout = 0
            , Thrift.getWork_tasks = [task]
            }
      assertEqual "repo" repo (Thrift.work_repo work)
      return $ IntMap.insert index work ws
    exec ws (Cancel index) = do
      Just work <- return $ IntMap.lookup index ws
      workCancelled env Thrift.WorkCancelled
        { workCancelled_work = work
        , workCancelled_reason = "you're cancelled!"
        }
      return $ IntMap.delete index ws
    exec ws (Succeed index) = do
      Just work <- return $ IntMap.lookup index ws
      workFinished env Thrift.WorkFinished
        { workFinished_work = work
        , workFinished_outcome = Thrift.Outcome_success Thrift.Success
        }
      return $ IntMap.delete index ws
    exec ws (Fail index) = do
      Just work <- return $ IntMap.lookup index ws
      workFinished env Thrift.WorkFinished
        { workFinished_work = work
        , workFinished_outcome = Thrift.Outcome_failure $ Thrift.Failure "omg"
        }
      return $ IntMap.delete index ws

prop_workQueue :: Property
prop_workQueue = forAll (genTrace $ initialState fbsource_recipes) $ \trace ->
  monadicIO $ run $
  withTestEnv [setRecipes recipes] $ \Env{..} -> do
  storage <- newStorage
  let env = Env{envStorage = storage, ..}
      repo = Repo "fbsource" "foobar"
  Thrift.KickOffResponse{kickOffResponse_alreadyExists=False}
    <- kickOffDatabase env def{ Thrift.kickOff_repo = repo }
  execTrace env repo trace
  where
    recipes = Map.fromList [("fbsource", fbsource_recipes)]

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "workQueue" $ TestCase $ assertProperty "workQueue"
      prop_workQueue
  ]
