{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Work.Controller
  ( Context(..)
  , FailedTaskError(..)
  , Controller(..)
  , controller
  ) where

import Control.Exception
import Control.Monad
import Data.Char (isAlphaNum)
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import System.Process (callCommand)
import TextShow

import Util.Control.Exception (handleAll)
import Util.Defer
import Util.STM

import Glean.Database.Tailer
import Glean.Database.Types
import Glean.Database.Work.Queue
import Glean.Recipes.Types
import Glean.Types as Thrift hiding(Exception)
import Glean.Util.Warden (spawnMask)

-- | A work context
data Context = Context
  { ctxEnv :: Env
  , ctxRepo :: Repo
  , ctxTask :: Text
  , ctxRecipe :: Recipe
  , ctxWorkFinished :: Env -> Thrift.WorkFinished -> IO ()
  }

-- | A 'Controller' knows how to schedule and resume tasks.
data Controller = Controller
  { scheduleTask :: Context -> Defer IO STM (Vector ParcelState)
      -- ^ schedule all parcels in a task
  , resumeTask :: Context -> Vector ParcelState -> Defer IO STM ()
      -- ^ resume work in a fresh Env
  }

data FailedTaskError = FailedTaskError
  { failedTaskRepo :: Repo
  , failedTaskName :: Text
  , failedTaskParcel :: Maybe Int
  , failedTaskError :: Text
  }
  deriving(Show,Typeable)

instance Exception FailedTaskError

-- | Obtain a 'Controller' function for the given 'Context'
controller :: (Controller -> Context -> a) -> Context -> a
controller f ctx = f (getController $ ctxRecipe ctx) ctx

getController :: Recipe -> Controller
getController Recipe{..} = case recipe_executor of
  Executor_WorkQueue -> workQueueController
  Executor_External
    | Map.member "category" recipe_settings -> tailerController
    | otherwise -> externalBinaryController
  Executor_Manual -> manualController
  _ -> failController

workQueueController :: Controller
workQueueController = Controller
  { scheduleTask = \Context{..} -> do
      let n = fromIntegral $ recipe_parcels ctxRecipe
      lift $ insertWorkQueue (envWorkQueue ctxEnv) ctxRepo ctxTask n
      return $ Vector.replicate n $ ParcelState_waiting $ ParcelWaiting 0
  , resumeTask = \Context{..} parcels -> flip Vector.imapM_ parcels $
      \i parcel -> case parcel of
          ParcelState_waiting{} -> lift $ writeWorkQueue
            (envWorkQueue ctxEnv)
            Parcel
              { parcelRepo = ctxRepo
              , parcelTask = ctxTask
              , parcelIndex = i
              }
          _ -> return ()
  }

tailerController :: Controller
tailerController = Controller
  { scheduleTask = \ctx -> do
      let handle = fromMaybe ""
            $ Map.lookup "handle"
            $ recipe_settings
            $ ctxRecipe ctx
      start ctx mempty
      return $ Vector.singleton $ ParcelState_running ParcelRunning
        { parcelRunning_retries = 0
        , parcelRunning_handle = handle
        , parcelRunning_runner = "tailer"
        , parcelRunning_progress = mempty
        }
  , resumeTask = \ctx parcels -> forM_ parcels $ \parcel -> case parcel of
      ParcelState_running ParcelRunning{..} -> start ctx parcelRunning_progress
      _ -> return ()
  }
  where
    start Context{..} =
      later . startTailer ctxEnv ctxRepo ctxTask (recipe_settings ctxRecipe)

externalBinaryController :: Controller
externalBinaryController = Controller
  { scheduleTask = \ctx@Context{..} -> let
      indices = Vector.enumFromN 0 $ fromIntegral $ recipe_parcels ctxRecipe
    in forM indices $ \idx -> startParcel idx ctx
  , resumeTask = \ctx parcels -> flip Vector.imapM_ parcels $ \idx parcel ->
    case parcel of
      ParcelState_running ParcelRunning{} -> startParcel idx ctx
      x -> return x
  }
  where
    startParcel idx ctx@Context{..} = do
      let handl = ""
      case Map.lookup "command" $ recipe_settings ctxRecipe of
        Nothing -> now $ throwSTM FailedTaskError
          { failedTaskError = "No command for external executor"
          , failedTaskRepo = ctxRepo
          , failedTaskName = ctxTask
          , failedTaskParcel = Just idx
          }
        Just command -> do
          later $ void $ spawnMask (envWarden ctxEnv) $ \_ -> do
            let workFinished out = ctxWorkFinished ctxEnv $ Thrift.WorkFinished
                  { workFinished_work = Thrift.Work
                    { work_repo = ctxRepo
                    , work_task = ctxTask
                    , work_parcelIndex = fromIntegral idx
                    , work_parcelCount = recipe_parcels ctxRecipe
                    , work_handle = handl
                    }
                  , workFinished_outcome = out
                  }
            handleAll
              (\e -> workFinished $
                Thrift.Outcome_failure $ Thrift.Failure $ showt e) $
              do
                let subs = Map.union (builtins ctx) $ recipe_settings ctxRecipe
                callCommand $ substitute subs $ Text.unpack command
                workFinished $ Thrift.Outcome_success Thrift.Success
          return $ ParcelState_running ParcelRunning
            { parcelRunning_retries = 0
            , parcelRunning_runner = "binary"
            , parcelRunning_handle = handl
            , parcelRunning_progress = mempty
            }

    substitute settings ('$':'{':str)
      | (setting, '}':rest) <- break (== '}') str
      , all (\c -> isAlphaNum c || c == '_') setting = let
          value = maybe ("${" <> setting <> "}") Text.unpack $
            Map.lookup (Text.pack setting) settings
        in value ++ substitute settings rest
    substitute settings (c:str) = c : substitute settings str
    substitute _ "" = ""

    builtins Context{..} = Map.fromList
      [ ("REPO_NAME", repo_name ctxRepo)
      , ("REPO_HASH", repo_hash ctxRepo)
      ]

manualController :: Controller
manualController = Controller
  { scheduleTask = \Context{..} -> return $ Vector.singleton $
      ParcelState_running def
        { parcelRunning_retries = 0
        , parcelRunning_handle =
            Map.findWithDefault "" "handle" $ recipe_settings ctxRecipe
        , parcelRunning_runner = "manual"
        }
  , resumeTask = \_ _ -> return ()
  }

failController :: Controller
failController = Controller
  { scheduleTask = failure
  , resumeTask = const . failure
  }
  where
    failure Context{..} = now $ throwSTM FailedTaskError
      { failedTaskRepo = ctxRepo
      , failedTaskName = ctxTask
      , failedTaskParcel = Nothing
      , failedTaskError = "unknown worker '" <> recipe_worker ctxRecipe <> "'"
      }
