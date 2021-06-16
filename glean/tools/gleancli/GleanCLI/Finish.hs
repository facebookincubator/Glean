{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Finish (FinishCommand, finished) where

import Control.Exception
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Options.Applicative

import Util.IO
import Util.OptParse

import GleanCLI.Common
import GleanCLI.Types

import Glean
import Glean.Types as Thrift

data FinishCommand
  = Finish
      { finishRepo :: Repo
      , finishHandle :: Text
      , task :: Maybe Text
      , parcel :: Maybe Int
      , failure :: Maybe Text
      }

instance Plugin FinishCommand where
  parseCommand =
    commandParser "finish"
      (progDesc "Notify server that a database is complete") $ do
      finishRepo <- repoOpts
      task <- optional $ textOption
        (  long "task"
        <> metavar "NAME"
        <> internal
        )
      parcel <- optional $ option auto
        (  long "parcel"
        <> metavar "NUMBER"
        <> internal
        )
      failure <- optional $ textOption
        (  long "error"
        <> metavar "MESSAGE"
        )
      finishHandle <- handleOpt
      return Finish{..}

  runCommand backend Finish{..} =
    finished backend finishRepo finishHandle task parcel failure

finished
  :: Glean.Backend b
  => b
  -> Repo
  -> Text -- ^ handle
  -> Maybe Text -- ^ task
  -> Maybe Int -- ^ parcel
  -> Maybe Text -- ^ failure?
  -> IO ()
finished backend repo handle task parcel failure = do
  Glean.workFinished backend WorkFinished
    { workFinished_work = def
      { work_repo = repo
      , work_task = fromMaybe "" task
      , work_parcelIndex = maybe 0 fromIntegral parcel
      , work_handle = handle
      }
    , workFinished_outcome = case failure of
        Nothing -> Outcome_success def
        Just msg -> Outcome_failure (Thrift.Failure msg)
    }
    `catch` \e@Retry{} ->
       die 1 $
         "finish: " <> show e <> "\n" <>
         "  This error indicates that previous write or derive\n" <>
         "  operations have not completed yet. Please ensure that\n" <>
         "  all writing operations have completed before invoking\n" <>
         "  'glean finish'"
