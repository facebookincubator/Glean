-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Finish (FinishCommand, finished) where

import Control.Exception
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Options.Applicative

import Util.IO
import Util.OptParse

import GleanCLI.Common
import GleanCLI.Types

import Glean
import Glean.LocalOrRemote
import Glean.Types as Thrift

data FinishCommand
  = Finish
      { finishRepo :: Repo
      , finishHandle :: Text
      , task :: Maybe Text
      , parcel :: Maybe Int
      , failure :: Maybe Text
      , allowZeroFacts :: Bool
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
      allowZeroFacts <- switch
        (  long "allow-zero-facts"
        <> help "Allow creating a db without any facts"
        )
      return Finish{..}

  runCommand _ _ backend Finish{..} = do
    when (not allowZeroFacts) $ do
      stats <- Glean.predicateStats backend finishRepo Glean.ExcludeBase
      when (null stats) $
        die 6
          "finish: Database has no facts. Use --allow-zero-facts to allow this"
    finished backend finishRepo finishHandle task parcel failure

finished
  :: LocalOrRemote b
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

  -- If the client needs to create a stacked DB on top of this one, it
  -- will fail unless we wait for the DB to finish finalizing first.
  when (isNothing failure) $
    Glean.LocalOrRemote.finalize backend repo
