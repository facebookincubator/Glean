{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module GleanCLI.Finish (FinishCommand, finished) where

import Control.Monad
import Options.Applicative

import Util.IO
import Util.OptParse

import GleanCLI.Common
import GleanCLI.Types

import Glean
import qualified Glean.Remote
import Glean.LocalOrRemote
import Glean.Database.Meta (getACLMode, showACLMode)
import qualified Glean.Types as Thrift

data FinishCommand
  = Finish
      { finishRepo :: Thrift.Repo
      , allowZeroFacts :: Bool
      }

instance Plugin FinishCommand where
  parseCommand =
    commandParser "finish"
      (progDesc "Notify server that a database is complete") $ do
      finishRepo <- dbOpts
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
    logACLMode backend finishRepo
    finished backend finishRepo

-- | Log the database's ACL mode for this finish operation.
logACLMode :: LocalOrRemote b => b -> Thrift.Repo -> IO ()
logACLMode backend repo = do
  db <- Thrift.getDatabaseResult_database <$> Glean.getDatabase backend repo
  let aclMode = getACLMode (Thrift.database_properties db)
  putStrLn $ "[glean finish] " ++ showACLMode aclMode

finished
  :: LocalOrRemote b
  => b
  -> Thrift.Repo
  -> IO ()
-- | Retry transient channel exceptions so a write-server restart doesn't fail
-- the finish (covers both the finish and write --finish commands).
finished backend repo =
  Glean.finish
    (Glean.Remote.backendRetryWrites backend Glean.Remote.defaultRetryPolicy) repo
