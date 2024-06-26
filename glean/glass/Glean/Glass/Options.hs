{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}

module Glean.Glass.Options
  ( options
  , configParser
  ) where

import Options.Applicative
import Data.Text (Text)
import TextShow

import Control.Trace
import qualified Haxl.Core as Haxl

import qualified Glean.LocalOrRemote as Glean
import Glean.Util.Some (Some(..))
import Util.Time

import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Config as Glass
import Glean.Glass.SnapshotBackend
import Glean.Glass.SourceControl
import Glean.Glass.Config (defaultWelcomeMessage)

options
  :: TextShow a
  => Parser (Glass.Config a -> Glass.Config a) -> ParserInfo (Glass.Config a)
options mod = info (helper <*> mod <*> configParser) fullDesc

configParser :: TextShow a => Parser (Glass.Config a)
configParser = do
  gleanService <- Glean.options
  listenPort <- portParser
  serviceName <- serviceNameParser
  refreshFreq <- refreshFreqParser
  listDatabasesRetry <- listDatabasesRetryParser
  numWorkerThreads <- workerThreadsParser
  snapshotBackend <- pure $ pure $ pure $ Some NilSnapshotBackend
  sourceControl <- pure (const (return (Some NilSourceControl)))
  haxlState <- pure (const (return Haxl.stateEmpty))
  allocationLimit <- pure (return Nothing)
  tracer <- tracerParser
  return $ Glass.Config{
        configKey = Glass.defaultConfigKey,
        welcomeMessage = pure (pure . defaultWelcomeMessage),
        useSnapshotsForSymbolsList = pure True,
         ..}

portParser :: Parser Int
portParser = option auto $ mconcat
  [ long "port"
  , short 'p'
  , help "Default port to listen on"
  , value Glass.defaultPort
  ]

refreshFreqParser :: Parser DiffTimePoints
refreshFreqParser = option (minutes <$> auto) $ mconcat
  [ long "refresh"
  , help "Default update frequency for latest repos (minutes)"
  , value Glass.defaultRefreshFreq
  ]

listDatabasesRetryParser :: Parser (Maybe Int)
listDatabasesRetryParser = optional $ option auto $ mconcat
  [ long "list-databases-retry"
  , help "Default retry attempts for listing databases"
  , value Glass.defaultListDatabasesRetry
  ]

serviceNameParser :: Parser Text
serviceNameParser = option auto $ mconcat
  [ long "service-name"
  , help "Override service name for fb303 etc"
  , value Glass.defaultServiceName
  ]

workerThreadsParser :: Parser (Maybe Int)
workerThreadsParser = optional $ option auto $ mconcat
  [ long "worker-threads"
  , help "Number of worker threads (defaults to the number of cores)"
  ]

tracerParser :: TextShow a => Parser (Tracer a)
tracerParser = flag mempty (vlogShowTracer $ const 0) $ mconcat
  [ long "trace-to-vlog"
  , help "Sends telemetry traces to vlog"
  ]
