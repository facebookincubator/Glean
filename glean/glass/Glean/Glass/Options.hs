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

import qualified Glean.LocalOrRemote as Glean
import Glean.Util.Time

import qualified Glean.Glass.Env as Glass
import qualified Glean.Glass.Config as Glass

options :: ParserInfo Glass.Config
options = info (helper <*> configParser) fullDesc

configParser :: Parser Glass.Config
configParser = do
  gleanService <- Glean.options
  listenPort <- portParser
  serviceName <- serviceNameParser
  refreshFreq <- refreshFreqParser
  numWorkerThreads <- workerThreadsParser
  return Glass.Config{configKey = Glass.defaultConfigKey, ..}

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
