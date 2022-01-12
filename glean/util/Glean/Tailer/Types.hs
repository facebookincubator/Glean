{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Types corresponding to an abstract tailer. A tailer
-- implementation is expected to have this API:
--
-- > module Glean.Tailer
-- >   ( options :: TailerOptions
-- >   , data TailerOptions
-- >   , runTailer :: RunTailer TailerOptions
-- >   )
--
module Glean.Tailer.Types
  ( Entry(..)
  , TailerSettings(..)
  , RunTailer
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import Data.Text (Text)
import System.Exit

import Glean.Types
import Glean.ServerConfig.Types as ServerConfig

data Entry
  = LineData
      { dataPredicateRef :: PredicateRef
      , dataBytes :: BL.ByteString
      }
  | Checkpoint ByteString
  | Invalid String

data TailerSettings opts = TailerSettings
  { setServerConfig :: ServerConfig.Config
  , setSettings :: Map Text Text
  , setResume :: Maybe Text
  , setLogInfo :: String -> IO ()
  , setOptions :: opts

    -- | Read input lines immediately. Only used for testing.
  , setLineBuffering :: Bool
  }

type RunTailer opts =
  TailerSettings opts -> (Entry -> IO ()) -> IO ExitCode
