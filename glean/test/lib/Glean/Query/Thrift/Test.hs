{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ConstraintKinds, NamedFieldPuns #-}

-- | Query API for testing only.  See "Glean.Query.Thrift" for the real API.
module Glean.Query.Thrift.Test
  ( -- * Decode result
    runQueryJSON
  , runQueryJSON_
  , runQueryCompact
  , runQueryCompact_
  ) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString.Builder as B
import Data.Default
import Data.Maybe

import Thrift.Protocol
import Thrift.Protocol.Compact
import Thrift.Protocol.JSON

import Glean.Typed as Typed
import Glean.Types as Thrift
import Glean.Backend (Backend(..))
import Glean.Query.Thrift.Internal

