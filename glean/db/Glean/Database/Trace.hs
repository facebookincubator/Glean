{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.Trace
  ( GleanTrace(..)
  , WriteTraceEvent(..)
  ) where

import Data.Word
import TextShow

import qualified Glean.Types as Thrift

data GleanTrace = GleanTraceWrite !Thrift.Repo !WriteTraceEvent !Word64

data WriteTraceEvent
  = WriteTraceInput
  | WriteTraceRename
  | WriteTraceCommit

instance TextShow WriteTraceEvent where
  showb WriteTraceInput = "input"
  showb WriteTraceRename = "rename"
  showb WriteTraceCommit = "commit"
