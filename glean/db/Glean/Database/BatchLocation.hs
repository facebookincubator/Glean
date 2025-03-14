{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Database.BatchLocation
  ( Parser(..)
  , DefaultParser(..)
  ) where

import Glean.Types as Thrift
import Data.Text (Text)
import Glean.Util.Some
import Control.Exception

-- | A batch's location parser
class Parser a where
  -- | Parse a string returning a location to get the batch from
  fromString :: a -> Text -> Thrift.BatchFormat -> IO Thrift.Batch

instance Parser (Some Parser) where
  fromString (Some parser) = fromString parser

data DefaultParser = DefaultParser

instance Parser DefaultParser where
  fromString _ _ = throw $
    Thrift.Exception "Batch location's parser is not implemented"
