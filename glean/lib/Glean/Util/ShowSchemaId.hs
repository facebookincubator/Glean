{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.ShowSchemaId
  ( showSchemaId
  ) where

import Data.Text (unpack)
import Options.Applicative ( Parser, help, infoOption, long )

import Glean.Types (unSchemaId)
import Glean.Schema.Builtin.Types (schema_id)

-- | Adds a flag to show the schema id
--
--   > parser <**> helper <**> showSchemaId

showSchemaId :: Parser (a -> a)
showSchemaId =
  infoOption (unpack $ unSchemaId schema_id)
    (long "show-schema-id" <>
     help "Show schema id and exit"
     )
