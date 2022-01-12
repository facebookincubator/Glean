{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Diff (main) where

import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Options.Applicative

import Util.EventBase

import qualified Glean
import Glean (Repo(..))
import qualified Glean.Database.Config as Database
import qualified Glean.Database.Env as Database
import Glean.Database.Schema (schemaInventory)
import Glean.Database.Open (readDatabase)
import Glean.Database.Types
import Glean.FFI (invoke, with)
import Glean.RTS.Foreign.Inventory (Inventory)
import Glean.RTS.Foreign.Lookup
import Glean.Util.ConfigProvider

data Config = Config
  { cfgDB :: Database.Config
  , cfgOriginal :: Repo
  , cfgNew :: Repo
  }

options :: ParserInfo Config
options = info (parser <**> helper)
  (fullDesc <> progDesc "Create, manipulate and query Glean databases")
  where
    parser :: Parser Config
    parser = do
      cfgDB <- Database.options
      cfgOriginal <- argument (maybeReader Glean.parseRepo)
        (  metavar "NAME/HASH"
        )
      cfgNew <- argument (maybeReader Glean.parseRepo)
        (  metavar "NAME/HASH"
        )
      return Config{..}

main :: IO ()
main =
  withConfigOptions options $ \(Config{..}, cfgOpts) ->
  withEventBaseDataplane $ \evb ->
  withConfigProvider cfgOpts $ \cfgAPI ->
  Database.withDatabases evb cfgDB cfgAPI $ \env ->
  readDatabase env cfgOriginal $ \_original_schema original ->
  readDatabase env cfgNew $ \odb new ->
  with (schemaInventory (odbSchema odb)) $ \inventory_ptr ->
  withLookup original $ \original_ptr ->
  withLookup new $ \new_ptr ->
  invoke $ glean_diff inventory_ptr original_ptr new_ptr

foreign import ccall safe glean_diff
  :: Ptr Inventory -> Lookup -> Lookup -> IO CString
