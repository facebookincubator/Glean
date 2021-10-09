-- Copyright (c) Facebook, Inc. and its affiliates.

module Derive.Generic
  ( derivePredicate
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

import Util.Log

import Glean
import Glean.Write (parseRef)
import qualified Glean.Derive

import Derive.Types

derivePredicate :: Backend b => b -> Config -> Text -> IO ()
derivePredicate backend cfg txt = do
  logInfo $ "deriving " <> Text.unpack txt
  Glean.Derive.derivePredicate backend (cfgRepo cfg)
    (Just (fromIntegral (cfgMaxStoreQuerySize cfg)))
    (fromIntegral <$> cfgMaxQueryFacts cfg)
    (parseRef txt)
    Nothing
  logInfo $ "done deriving " <> Text.unpack txt
