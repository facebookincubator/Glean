-- Copyright 2004-present Facebook. All Rights Reserved.
{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Pretty.Src
  (
  ) where

import Data.Text.Prettyprint.Doc

import Glean
import Glean.Schema.Src.Types as Src

instance Pretty Src.Range where
  pretty Src.Range {
    range_file = File { file_key = filepath },
    range_lineBegin = Nat { unNat = lineBegin }} =
      pretty filepath <> colon <> pretty lineBegin

instance Pretty Src.Loc where
  pretty Src.Loc{..} =
    pretty (file_key loc_file) <> colon <> pretty (unNat loc_line)
