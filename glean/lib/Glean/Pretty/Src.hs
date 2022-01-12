{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Pretty.Src (
  ) where

import Data.Text.Prettyprint.Doc

import Glean
import Glean.Schema.Src.Types as Src

instance Pretty Src.Range where
  pretty
    Src.Range
      { range_file = File{file_key = filepath}
      , range_lineBegin = Nat{unNat = lineBegin}
      } =
      pretty filepath <> colon <> pretty lineBegin

instance Pretty Src.Loc where
  pretty Src.Loc{..} =
    pretty (file_key loc_file) <> colon <> pretty (unNat loc_line)

instance Pretty Src.FileLocation where
  pretty
    Src.FileLocation
      { fileLocation_file = File{file_key = filepath}
      , fileLocation_span =
        ByteSpan
          { byteSpan_start = Nat{unNat = bsStart}
          , byteSpan_length = Nat{unNat = bsLength}
          }
      } =
      -- ,
      pretty filepath <+> "at byte"
        <+> pretty bsStart
        <+> " with length "
        <+> pretty bsLength
