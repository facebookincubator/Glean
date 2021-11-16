{-
  Copyright (c) Facebook, Inc. and its affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Glean.Pretty.Search () where

import Data.Text.Prettyprint.Doc

import Glean.Search.Types
import Glean.Util.URI

instance Pretty FileXRef where
  pretty = prettyFileXRef


prettyFileXRef :: FileXRef -> Doc a
prettyFileXRef FileXRef { fileXRef_file_name = name
                        , fileXRef_line_nos = mLineNos} =
  case mLineNos of
    Nothing -> pretty name
    Just lineNos -> vsep $ map (prettyRef name) lineNos
  where
    prettyRef file lineNo = vsep
      [ pretty file <> colon <> pretty lineNo
      , indent 2 $ pretty $ show $ fbsDiffusionURI file (Just lineNo)
      ] <> line
