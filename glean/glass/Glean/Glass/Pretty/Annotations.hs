{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


module Glean.Glass.Pretty.Annotations
  ( prettyCxxAnnotation
  , prettyHackAnnotation
  ) where

import Data.Text (Text, intercalate)

prettyCxxAnnotation :: Text -> Text
prettyCxxAnnotation key = key

prettyHackAnnotation :: Text -> [Text] -> Text
prettyHackAnnotation name [] = name
prettyHackAnnotation name args = name <> prettyArgs
  where
    prettyArgs = "(" <> intercalate ", " args <> ")"
