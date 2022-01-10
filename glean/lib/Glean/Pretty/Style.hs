{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}


-- | Support @Glean.Pretty.*@ modules, "Glean.Pretty.Styles",
-- and "Glean.Util.AnnMaker".
module Glean.Pretty.Style
  ( Style(..)
  ) where

import Data.String (IsString(..))
import Data.Text (Text)

-- | Specific Text newtype for annotations, used in "Glean.Util.AnnMaker".
-- Known 'Style' values are in "Glean.Pretty.Styles".
newtype Style = Style{ unStyle :: Text } deriving (Eq, Show, Ord)

instance IsString Style where
  fromString = Style . fromString
