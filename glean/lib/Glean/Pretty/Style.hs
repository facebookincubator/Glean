-- Copyright (c) Facebook, Inc. and its affiliates.


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
