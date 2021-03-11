module Glean.RTS.Constants
  ( firstAnonId
  ) where

import Data.Int

-- | Fact IDs used for anonymous facts, see Glean.Write.JSON
firstAnonId :: Int64
firstAnonId = 0x4000000000000000  -- 50% of the positive Id range
