{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Util.Disk
  ( getDiskSize
  , getUsedDiskSpace
  , getFreeDiskSpace
  ) where
import System.Process
import Text.Printf

-- | Return the size in bytes for the current volume
getDiskSize :: FilePath -> IO Int
getDiskSize = getDfOutput "size"

-- | Return the used bytes for the current volume
getUsedDiskSpace :: FilePath -> IO Int
getUsedDiskSpace = getDfOutput "used"

-- | Return the free bytes in the current volume
getFreeDiskSpace :: FilePath -> IO Int
getFreeDiskSpace = getDfOutput "avail"

getDfOutput :: String -> FilePath -> IO Int
getDfOutput outp path =
  read . (!! 1) . words <$> readCreateProcess cmd ""
  where
    cmd = shell (printf "df --output=%s -B1 '%s'" outp path)
