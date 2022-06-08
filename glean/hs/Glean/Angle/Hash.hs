{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- | Fingerprints for schema versioning.
--
-- Doesn't need to be cryptographically secure, but does need to be
-- robust against accidental collisions.

module Glean.Angle.Hash
  ( Hash
  , hash0
  , hashString
  , hashByteString
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign (castPtr)
import GHC.Fingerprint as GHC
import System.IO.Unsafe ( unsafeDupablePerformIO )

-- efficient MD5 hash
type Hash = GHC.Fingerprint

hash0 :: Hash
hash0 = hashString ""

hashString :: String -> Hash
hashString = GHC.fingerprintString

hashByteString :: ByteString -> Hash
hashByteString bs = unsafeDupablePerformIO $ do
  unsafeUseAsCStringLen (LB.toStrict bs) $ \(ptr, len) ->
    GHC.fingerprintData (castPtr ptr) (fromIntegral len)
