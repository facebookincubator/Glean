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
  , hashFingerprint
  ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Hashable
import Foreign (castPtr)
import GHC.Fingerprint as GHC
import System.IO.Unsafe ( unsafeDupablePerformIO )

-- efficient MD5 hash
type Hash = GHC.Fingerprint

hash0 :: Hash
hash0 = GHC.fingerprint0

hashString :: String -> Hash
hashString = GHC.fingerprintString

hashByteString :: ByteString -> Hash
hashByteString bs = unsafeDupablePerformIO $ do
  unsafeUseAsCStringLen (LB.toStrict bs) $ \(ptr, len) ->
    GHC.fingerprintData (castPtr ptr) (fromIntegral len)

-- | For building Hashable instances. Avoiding an orphan instance by
-- making this a function instead.
hashFingerprint :: Int -> Hash -> Int
hashFingerprint salt (GHC.Fingerprint _ w) = hashWithSalt salt w
