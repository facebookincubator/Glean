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
  , hashBinary
  , hashFingerprint
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
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

hashByteString :: B.ByteString -> Hash
hashByteString bs = unsafeDupablePerformIO $ do
  unsafeUseAsCStringLen bs $ \(ptr, len) ->
    GHC.fingerprintData (castPtr ptr) (fromIntegral len)

hashBinary :: Binary a => a -> Hash
hashBinary = hashByteString . LB.toStrict . Binary.encode

-- | For building Hashable instances. Avoiding an orphan instance by
-- making this a function instead.
hashFingerprint :: Int -> Hash -> Int
hashFingerprint salt (GHC.Fingerprint _ w) = hashWithSalt salt w
