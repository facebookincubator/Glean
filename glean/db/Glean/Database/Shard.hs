module Glean.Database.Shard
  ( dbShard, Shard
  ) where

import Data.Bits
import qualified Data.ByteString.Unsafe as B
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Foreign.Ptr
import GHC.Fingerprint
import System.IO.Unsafe

import qualified Glean.Types as Thrift


type Shard = Text

dbShard :: Thrift.Repo -> Shard
dbShard Thrift.Repo{..} =
  unsafeDupablePerformIO $ B.unsafeUseAsCStringLen repo $ \(ptr,len) -> do
      -- Use GHC's md5 binding. If this ever changes then the test in
      -- hs/tests/TestShard.hs will detect it.
    Fingerprint w _ <- fingerprintData (castPtr ptr) len
    return $ Text.pack (show (w `shiftR` 1))
       -- SR doesn't like shards >= 0x8000000000000000
  where
  repo = Text.encodeUtf8 repo_name <> "/" <> Text.encodeUtf8 repo_hash
