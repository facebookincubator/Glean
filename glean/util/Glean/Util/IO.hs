module Glean.Util.IO (
  HandleIO(..),
  readFileContents, writeFileContents, withTempFileContents,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import System.Directory
import System.FilePath
import System.IO
import qualified System.IO.Temp as Temp

-- Things that can be read from and written to a file handle
class HandleIO a where
  getHandleContents :: Handle -> IO a
  writeHandle :: Handle -> a -> IO ()

instance HandleIO String where
  getHandleContents = hGetContents
  writeHandle = hPutStr

instance HandleIO BS.ByteString where
  getHandleContents = BS.hGetContents
  writeHandle = BS.hPutStr

instance HandleIO LBS.ByteString where
  getHandleContents = LBS.hGetContents
  writeHandle = LBS.hPutStr

instance HandleIO Text.Text where
  getHandleContents = Text.hGetContents
  writeHandle = Text.hPutStr

instance HandleIO LText.Text where
  getHandleContents = LText.hGetContents
  writeHandle = LText.hPutStr

-- Read the contents of the entire file
readFileContents :: HandleIO a => FilePath -> IO a
readFileContents file = getHandleContents =<< openFile file ReadMode

withContents
    :: HandleIO a => a -> (FilePath -> IO b) -> FilePath -> Handle -> IO b
withContents s f path handle = do
  writeHandle handle s
  hClose handle
  f path

-- Execute the action with a temp file containing some data
withTempFileContents :: HandleIO a => a -> (FilePath -> IO b) -> IO b
withTempFileContents s f =
  Temp.withSystemTempFile "glean" $ withContents s f

withTempFileContentsIn
    :: HandleIO a => FilePath -> a -> (FilePath -> IO b) -> IO b
withTempFileContentsIn dir s f =
  Temp.withTempFile dir ".glean" $ withContents s f

-- Atomically create or replace the file with the given contents
writeFileContents :: HandleIO a => FilePath -> a -> IO ()
writeFileContents file s =
  withTempFileContentsIn (takeDirectory file) s $ \tmp -> renameFile tmp file
