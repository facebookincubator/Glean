module Glean.Util.IO (
  HandleIO(..),
  readFileContents, writeFileContents, withTempFileContents,
  backupFileOnError,
) where

import Util.Control.Exception
import Util.Log

import Control.Monad.Extra
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
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

-- | This is a combinator to use with withTempFileContents, to
-- keep a copy of the temp file when there has been an error.  These
-- backups are in the @glean-backup-file-on-error@ subdirectory.
backupFileOnError
  :: Maybe FilePath
  -- ^ Folder to place backup, defaults to a subfolder in the same folder
  -> (FilePath -> IO b)
  -> (FilePath -> IO b)
backupFileOnError dir action filepath = do
  whenJust dir $ createDirectoryIfMissing False
  action filepath `onSomeException` backup filepath
  where
    backup filepath _exc = do
      let subdir =
            fromMaybe
              (takeDirectory filepath </> "glean-backup-file-on-error")
              dir
          dest = subdir </> takeFileName filepath
      logError $ "Making backup of temp file: " <> dest
      createDirectoryIfMissing True subdir
      copyFile filepath dest
