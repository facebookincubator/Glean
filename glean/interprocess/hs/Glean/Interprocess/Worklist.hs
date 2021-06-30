-- Copyright (c) Facebook, Inc. and its affiliates.

module Glean.Interprocess.Worklist
  ( Worker, Worklist, Range(..), withTemp, get, next, peek, doNext
  ) where

import Glean.FFI (invoke)

import Control.Exception (bracket)
import Data.Word (Word32)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable)
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

newtype Worklist = Worklist (Ptr Worklist)
  deriving(Storable)

data Range = Range
  { rangeStart :: {-# UNPACK #-} !Int
  , rangeEnd   :: {-# UNPACK #-} !Int
  }
  deriving(Eq,Ord,Show)

mkRange :: Word32 -> Word32 -> Range
mkRange start end = Range (fromIntegral start) (fromIntegral end)

withTemp :: [Range] -> (FilePath -> Worklist -> IO a) -> IO a
withTemp xs f = withSystemTempFile ".glean-worklist" $ \path h -> do
  hClose h
  withCString path $ \cpath -> do
    withArray (map (fromIntegral . rangeStart) xs) $ \starts ->
      withArray (map (fromIntegral . rangeEnd) xs) $ \ends ->
        invoke $ glean_interprocess_worklist_create
          cpath
          (fromIntegral $ length xs)
          starts
          ends
    bracket
      (invoke $ glean_interprocess_worklist_open cpath)
      glean_interprocess_worklist_close
      (f path)

type Worker = Int

get :: Worklist -> Worker -> IO Range
get w i = fmap (uncurry mkRange) $
  invoke $ glean_interprocess_worklist_get w $ fromIntegral i

next :: Worklist -> Worker -> IO (Range, Worker)
next w i = do
  (start,end,victim) <-
    invoke $ glean_interprocess_worklist_next w $ fromIntegral i
  return (mkRange start end, fromIntegral victim)

withWorkfile
  :: (Worklist -> Worker -> IO a)
  -> FilePath -> Worker -> IO a
withWorkfile op workfile w = withCString workfile $ \ cpath -> bracket
  (invoke $ glean_interprocess_worklist_open cpath)
  glean_interprocess_worklist_close
  (`op` w)

doNext :: FilePath -> Worker -> IO (Range, Worker)
doNext w = withWorkfile next w

peek :: FilePath -> Worker -> IO Range
peek w = withWorkfile get w

foreign import ccall unsafe glean_interprocess_worklist_create
  :: CString -> CSize -> Ptr Word32 -> Ptr Word32 -> IO CString

foreign import ccall unsafe glean_interprocess_worklist_open
  :: CString -> Ptr Worklist -> IO CString

foreign import ccall unsafe glean_interprocess_worklist_close
  :: Worklist -> IO ()

foreign import ccall unsafe glean_interprocess_worklist_get
  :: Worklist -> CSize -> Ptr Word32 -> Ptr Word32 -> IO ()

foreign import ccall unsafe glean_interprocess_worklist_next
  :: Worklist -> CSize -> Ptr Word32 -> Ptr Word32 -> Ptr CSize -> IO ()
