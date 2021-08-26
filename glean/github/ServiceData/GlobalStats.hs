-- Copyright (c) Facebook, Inc. and its affiliates.

module ServiceData.GlobalStats (module ServiceData.GlobalStats) where

import Data.ByteString (ByteString)

import ServiceData.Types

setCounter :: ByteString -> Int -> IO ()
setCounter _ _ = return ()

addStatValue :: ByteString -> Int -> IO ()
addStatValue _ _ = return ()

addStatValueType :: ByteString -> Int -> ExportType -> IO ()
addStatValueType _ _ _ = return ()
