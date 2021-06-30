-- Copyright (c) Facebook, Inc. and its affiliates.

module MakeTestDB (main) where

import System.Environment

import Glean.Database.Test
import TestDB

main :: IO ()
main = do
  [tmpdir] <- getArgs
  withTestDB [setRoot tmpdir] $ \_ _ -> return ()
