{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Clang where

import Control.Monad.Except
import Options.Applicative

import Glean.CMake

-- TODO: when the Derive business is integrated,
--       turn this CLI interface into several
--       commands? (index, derive, do both in one shot)
options :: ParserInfo CppIndexerOpts
options = info (helper <*> parser) fullDesc
  where
    parser :: Parser CppIndexerOpts
    parser = CppIndexerOpts
      <$> strOption
            ( long "srcdir"
           <> help "C++ sources directory, containing the root CMakeLists.txt"
           <> metavar "DIR"
            )
      <*> strOption
            ( long "output"
           <> short 'o'
           <> help "Path to the file where the indexed data should be dumped"
           <> metavar "FILE"
            )
      <*> strOption
            ( long "inventory"
           <> short 'i'
           <> help "Path to the schema inventory file"
           <> metavar "FILE"
            )
      <*> strOption
            ( long "target"
           <> short 't'
           <> help "CMake target to index from the project given by --srcdir"
           <> value "all"
           <> showDefault
            )
      <*> strOption
            ( long "clang-args"
           <> help "Extra arguments to pass to clang (in addition to the ones dictated by CMake)"
           <> value ""
           <> showDefault
            )

main :: IO ()
main = execParser options >>= go
  where
    go opts = do
      r <- runExceptT (indexCMake opts)
      case r of
        Left e  -> error $ "Indexing error: " ++ show e
        Right _ -> putStrLn "Indexing OK."
