{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}
module Glean.Clang.Test (driver, driverWith, Options) where

import qualified Data.Aeson as Aeson
import Data.List
import System.Directory
import System.FilePath
import Text.JSON

import Glean.Indexer
import Glean.Indexer.Cpp
import Glean.Regression.Snapshot.Driver
import Glean.Regression.Snapshot.Transform
import Glean.Util.CxxXRef

import Debug.Trace (trace)

import qualified Data.HashMap.Strict as HM

type Options = Clang

driverWith :: Bool -> Driver Clang
driverWith deriveToo =
  driver' {
    driverTransforms = HM.insert "xrefs" (Transform xrefTransform)
      (driverTransforms driver')
  }
  where
    driver' = driverFromIndexer indexer'
    baseIndexer = indexerWith deriveToo
    isSrcFile file = takeExtension file `elem` [".c", ".cpp", ".m", ".mm"]
    withCompileCommandsFor opts params f = do
      srcFiles <- map (indexerRoot params </>) . filter isSrcFile <$>
        listDirectory (indexerRoot params)
      writeFile (indexerOutput params </> "compile_commands.json") $
        commandsJsonFor (indexerOutput params) srcFiles
      f (indexerOutput params)
    indexer' = baseIndexer
      { indexerRun = \clang backend repo params ->
          withCompileCommandsFor clang params $ \cdbDir ->
            indexerRun baseIndexer (clang { clangCompileDBDir = Just cdbDir })
              backend repo params
      }

driver :: Driver Clang
driver = driverWith False

commandsJsonFor :: FilePath -> [FilePath] -> String
commandsJsonFor tmpDir srcFiles = ("["++) . (++"]") . unlines $ intersperse ","
  [ unlines
    [ "{"
    , "  \"directory\": \"" ++ tmpDir ++ "\","
    , "  \"command\": \"/usr/bin/clang-11 " ++ argsFor srcFile ++ " -o " ++ objFileFor srcFile ++ " -c " ++ srcFile ++ "\","
    , "  \"file\": \"" ++ srcFile ++ "\""
    , "}"
    ]
  | srcFile <- srcFiles
  ]

  where objFileFor srcFile = tmpDir </> (takeFileName srcFile <.> "o")
        argsFor srcFile
          | takeExtension srcFile == ".cpp" = "-std=c++14 -fcoroutines-ts -xc++"
          | takeExtension srcFile == ".c"   = "-std=c99 -xc"
          | takeExtension srcFile == ".mm"  = "-xobjective-c++"
          | otherwise                       = "-xobjective-c"

xrefTransform :: Unit -> [JSValue] -> [JSValue]
xrefTransform _ = map go
  where go jsval = case transformXRefs jsval of
          Ok x -> x
          Error msg -> trace ("xrefTransform error: " ++ show msg) jsval

-- with aeson <2, () isn't a valid decoding of
-- [] and {}, which xrefTransform apparently runs into,
-- so we embed our ()-like little helper that can
-- always be decoded.
-- (problem is fixed with aeson >= 2)
data Unit = Unit
instance Aeson.FromJSON Unit where
  parseJSON _ = pure Unit
