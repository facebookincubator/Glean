{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo #-}
module Glean.Indexer.Java ( Java(..), indexer ) where

import Options.Applicative

import Glean.Indexer
import Glean.Indexer.External

data Java = Java
  { javaWrapper :: FilePath
  , javaJarJex :: FilePath
  , javacPath :: FilePath
  }

options :: Parser Java
options = do
  javaWrapper <- strOption $
    long "indexer-sh" <>
    value "glean/lang/java-alpha/extract_facts_from_jar.sh" <>
    help "path to the Java indexer wrapper"
  javaJarJex <- strOption $
    long "indexer-jar" <>
    help "path to the Java jar/jex lib"
  javacPath <- strOption $
    long "indexer-javac" <>
    help "path to the javac"
  return Java{..}

indexer :: Indexer Java
indexer = Indexer {
  indexerShortName = "java",
  indexerDescription = "Index Java code",
  indexerOptParser = options,
  indexerRun = \Java{..} backend repo params ->

    let ext = Ext {
          extRunScript = javaWrapper,
          extFlavour = Json,
          extArgs =
            [ javaJarJex -- arg 1
            , "${TEST_ROOT}" -- arg 2: test case folder
            , "${JSON_BATCH_DIR}" -- arg 3: dest
            , javacPath -- arg 4: javac
            ],
          extDerivePredicates =
            [ "java.alpha.Inheritance"
            , "java.alpha.InterfaceInheritance"
            , "java.alpha.ContainsDeclaration"
            , "java.alpha.XRefFile"
            , "java.alpha.FileDeclarations"
            , "javakotlin.alpha.NameLowerCase"
            ]
      }
    in indexerRun externalIndexer ext backend repo params

  }
