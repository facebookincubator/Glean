{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Logger.GleanServer (module Logger.GleanServer) where

import Data.Text

import Logger.IO

data GleanServerLogger = GleanServerLogger

instance Semigroup GleanServerLogger where
  _ <> _ = GleanServerLogger

instance Monoid GleanServerLogger where
  mempty = GleanServerLogger

runLog :: Logger -> GleanServerLogger -> IO ()
runLog _ _ = return ()

setSuccess :: Bool -> GleanServerLogger
setSuccess _ = GleanServerLogger

setError :: Text -> GleanServerLogger
setError _ = GleanServerLogger

setTimeElapsed :: Double -> GleanServerLogger
setTimeElapsed _ = GleanServerLogger

setAllocatedBytes :: Int -> GleanServerLogger
setAllocatedBytes _ = GleanServerLogger

setMethod :: Text -> GleanServerLogger
setMethod _ = GleanServerLogger

setWeight :: a -> GleanServerLogger
setWeight _ = GleanServerLogger

setRepoName :: Text -> GleanServerLogger
setRepoName _ = GleanServerLogger

setRepoHash :: Text -> GleanServerLogger
setRepoHash _ = GleanServerLogger

setQuery :: Text -> GleanServerLogger
setQuery _ = GleanServerLogger

setPredicate :: Text -> GleanServerLogger
setPredicate _ = GleanServerLogger

setPredicateVersion :: Int -> GleanServerLogger
setPredicateVersion _ = GleanServerLogger

setSchemaVersion :: Int -> GleanServerLogger
setSchemaVersion _ = GleanServerLogger

setNoBase64Binary :: Bool -> GleanServerLogger
setNoBase64Binary _ = GleanServerLogger

setExpandResults :: Bool -> GleanServerLogger
setExpandResults _ = GleanServerLogger

setRecursive :: Bool -> GleanServerLogger
setRecursive _ = GleanServerLogger

setMaxResults :: Int -> GleanServerLogger
setMaxResults _ = GleanServerLogger

setTruncated :: Bool -> GleanServerLogger
setTruncated _ = GleanServerLogger

setResults :: Int -> GleanServerLogger
setResults _ = GleanServerLogger

setRawQueries :: Int -> GleanServerLogger
setRawQueries _ = GleanServerLogger

setFacts :: Int -> GleanServerLogger
setFacts _ = GleanServerLogger

setSyntax :: Text -> GleanServerLogger
setSyntax _ = GleanServerLogger

setType :: Text -> GleanServerLogger
setType _ = GleanServerLogger

setBytecodeSize :: Int -> GleanServerLogger
setBytecodeSize _ = GleanServerLogger

setCompileTimeUs :: Int -> GleanServerLogger
setCompileTimeUs _ = GleanServerLogger

setExecuteTimeUs :: Int -> GleanServerLogger
setExecuteTimeUs _ = GleanServerLogger

setClientUnixname :: Text -> GleanServerLogger
setClientUnixname _ = GleanServerLogger

setClientApplication :: Text -> GleanServerLogger
setClientApplication _ = GleanServerLogger

setClientName :: Text -> GleanServerLogger
setClientName _ = GleanServerLogger

setRequestContinuationSize :: Int -> GleanServerLogger
setRequestContinuationSize _ = GleanServerLogger

setResponseContinuationSize :: Int -> GleanServerLogger
setResponseContinuationSize _ = GleanServerLogger
