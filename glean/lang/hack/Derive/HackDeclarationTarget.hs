{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE TypeApplications #-}
module Derive.HackDeclarationTarget
  ( deriveHackDeclarationTarget
  ) where

import Prelude hiding (span)

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.Int as Int
import qualified Data.IntervalMap.Generic.Strict as IM
import Data.List (foldl')
import Data.List.Extra (chunksOf)
import Util.Log (logInfo)

import Glean
import Glean.Angle as Angle
import qualified Glean.Schema.Hack.Types as Hack
import qualified Glean.Schema.Src.Types as Src
import Glean.Util.PredMap (PredMap)
import qualified Glean.Util.PredMap as PredMap
import Glean.Util.Mutex

import Derive.Env
import Derive.Types

type TargetSpans = IM.IntervalMap ByteSpan [Hack.Declaration]
type Files = PredMap Src.File ()
type Sources = PredMap Src.File [(Hack.Declaration, ByteSpan)]
type Targets = PredMap Src.File [[TargetSpans]]

newtype ByteSpan = ByteSpan Src.ByteSpan
  deriving (Eq, Ord, Show)

instance IM.Interval ByteSpan Int.Int64 where
  lowerBound (ByteSpan bs) = unNat $ Src.byteSpan_start bs
  upperBound (ByteSpan bs) =
    unNat (Src.byteSpan_start bs) +
    unNat (Src.byteSpan_length bs)
  leftClosed _ = True -- The interval contains the start value
  rightClosed _ = False -- The interval does not contain start + length

getFilesQuery :: Query Hack.FileXRefs
getFilesQuery = Angle.query $ Angle.predicate @Hack.FileXRefs Angle.wild

getSourcesQuery :: [IdOf Src.File] -> Query Hack.DeclarationSpan
getSourcesQuery files =
  Angle.query $ Angle.vars $ \x file decls decl -> x `Angle.where_`
  [ file .= Angle.elementsOf (Angle.array $ map Angle.factId files)
  , wild .= Angle.predicate @Hack.FileDeclarations  (
      rec $
        field @"file" (asPredicate file) $
        field @"declarations" decls
      end
    )
  , decl .= Angle.elementsOf decls
  , x .= Angle.predicate @Hack.DeclarationSpan (
      rec $
        field @"declaration" decl $
        field @"file" (asPredicate file)
      end
    )
  ]

getTargetsQuery :: [IdOf Src.File] ->  Query Hack.FileXRefs
getTargetsQuery files =
  Angle.query $ Angle.vars $ \x file -> x `Angle.where_`
  [ file .= Angle.elementsOf (Angle.array $ map Angle.factId files)
  , x .= Angle.predicate @Hack.FileXRefs (
      rec $
        field @"file" (asPredicate file)
      end
    )
  ]

getFiles
  :: (Backend e)
  => e
  -> Config
  -> IO Files
getFiles e cfg =
  runQueryEach e (cfgRepo cfg) keysQuery mempty $ \ pm p -> do
    return $! PredMap.insert (getId $ Hack.fileXRefs_key_file p) () pm
  where
      keysQuery = limitBytes (cfgMaxQuerySize cfg) $ keys getFilesQuery

loadPredMap
  :: (Backend e, Predicate p)
  => e
  -> Config
  -> Query p
  -> (KeyType p -> (IdOf k, v))
  -> IO (PredMap k [v])
loadPredMap e cfg q f =
  runQueryEach e (cfgRepo cfg) keysQuery mempty $ \ pm p -> do
    let (k, v) = f p
    return $! PredMap.alter (Just . maybe [v] ((:) v)) k pm
    where
      keysQuery = limitBytes (cfgMaxQuerySize cfg) $ keys q

getSources :: Backend e => e -> Config -> [IdOf Src.File] -> IO Sources
getSources e cfg files = do
  loadPredMap e cfg (getSourcesQuery files) f
  where
    f key = (getId file, (declaration, ByteSpan span))
      where
        file = Hack.declarationSpan_key_file key
        declaration = Hack.declarationSpan_key_declaration key
        span = Hack.declarationSpan_key_span key

getTargets :: Backend e => e -> Config -> [IdOf Src.File] -> IO Targets
getTargets e cfg files = do
  loadPredMap e cfg (getTargetsQuery files) f
  where
    f key = (getId file, targets)
      where
        file = Hack.fileXRefs_key_file key
        xrefs = Hack.fileXRefs_key_xrefs key
        targets = map target xrefs
        target xref = im
          where
            (Hack.XRefTarget_declaration decl) = Hack.xRef_target xref
            spans = Hack.xRef_ranges xref
            (_, im) = foldl' (buildIntervalMap decl) (0, IM.empty) spans
        buildIntervalMap
          :: Hack.Declaration
          -> (Int.Int64, TargetSpans)
          -> Src.RelByteSpan
          -> (Int.Int64, TargetSpans)
        buildIntervalMap decl (offset, im) span =
            (nextOffset, IM.insert byteSpan [decl] im)
          where
            (nextOffset, byteSpan) = relToAbsolute offset span
        relToAbsolute offset rel =
          (start, ByteSpan $ Src.ByteSpan (Nat start) len)
          where
            start = unNat (Src.relByteSpan_offset rel) + offset
            len = Src.relByteSpan_length rel

declarationTargetsForFile
  :: (Sources, Targets)
  -> IdOf Src.File
  -> [Hack.DeclarationTarget_key]
declarationTargetsForFile (sources, targets) fileId =
  declTargets
  where
    sourceList = PredMap.findWithDefault [] fileId sources
    targetList = PredMap.findWithDefault [] fileId targets
    targetIm = IM.unionsWith (++) $ concat targetList
    declTargets = concatMap (declarationTargetsForSource targetIm) sourceList

declarationTargetsForSource
  :: TargetSpans
  -> (Hack.Declaration, ByteSpan)
  -> [Hack.DeclarationTarget_key]
declarationTargetsForSource targetSpans (source, sourceSpan) =
  map (Hack.DeclarationTarget_key source) targets
  where
    targets = concatMap snd $ IM.toList targetsIm
    targetsIm = targetSpans `IM.within` sourceSpan

writeHackDeclarationTargets
  :: Writer -> TBQueue (Maybe [Hack.DeclarationTarget_key]) -> IO ()
writeHackDeclarationTargets writer tbqueue = do
  next <- atomically $ readTBQueue tbqueue
  case next of
    Nothing -> return ()
    Just declTargets -> do
      forM_ declTargets $ \declTarget -> writeFacts writer $
        makeFact_ @Hack.DeclarationTarget declTarget
      writeHackDeclarationTargets writer tbqueue


writeChunkedParallelMap
  :: (NFData b)
  => Int
  -> TBQueue (Maybe [b])
  -> ((Int,[a]) -> IO ctx)
  -> (ctx -> a -> [b])
  -> [a]
  -> IO ()
writeChunkedParallelMap nchunks tbqueue chunkCtx f xs = do
  logInfo $ "calculating " ++ show nchunks ++ " chunks for "
    ++ show (length xs) ++ " inputs"
  mutex <- newMutex ()
  forConcurrently_ (zip [1..] chunks) $ \(i, chunk) -> do
    -- Use mutex to avoid many queries in flight at once
    ctx <- withMutex_ mutex $ chunkCtx (i, chunk)
    ys <- evaluate $ force $ Just $ concatMap (f ctx) chunk
    atomically $ writeTBQueue tbqueue ys
    logInfo $ "calculated chunk " ++ show (i::Int)
  where
    chunks = chunksOf (max 1 chunkSize) xs
    chunkSize = (len + nchunks - 1) `div` nchunks
    len = length xs

deriveHackDeclarationTarget
  :: Env
  -> IO ()
deriveHackDeclarationTarget env@Env{..} = do
  numCapabilities <- getNumCapabilities
  logInfo "deriving HackDeclarationTarget"
  files <- getFiles envBackend envConfig
  logInfo "Got files"
  tbqueue :: TBQueue (Maybe [Hack.DeclarationTarget_key]) <-
    atomically $ newTBQueue (fromIntegral $ cfgMaxQueueSize envConfig)
  if null files
  then do
    logInfo "no file facts found - skipping"
    return ()
  else do
    let
      contextFor (i, fileIds) = do
        (sources, targets) <- concurrently
           (getSources envBackend envConfig fileIds
              >>= done ("Chunk " <> show i <> " got sources"))
           (getTargets envBackend envConfig fileIds
              >>= done ("Chunk " <> show i <> " got targets"))
        return (sources, targets)

      calcDeclTargets = do
        writeChunkedParallelMap
          nchunks
          tbqueue
          contextFor
          declarationTargetsForFile
          (PredMap.keys files)
        atomically $ writeTBQueue tbqueue Nothing
        where
          nchunks = max 1 $ numCapabilities * cfgChunksPerCapability envConfig

      writeDeclTargets =
        withEnvWriter env $ \writer ->
          writeHackDeclarationTargets writer tbqueue

    ((), ()) <- runConcurrently $ (,)
      <$> Concurrently
           ( calcDeclTargets >>= done "calculated declaration targets")
      <*> Concurrently
            (writeDeclTargets >>= done "written facts")
    return ()
  where
    done msg x = logInfo msg >> return x
