{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE ApplicativeDo, TypeApplications #-}
module Derive.CxxDeclarationTargets
  ( deriveCxxDeclarationTargets
  ) where

import Control.Concurrent.Async (Concurrently(..), runConcurrently)
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Array hiding ((!))
import Data.Array.ST
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Vector as V
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import GHC.Compact as Compact
import GHC.Stack (HasCallStack)
import Text.Printf

import Thrift.Channel
import Control.Concurrent.Stream
import Util.Control.Exception (catchAll)
import Util.Log (logError, logInfo)

import Glean
import Glean.Angle
import qualified Glean.Schema.Cxx1.Types as Cxx
import qualified Glean.Schema.Src.Types as Src
import Glean.Util.Declarations ( applyDeclaration, getDeclId )
import Glean.Util.PredMap (PredMap)
import qualified Glean.Util.PredMap as PredMap
import Glean.Util.Range
import Glean.Util.Time

import Derive.Common
import Derive.Types


-- -----------------------------------------------------------------------------
-- Derive function calls : make delcaration source/target map
-- The second derived pass will reverse this map

-- | a map from a file id to the declaration in the file
type Decls = PredMap Src.File [(Cxx.Declaration,Src.Range)]
type FileLines = PredMap Src.File LineOffsets

-- | Helper to pick out the declaration branch
toPredicateRef :: Cxx.Declaration -> PredicateRef
toPredicateRef = applyDeclaration (getName . mkProxy)
  where
    mkProxy :: a -> Proxy a
    mkProxy _ = Proxy

getDeclarations :: Backend e => e -> Config -> IO Decls
getDeclarations e cfg = do
  let
    q :: Query (Cxx.Declaration, Src.Range)
    q = maybe id limit (cfgMaxQueryFacts cfg) $
       limitBytes (cfgMaxQuerySize cfg) $ query $
         vars $ \decl range ->
         tuple (decl,range) `where_` [
           stmt $
             (sig unit `where_` [
               alt @"function_" (rec $ field @"source" range end) .= decl
             ]) .|
             (sig unit `where_` [
               alt @"objcContainer" (rec $ field @"source" range end) .= decl
             ]) .|
             (sig unit `where_` [
               alt @"objcMethod" (rec $ field @"source" range end) .= decl
             ]) .|
             (sig unit `where_` [
               alt @"objcProperty" (rec $ field @"source" range end) .= decl
             ])
         ]

  runQueryEach e (cfgRepo cfg) q mempty $ \newdecls p@(_,range) -> do
    let fileId = getId $ Src.range_file range
    return $! PredMap.insertWith (const (p:)) fileId [p] newdecls

getFileLines ::  Backend e => e -> Config -> IO FileLines
getFileLines e cfg = do
  let
    q :: Query Src.FileLines
    q = maybe id limit (cfgMaxQueryFacts cfg) $
       limitBytes (cfgMaxQuerySize cfg) allFacts

  runQueryEach e (cfgRepo cfg) q mempty
    $ \fileliness (Src.FileLines _ k) -> do
    key <- case k of
      Just k -> return k
      Nothing -> throwIO $ ErrorCall "internal error: getFileLines"
    let
      fileId = getId $ Src.fileLines_key_file key
      !offsets = lengthsToLineOffsets key
    return $! PredMap.insert fileId offsets fileliness

-- For building the declaration graph, there are a few algorithm choices.
--
-- Given a list of "small" ranges representing cross-references (targets)
-- and a set of "big" ranges representing function declarations (sources),
-- the function matchTargetsToSources returns a list of all pairs consisting
-- of a function declaration and the cross references to functions in it.
-- Each "small" range is represented by a pair ((begin, len), s),
-- and each "big" range is represented by a pair ((begin, len), t),
-- where in both cases begin <= begin + len is assumed.

-- | This performs the target/source matching using brute force naive
-- list traversals. The other implementations below should run
-- faster than this.
--
-- This naive implementation works in O(T * S) time
-- where T = length targets and S = length sources.
matchNaive :: [[(ByteRange, t)]] -> [(ByteRange, s)] -> [(s, [t])]
matchNaive targetss sources =
  [ (s, ts)
  | (rS, s) <- sources
  , let ts = [ t | (rT, t) <- concat targetss, byteRangeContains rS rT ]
  , not $ null ts
  ]

-- the below one works in time O((T+S) * log (T + S) + M)
-- where M is the size of the output. In practice slower than the naive one.

data Event t s = TargetEnds   !ByteRange t
               | SourceEnds   !ByteRange s

data MatchesAcc t s = MatchesAcc !(Map.Map ByteRange [t]) ![(s, [t])]

-- | This performs the target/source matching with a preprocessing pass. It
-- should run faster than `matchNaive`.
--
-- Both targets and sources are pre-processed into a single event list
-- (sorted by range end, ascending).
-- Then events are processed in order. targets are accumulated into a Map,
-- and each source is associated with a suffix of the Map of targets.
matchSweepingLine :: [[(ByteRange, t)]] -> [(ByteRange, s)] -> [(s, [t])]
matchSweepingLine targetss sources = matches
  where
    (MatchesAcc _ matches) =
      foldl' applyEvent (MatchesAcc Map.empty []) events
    events = sortOn eventKey $ [ SourceEnds range s | (range, s) <- sources]
      ++ [ TargetEnds range t | targets <- targetss, (range, t) <- targets]

    eventKey :: Event t s -> (Word64, Int)
    eventKey (TargetEnds br _) = (byteRangeExclusiveEnd br, 0)
    eventKey (SourceEnds br _) = (byteRangeExclusiveEnd br, 1)

    applyEvent :: MatchesAcc t s
               -> Event t s
               -> MatchesAcc t s
    applyEvent (MatchesAcc endedTargets matches) event = case event of
      (TargetEnds range t) -> MatchesAcc endedTargets' matches
        where
          endedTargets' = Map.insertWith (const (t:)) range [t] endedTargets
      (SourceEnds (ByteRange beginS _) s) -> MatchesAcc endedTargets matches'
        where
          ts = concatMap snd
            $ takeWhile (\(ByteRange beginT _, _) -> beginT >= beginS)
            $ Map.toDescList endedTargets
          matches' = if null ts then matches else (s, ts) : matches


-- given a sorted vector, returns the least index i such that v!i >= a
-- or (VU.length v) if no such element
binsearchGE :: (Ord a, VU.Unbox a) => VU.Vector a -> a -> Int
binsearchGE v a = go (-1) (VU.length v)
    where
  -- invariant: v!lo < a && v!hi >= a
  go lo hi =
    if succ lo == hi
    then hi
    else
      let mi = (lo + hi) `div` 2
      in if v!mi < a
         then go mi hi
         else go lo mi

-- | This performs the target/source matching with pre-processed vectors and
-- ST for arrays. It should run faster than `matchNaive`.
--
-- Iterates though targets. O(S*log(S)) then
-- O(T * log(S) * overlap) where overlap is max(forall targets: number
-- of sources partially overlapping the target).
matchSourceVector :: [[(ByteRange, t)]] -> [(ByteRange, s)] -> [(s, [t])]
matchSourceVector _ [] = []
matchSourceVector [] _ = []
matchSourceVector targetss sources =
  let
    -- ascending by end position
    sortedSources = sortOn (byteRangeExclusiveEnd . fst) sources
    -- unsorted
    begins = VU.fromList [begin | (ByteRange begin _, _) <- sortedSources]
    -- ascending
    ends = VU.fromList [byteRangeExclusiveEnd br | (br, _) <- sortedSources]
    -- ascending (min of begin of ranges in tail)
    beginMins = VU.fromList
      $ scanr1 min [begin | (ByteRange begin _, _) <- sortedSources]

    numSources = length sortedSources
    sourcesForTarget :: ByteRange -> [Int]
    sourcesForTarget br@(ByteRange begin _len) =
      filter (\i -> begins!i <= begin) $
      {- since sources are sorted, we already know that end <= ends!i -}
      takeWhile (\i -> beginMins!i <= begin)
      [binsearchGE ends (byteRangeExclusiveEnd br)..(numSources-1)]

    targetsBySource = runSTArray $ do
      arr <- newArray (0,numSources-1) []
      forM_ targetss $ \targets ->
        forM_ targets $ \(range, t) ->
          forM_ (sourcesForTarget range) $ \i -> do
            ts <- readArray arr i
            writeArray arr i (t:ts)
      return arr
  in
    [ (s, ts) | ((_, s), ts) <- zip sortedSources $ elems targetsBySource]

-- | Make 'Cxx.DeclarationTargets' and then 'Cxx.DeclarationSources' facts
--
-- Works by matching target coordinates to span of source declarations. There
-- are 3 choices for this
deriveCxxDeclarationTargets
  :: Backend e
  => e
  -> Config
  -> (Int -> ([Writer] -> IO ()) -> IO ())
  -> IO ()
deriveCxxDeclarationTargets e cfg withWriters = withWriters workers $ \ writers -> do
  logInfo "deriveCxxDeclarationTargets"

  -- ---------------------------------------------------------------------------
  -- Main queue and logging

  startTimePoint <- getTimePoint -- for cfgBenchmark

  -- for cfgBenchmark : total microseconds running the chosen matching algorithm
  matchingTimeRef <- newIORef (0::Int)

  -- For cfgDebugPrintReferences, track sparse matrix of count of references
  summaryRef <- newIORef (Map.empty :: Map (PredicateRef, PredicateRef) Int)

  -- ---------------------------------------------------------------------------
  -- We are limited by the loading of cxx.FileXRefMap facts, start it first

  -- NOTE: We rely on the ordering property of the `allFacts` query.
  --       Specifically, we expect to encounter `FileXRefMap`s for
  --       a single file in a single sequence.
  let
    q :: Query Cxx.FileXRefMap
    q = maybe id limit (cfgMaxQueryFacts cfg) $
        limitBytes (cfgMaxQuerySize cfg) allFacts

    doFoldEach go = do
      fileTargetCountAcc <-
        runQueryEach e (cfgRepo cfg) q (Nothing, [], 0::Int, 0::Int) $
          \ (!mLastFile, !targetssIn, !countIn, !nIn)
            (Cxx.FileXRefMap i k) -> do
            when (mod nIn 10000 == 0) $ logInfo $
              "(file count, FileXRefMap count) progress: "
              ++ show (countIn, nIn)
            let !id = IdOf $ Fid i
            key <- case k of
              Just k -> return k
              Nothing -> throwIO $ ErrorCall
                "internal error: deriveFunctionCalls"
            let !file = Cxx.fileXRefMap_key_file key
            case mLastFile of
              (Just lastFile) | getId lastFile /= getId file -> do
                go (lastFile, targetssIn)
                return (Just file, [(id, key)], succ countIn, succ nIn)
              _ ->
                return (Just file, (id, key):targetssIn, countIn, succ nIn)
      (countF, nF) <- case fileTargetCountAcc of
        (Nothing, _empty, countIn, nIn) -> return (countIn, nIn)
        (Just file, targetssIn, countIn, nIn) -> do
          go (file, targetssIn)
          return (succ countIn, nIn)
      logInfo $ "(file count, FileXRefMap count) final: " ++ show (countF, nF)

  -- ---------------------------------------------------------------------------
  -- Load state needed for processing cxx.FileXRefMap

  let done :: String -> a -> IO a  -- useful for seeing timing in logs
      done msg x = logInfo msg >> return x

  (decls, fileliness, indirects) <- do
    maps <- runConcurrently $ (,,)
      <$> Concurrently (getDeclarations e cfg >>= done "declarations")
      <*> Concurrently (getFileLines e cfg >>= done "fileLines")
      <*> Concurrently (getIndirectTargets e cfg >>= done "indirect")
    logInfo "loaded predmaps, compacting"
    compactMaps <- Compact.compact maps
    size <- Compact.compactSize compactMaps
    logInfo $ "compact complete (" ++ show size ++ " bytes)"
    return (Compact.getCompact compactMaps)
  logInfo $ "loaded " ++ show (PredMap.size decls) ++ " declarations"
  logInfo $ "loaded " ++ show (PredMap.size fileliness) ++ " file liness"
  logInfo $ "loaded " ++ show (PredMap.size indirects) ++ " indirect targets"

  -- ---------------------------------------------------------------------------
  -- Code for processing cxx.FileXRefMap

  let matchTargetsToSources = case cfgMatchAlgorithm cfg of
        Naive -> matchNaive
        SweepingLine -> matchSweepingLine
        SourceVector -> matchSourceVector

  let generateCalls
        :: HasCallStack
        => Writer -> Src.File -> [[(ByteRange, [Cxx.Declaration])]] -> IO Int
      generateCalls writer file targetss =
        let
          fileId = getId file

          offsets = case PredMap.lookup fileId fileliness of
              (Just offsets) -> offsets
              _ -> error $ "no file lines for file" ++ show fileId

          sources =
            [ ( srcRangeToSimpleByteRange offsets srcRange, decl)
            | (decl, srcRange) <- PredMap.findWithDefault [] fileId decls
            ]

          calls = matchTargetsToSources targetss sources

          callsTargetSets =
            [ (s, ts')
            | (s, ts) <- calls
            , let ts' = Set.fromList $ concat ts
            , not $ Set.null ts'
            ]

        in
          do
            forcedCallsTargetSets <-
              if cfgBenchmark cfg
              then do
                before <- getTimePoint
                fcts <- evaluate $ force callsTargetSets
                elapsed <- toDiffMicros <$> getElapsedTime before
                atomicModifyIORef' matchingTimeRef $ \ old -> (old+elapsed, ())
                return fcts
              else
                evaluate $ force callsTargetSets
            when (not $ cfgDryRun cfg) $
              writeFacts writer $
                forM_ forcedCallsTargetSets $ \(s, ts) -> do
                    makeFact_ @Cxx.DeclarationTargets Cxx.DeclarationTargets_key
                      { declarationTargets_key_source = s
                      , declarationTargets_key_targets = Set.toList ts
                      }
            when (cfgDebugPrintReferences cfg) $
              forM_ forcedCallsTargetSets $ \(s, ts) ->
                forM_ ts $ \t -> do
                  let sname = toPredicateRef s
                      tname = toPredicateRef t
                  firstPair <- atomicModifyIORef' summaryRef $ \ old ->
                    let summary' =
                          Map.insertWith (const (+1)) (sname, tname) 1 old
                    in (summary', Map.notMember (sname, tname) old)
                  when firstPair $ putStrLn $ concat
                    [ Text.unpack $ predicateRef_name sname
                    , "{", show $ getDeclId s, "} -> "
                    , Text.unpack $ predicateRef_name tname
                    , "{", show $ getDeclId t, "}" ]
            let count = sum (map (Set.size . snd) forcedCallsTargetSets)
            return $! count

  let -- This should be called only once per 'IdOf Cxx.FileXRefMap'
      oneFileXRefMap
        :: XRefs
        -> (IdOf Cxx.FileXRefMap, Cxx.FileXRefMap_key)
        -> [(ByteRange, [Cxx.Declaration])]
      oneFileXRefMap xrefs (i, key) =
        let fixedTargets =
              [ (range, [decl])
              | Cxx.FixedXRef target from <- Cxx.fileXRefMap_key_fixed key
              , (Cxx.XRefTarget_declaration decl)
                    <- mapMaybe (resolve indirects) [target]
              , range <- fromToSpansAndExpansions from
              ]

            matched = zip froms $ V.toList targets
              where
                froms = Cxx.fileXRefMap_key_froms key
                targets = case PredMap.lookup i xrefs of
                  Nothing -> mempty
                  Just (_, xs) -> xs

            variableTargets =
              [ (range, targetDecls)
              | (from, extTargets) <- matched
              , let !targetDecls = Set.toList $ Set.fromList $
                      [ decl
                      | (Cxx.XRefTarget_declaration decl)
                             <- mapMaybe (resolve indirects)
                                $ HashSet.toList extTargets]
              , range <- fromToSpansAndExpansions from
              ]

            newTargets = fixedTargets ++ variableTargets

        in newTargets

  let fileWorker writer (file, fileXRefMaps) = do
        logExceptions file $ do
          xrefs <- getFileXRefsFor e (map fst fileXRefMaps) cfg
          targetss <- forM fileXRefMaps $ \ fileXRefMap -> do
            let newTargets = oneFileXRefMap xrefs fileXRefMap
            _ <- evaluate (force (length newTargets))
            return newTargets
          void $ generateCalls writer file targetss
        -- continue with the next incoming item ignoring errors
        -- we'd rather get a DB with missing derived facts
        -- than no DB at all
        where
            logExceptions (Src.File fid file) action =
              action `catchAll` \e ->
                if
                  | Just ChannelException{} <- fromException e ->
                    throwIO e
                  | otherwise -> do
                    logError $ printf
                      "Unhandled exception deriving predicates for file %s:\n%s\n"
                      (maybe (show fid) Text.unpack file) (show e)

  -- ---------------------------------------------------------------------------

  streamWithState doFoldEach writers fileWorker

  when (cfgBenchmark cfg) $ do
    foldEachTime <- toDiffMicros <$> getElapsedTime startTimePoint
    matchingTime <- readIORef matchingTimeRef
    logInfo $ "foldEach time : "
           ++ show (fromIntegral foldEachTime / 1000000 :: Double) ++ " seconds"
    logInfo $ "matching time : "
           ++ show (fromIntegral matchingTime / 1000000 :: Double) ++ " seconds"

  when (cfgDebugPrintReferences cfg) $ do
    putStrLn "x-ref count by declarations kind:"
    summary <- readIORef summaryRef
    forM_ (Map.toList summary) $ \((sname, tname), count) -> do
        Text.IO.putStrLn $ Text.concat
          [ predicateRef_name sname
          , " -> "
          , predicateRef_name tname
          , ": "
          , Text.pack (show count) ]
        let predicateRefs = Set.fromList $
              map fst (Map.keys summary) ++ map snd (Map.keys summary)
        forM_ (Set.toList predicateRefs) $ \predicateRef ->
          putStr $ "," ++ Text.unpack (predicateRef_name predicateRef)
        putStrLn ""
        forM_ (Set.toList predicateRefs) $ \s -> do
          putStr $ Text.unpack $ predicateRef_name s
          forM_ (Set.toList predicateRefs) $ \t ->
            putStr $ "," ++ show (Map.findWithDefault 0 (s,t) summary)
          putStrLn ""

  logInfo "deriveFunctionCalls done"
  where
    -- Number of worker threads consuming from incomingQ and writing to Glean
    workers = max 1 (cfgWorkerThreads cfg)
