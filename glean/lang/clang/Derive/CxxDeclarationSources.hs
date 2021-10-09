-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE ApplicativeDo, TypeApplications #-}
-- | Reversing DeclarationTargets into DeclarationSources
--
-- * Load the DeclarationTargets which hold (source, [target])
--   into vector of (target, source)
--
-- * Sort them by target
--
-- * Group them by target
--
-- * convert the group into (target, [source])
--
-- * Make this into DeclarationSources facts
--
-- * chunk the facts and write to Glean
module Derive.CxxDeclarationSources
  ( deriveCxxDeclarationSources
  ) where

import Data.Int (Int64)
import qualified Data.Vector.Algorithms.Intro as VSort
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word8)
import Util.Log (logInfo)

import Glean
import qualified Glean.Schema.Cxx1.Types as Cxx
import Glean.Util.Declarations (getDeclId)
import qualified Glean.Util.ValueBuffer as Buffer

import Derive.Types

-- -----------------------------------------------------------------------------

-- | This represents which 'Cxx.Declaration' constructor is being
-- used.  This means we will not depend on each predicate
-- having a non-overlapping space of fact Ids.
type DeclTag x = x

-- | Get the branch index.
toDeclTag :: Cxx.Declaration -> DeclTag Word8
toDeclTag = \case
  Cxx.Declaration_namespace_{} -> 0
  Cxx.Declaration_usingDeclaration{} -> 1
  Cxx.Declaration_usingDirective{} -> 2
  Cxx.Declaration_record_{} -> 3
  Cxx.Declaration_enum_{} -> 4
  Cxx.Declaration_function_{} -> 5
  Cxx.Declaration_variable{} -> 6
  Cxx.Declaration_objcContainer{} -> 7
  Cxx.Declaration_objcMethod{} -> 8
  Cxx.Declaration_objcProperty{} -> 9
  Cxx.Declaration_typeAlias{} -> 10

-- | Reconstruct the 'Cxx.Declaration' from the branch index (creatged by
-- 'toDeclTag') and the fact id.
fromDeclTag :: DeclTag Word8 -> Int64 -> Cxx.Declaration
fromDeclTag n i = case n of
  0 -> Cxx.Declaration_namespace_ $! justId (IdOf (Fid i))
  1 -> Cxx.Declaration_usingDeclaration $! justId (IdOf (Fid i))
  2 -> Cxx.Declaration_usingDirective $! justId (IdOf (Fid i))
  3 -> Cxx.Declaration_record_ $! justId (IdOf (Fid i))
  4 -> Cxx.Declaration_enum_ $! justId (IdOf (Fid i))
  5 -> Cxx.Declaration_function_ $! justId (IdOf (Fid i))
  6 -> Cxx.Declaration_variable $! justId (IdOf (Fid i))
  7 -> Cxx.Declaration_objcContainer $! justId (IdOf (Fid i))
  8 -> Cxx.Declaration_objcMethod $! justId (IdOf (Fid i))
  9 -> Cxx.Declaration_objcProperty $! justId (IdOf (Fid i))
  10 -> Cxx.Declaration_typeAlias $! justId (IdOf (Fid i))
  _ -> error "impossible fromDeclTag"

-- | Decompose the declaration into pieces, and it can be
--reconstructed by 'fromDecN'
fromDecl :: Cxx.Declaration -> (DeclTag Word8, Int64)
fromDecl d =  (toDeclTag d, getDeclId d)

-- | Represent an edge from target to source. Put the target's Id
-- first so that sorting will only need the first component of
-- the tuple.
--
-- > (targetId, targetTag, sourceId, sourceTag)
type TargetSource = (Int64, DeclTag Word8, Int64, DeclTag Word8)

type TargetSourcesBuffer = Buffer.IOBuffer VU.Vector TargetSource

type TargetSourcesM = VUM.IOVector TargetSource

type TargetSources = VU.Vector TargetSource

-- | Start with a small 'ValueBuffer', it will grow by doubling as needed.
initialSize :: Int
initialSize = 2^(10 :: Int)

-- -----------------------------------------------------------------------------

addDeclarationTarget
  :: TargetSourcesBuffer
  -> ()
  -> Cxx.DeclarationTargets_key
  -> IO ()
addDeclarationTarget buffer () Cxx.DeclarationTargets_key{..} =
  mapM_ (Buffer.push buffer)
    [ (targetId, targetTag, sourceId, sourceTag)
    | let (sourceTag, sourceId) = fromDecl declarationTargets_key_source
    , target <- declarationTargets_key_targets
    , let (targetTag, targetId) = fromDecl target ]

-- | Create lazy list of facts to write
toDeclarationSources :: TargetSources -> [Cxx.DeclarationSources_key]
toDeclarationSources = loop
  where
    loop v | VU.null v = []
           | otherwise =
      let front = VU.head v  -- head is safe due to null check above
          (matching, next) = VU.span (eqTarget front) v
          this = Cxx.DeclarationSources_key
            { declarationSources_key_target = targetOf front
            , declarationSources_key_sources =
              map sourceOf (VU.toList matching) }
      in this : loop next

    eqTarget (x1, x2, _, _) (y1, y2, _, _) = x1 == y1 && x2 == y2
    targetOf (targetId, targetTag, _, _) = fromDeclTag targetTag targetId
    sourceOf (_, _, sourceId, sourceTag) = fromDeclTag sourceTag sourceId

-- | Write all the 'Cxx.DeclarationSources' facts to Glean
writeDeclSources :: Writer -> TargetSources -> IO ()
writeDeclSources writer targetSources = do
  logInfo "writeDeclSources"
  mapM_ (\f -> writeFacts writer (makeFact_ @Cxx.DeclarationSources f))
    (toDeclarationSources targetSources)
  logInfo "writeDeclSources done"

deriveCxxDeclarationSources :: Backend e => e -> Config -> Writer -> IO ()
deriveCxxDeclarationSources e cfg writer = do
  logInfo "deriveCxxDeclarationSources"
  let
    q :: Query Cxx.DeclarationTargets_key
    q = keys $ maybe id limit (cfgMaxQueryFacts cfg) $
      limitBytes (cfgMaxQuerySize cfg)
      (allFacts :: Query Cxx.DeclarationTargets)
  (targetSourcesM :: TargetSourcesM) <- do
    (buffer :: TargetSourcesBuffer) <- Buffer.new initialSize
    runQueryEach e (cfgRepo cfg) q () (addDeclarationTarget buffer)
    Buffer.get buffer
  logInfo "query complete, got targetSourcesM"
  VSort.sort targetSourcesM
  logInfo "sorted targetSourcesM"
  (targetSources :: TargetSources) <- VU.unsafeFreeze targetSourcesM
  writeDeclSources writer (VU.uniq targetSources)
  logInfo "deriveCxxDeclarationSources done"
