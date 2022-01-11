{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Bytecode.Code
  ( MonadInsn(..)
  , Code
  , Register
  , Registers
  , Label
  , Optimised(..)
  , constant
  , local
  , locals
  , output
  , outputs
  , generate
  , castRegister
  , move
  , advancePtr
  , callSite
  , calledFrom
  ) where

import Control.Exception (assert)
import Control.Monad
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.ST (ST, runST)
import qualified Control.Monad.Trans.State.Lazy as S
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Functor
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (mapAccumL, sortBy)
import Data.Maybe
import Data.Ord
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import qualified Data.Vector.Storable as VS
import Data.Word (Word64)

import Glean.Bytecode.Types
import Glean.RTS.Bytecode.Gen.Instruction
import Glean.RTS.Bytecode.Gen.Issue
import Glean.RTS.Bytecode.MonadInsn
import Glean.RTS.Foreign.Bytecode (Subroutine, subroutine)

type Instruction = Insn Register Label

-- | A basic block
newtype Block = Block
  { -- | Instructions (reversed)
    blockInsns :: [Instruction]
  }

data CodeS = CodeS
  { -- | Label of current block
    csLabel :: {-# UNPACK #-} !Label

    -- | Instructions in current block (reversed)
  , csInsns :: [Instruction]

    -- | Blocks produced so far (reversed)
  , csBlocks :: [Block]

    -- | All constant values used by the subroutine. These will be preloaded
    -- into registers at the start.
  , csConstants :: [Word64]

    -- | Cached `length csConstants`
  , csConstantsSize :: !Word64

    -- | Map known constants to their offsets
  , csConstantMap :: IntMap Word64

    -- | All literals in the subroutine.
  , csLiterals :: HashMap ByteString Word64

    -- | Cached `length csLiterals`
  , csLiteralsSize :: !Word64

    -- | Currently used number of local registers
  , csLocals :: {-# UNPACK #-} !Word64

    -- | Maximum number of local registers used so far
  , csMaxLocal :: {-# UNPACK #-} !Word64

    -- | Currently used number of binary::Output registers
  , csOutputs :: {-# UNPACK #-} !Word64

    -- | Maximum number of binary::Output registers
  , csMaxOutputs :: {-# UNPACK #-} !Word64
  }

-- | Code gen monad
newtype Code a = Code { runCode :: S.State CodeS a }
  deriving(Functor, Applicative, Monad, MonadFix)


-- | Load a constant value into a register. This will happen at the start of
-- the subroutine, effectively giving us a poor man's version of constant
-- hoisting.
constant :: Word64 -> Code (Register 'Word)
constant w = Code $ do
  s@CodeS{..} <- S.get
  Register Constant <$> case IntMap.lookup (fromIntegral w) csConstantMap of
    Just i -> return i
    Nothing -> do
      S.put s
        { csConstants = w : csConstants
        , csConstantsSize = csConstantsSize + 1
        , csConstantMap =
            IntMap.insert (fromIntegral w) csConstantsSize csConstantMap
        }
      return csConstantsSize

-- | Generate a chunk of code with a reserved fresh register. The register can
-- be reused afterwards.
local :: (Register t -> Code a) -> Code a
local f = do
  CodeS{..} <- Code S.get
  Code $ S.modify' $ \s -> s
    { csLocals = csLocals + 1
    , csMaxLocal = max csMaxLocal (csLocals + 1) }
  x <- f $ Register Local csLocals
  Code $ S.modify' $ \s -> s { csLocals = csLocals }
  return x

locals :: Int -> ([Register t] -> Code a) -> Code a
locals 0 f = f []
locals n f = do
  CodeS{..} <- Code S.get
  Code $ S.modify' $ \s -> s
    { csLocals = csLocals + fromIntegral n
    , csMaxLocal = max csMaxLocal (csLocals + fromIntegral n) }
  x <- f $ map (Register Local) [csLocals .. csLocals + fromIntegral n - 1]
  Code $ S.modify' $ \s -> s { csLocals = csLocals }
  return x

-- | Declare a register of type @Register 'BinaryOutputPtr@.  The register
-- is one of the inputs to the subroutine. Use 'resetOutput' to reset the
-- byte array stored in this register to empty.
outputs :: Int -> ([Register 'BinaryOutputPtr] -> Code a) -> Code a
outputs 0 f = f []
outputs n f = do
  CodeS{..} <- Code S.get
  Code $ S.modify' $ \s -> s
    { csOutputs = csOutputs + fromIntegral n
    , csMaxOutputs = max csMaxOutputs (csOutputs + fromIntegral n) }
  x <- f $ map (Register Input) [csOutputs .. csOutputs + fromIntegral n - 1]
  Code $ S.modify' $ \s -> s { csOutputs = csOutputs }
  return x

output :: (Register 'BinaryOutputPtr -> Code a) -> Code a
output f = outputs 1 (f . head)

-- | Poor man's function calls
--
-- * Put the label of the return address into a register with
--   'loadReg', jump to the code, and return with 'jumpReg'.
--
-- * We have to avoid local registers at the call site(s) clashing
--   with local registers in the called code.  So 'callSite' remembers
--   the number of locals in scope at the call site(s) and
--   'calledCode' uses the high-water mark of the call sites as the
--   base for its local registers.
--
data CallSite = CallSite { callSiteLocals :: Word64, callSiteOutputs :: Word64 }

callSite :: Code CallSite
callSite = do
  CodeS{..} <- Code S.get
  return (CallSite csLocals csOutputs)

calledFrom :: [CallSite] -> Code a -> Code a
calledFrom frames inner = do
  CodeS{..} <- Code S.get
  Code $ S.modify' $ \s -> s
    { csLocals = maximum (map callSiteLocals frames)
    , csOutputs = maximum (map callSiteOutputs frames) }
  x <- inner
  Code $ S.modify' $ \s -> s { csLocals = csLocals, csOutputs = csOutputs }
  return x

-- | Tuples of registers
class Registers a where
  registers :: Segment -> Word64 -> (a, Word64)

instance Registers () where
  registers _ i = ((), i)

instance Registers (Register t) where
  registers s i = (Register s i, i+1)

instance (Registers a, Registers b) => Registers (a,b) where
  registers s i0 =
    let (a, i1) = registers s i0
        (b, i2) = registers s i1
    in
    ((a,b), i2)

instance (Registers a, Registers b, Registers c) => Registers (a,b,c) where
  registers s i0 =
    let (a, i1) = registers s i0
        (b, i2) = registers s i1
        (c, i3) = registers s i2
    in
    ((a,b,c), i3)

instance
    (Registers a, Registers b, Registers c, Registers d)
    => Registers (a,b,c,d) where
  registers s i0 =
    let (a, i1) = registers s i0
        (b, i2) = registers s i1
        (c, i3) = registers s i2
        (d, i4) = registers s i3
    in
    ((a,b,c,d), i4)

instance
    (Registers a, Registers b, Registers c, Registers d, Registers e)
    => Registers (a,b,c,d,e) where
  registers s i0 =
    let (a, i1) = registers s i0
        (b, i2) = registers s i1
        (c, i3) = registers s i2
        (d, i4) = registers s i3
        (e, i5) = registers s i4
    in
    ((a,b,c,d,e), i5)

data Optimised = Optimised | Unoptimised
  deriving(Eq,Ord,Enum,Bounded,Show)

-- | Generate a 'Subroutine', allocating input registers as necessary.
-- Example:
--
-- generate $ \(reg1,reg2) -> do
--   add reg1 reg2 reg1
--   ret
--
generate
  :: Registers input => Optimised -> (input -> Code ()) -> IO (Subroutine t)
generate opt gen
  | (input, inputSize) <- registers Input 0 =
  case S.runState (runCode $ gen input) CodeS
        { csLabel = Label 0
        , csInsns = []
        , csBlocks = []
        , csConstants = []
        , csConstantMap = IntMap.empty
        , csConstantsSize = 0
        , csLiterals = HashMap.empty
        , csLiteralsSize = 0
        , csLocals = 0
        , csMaxLocal = 0
        , csOutputs = inputSize
        , csMaxOutputs = inputSize } of
    ((), CodeS{..}) -> do
      -- sanity check
      when (not $ null csInsns) $ fail "unterminated basic block"
      let -- output registers go after input registers
          finalInputSize = csMaxOutputs

          get_label pc label = (offsets VP.! fromLabel label) - pc

          get_reg :: forall ty . Register ty -> Word64
          get_reg (Register Input n) = assert (n < finalInputSize) n
          get_reg (Register Constant n) = assert (n < csConstantsSize)
            (n + finalInputSize)
          get_reg (Register Local n) = n + finalInputSize + csConstantsSize

          optimise = case opt of
            Optimised -> shortcut
            Unoptimised -> id

          (insns, offsets) = layout $ optimise CFG
            { cfgBlocks = V.fromListN (fromLabel csLabel) $ reverse csBlocks
            , cfgEntry = Label 0
            }

          code = concat $ snd $ mapAccumL
            (\offset insn ->
              let !next = offset + insnSize insn
              in
              (next, insnWords get_reg (get_label next) insn))
            0
            insns
      subroutine
        (VS.fromListN (length code) code)
        finalInputSize
        (csMaxOutputs - inputSize)
        (csMaxLocal + csConstantsSize)
        (reverse csConstants)
        (map fst $ sortBy (comparing snd) $ HashMap.toList csLiterals)


-- | Control flow graph
data CFG = CFG
  { -- | Basic blocks
    cfgBlocks :: !(V.Vector Block)

    -- | Entry block
  , cfgEntry :: {-# UNPACK #-} !Label
  }

-- | Inline blocks containing only one instruction and short-circuit labels
-- which point to blocks consisting of a single Jump.
--
-- NOTE: This will leave behind unreachable blocks.
shortcut :: CFG -> CFG
shortcut cfg@CFG{..}
  | V.all isNothing shortcuts = cfg
  | otherwise = CFG
      { cfgBlocks = V.imap
          (\i Block{..} ->
                Block { blockInsns = fmap relabel <$> inline i blockInsns })
          cfgBlocks
      , cfgEntry = relabel cfgEntry
      }
  where
    inline i _
      | Just insn <- shortcuts V.! i = [insn]
    inline _ (Jump target : insns)
      | Just insn <- shortcuts V.! fromLabel target = insn : insns
    inline _ insns = insns

    -- FIXME: This can loop if the generated code contains infinite loops
    shortcuts = cfgBlocks <&> \Block{..} -> case blockInsns of
      [insn] -> Just $ case insn of
        Jump target | Just insn' <- shortcuts V.! fromLabel target -> insn'
        _ -> insn
      _ -> Nothing

    relabel label
      | Just (Jump target) <- shortcuts V.! fromLabel label = target
      | otherwise = label

data Layout s = Layout
  { -- | Current offset in the instruction stream
    layoutOffset :: {-# UNPACK #-} !Word64

    -- | Label offsets
  , layoutLabels :: !(VPM.STVector s Word64)

    -- | Instruction stream (all reversed)
  , layoutInsns :: [[Instruction]]

    -- | Blocks not yet emitted
  , layoutTodo :: !IntSet
  }

-- | Compute a flat instruction stream for a subroutine as well as a mapping
-- from labels to their offsets in that stream.
--
-- This is really simple at the moment.
--
-- * Start with the entry block.
-- * If the current block ends with an unconditional jump and the block it jumps
--   to hasn't been emitted yet, continue with that block, thus saving the jump.
-- * Otherwise, continue with the unemitted block with lowest label number.
--
-- There is obviously ample room for improvement here.
layout :: CFG -> ([Instruction], VP.Vector Word64)
layout CFG{..} = runST $ do
  mlabels <- VPM.new $ V.length cfgBlocks
  emit cfgEntry Layout
    { layoutOffset = 0
    , layoutLabels = mlabels
    , layoutInsns = []
    , layoutTodo =
        IntSet.delete (fromLabel cfgEntry)
        $ IntSet.fromDistinctAscList [0 .. V.length cfgBlocks - 1]
    }
  where
    emit :: Label -> Layout s -> ST s ([Instruction], VP.Vector Word64)
    emit !label Layout{..} = do
      VPM.write layoutLabels (fromLabel label) layoutOffset

      let (next, insns) = case blockInsns (cfgBlocks V.! fromLabel label) of
            -- Handle unconditional jumps specially
            Jump target : insns
              | IntSet.member (fromLabel target) layoutTodo ->
                ( Just (target, IntSet.delete (fromLabel target) layoutTodo)
                , insns)

            -- Pick the numerically first block
            insns -> (first Label <$> IntSet.minView layoutTodo, insns)

      case next of
        Just (target, todo) -> emit target Layout
          { layoutOffset = layoutOffset + sum (map insnSize insns)
          , layoutLabels = layoutLabels
          , layoutInsns = insns : layoutInsns
          , layoutTodo = todo
          }

        Nothing -> do
          labels <- VP.unsafeFreeze layoutLabels
          return (reverse $ concat $ insns : layoutInsns, labels)

-- | Code labels
newtype Label = Label { fromLabel :: Int }
  deriving(Eq,Ord,Enum,Num,Show)

-- | Frame segment which a register belongs to
data Segment = Input | Constant | Local
  deriving(Eq,Ord,Enum,Bounded,Show)

-- | Registers
data Register (t :: Ty)
  = Register !Segment {-# UNPACK #-} !Word64
  deriving Show

castRegister :: Register a -> Register b
castRegister = coerce

-- | loadReg is fixed to Word, so make a polymorphic version
move :: Register a -> Register a -> Code ()
move src dst = loadReg (castRegister src) (castRegister dst)

advancePtr :: Register 'DataPtr -> Register 'Word -> Code ()
advancePtr ptr off = add off (castRegister ptr)

newBlock :: Code Label
newBlock = Code $ do
  s@CodeS{..} <- S.get
  let !label = succ csLabel
  S.put $! s
    { csLabel = label
    , csInsns = []
    , csBlocks = Block (jump_to label csInsns) : csBlocks
    }
  return label
  where
    jump_to label insns
      | insn : _ <- insns
      , insnControl insn == UncondJump
        || insnControl insn == UncondReturn = insns
      | otherwise = Jump label : insns

instance MonadInsn Code where
  type Lbl Code = Label
  type Reg Code = Register

  label = do
    CodeS
      { csLabel = label
      , csInsns = insns } <- Code S.get
    if null insns
      then return label
      else newBlock

  issue insn = do
    Code $ S.modify' $ \s@CodeS{..} -> s { csInsns = insn : csInsns }
    case insnControl insn of
      CondJump -> void newBlock
      UncondJump -> void newBlock
      UncondReturn -> void newBlock
      _ -> return ()

  literal lit = Code $ do
    m <- S.gets csLiterals
    case HashMap.lookup lit m of
      Just n -> return n
      Nothing -> do
        n <- S.gets csLiteralsSize
        S.modify' $ \s@CodeS{..} -> s
          { csLiterals = HashMap.insert lit n csLiterals
          , csLiteralsSize = csLiteralsSize + 1 }
        return n
