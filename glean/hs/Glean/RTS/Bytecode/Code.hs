{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.RTS.Bytecode.Code
  ( Code
  , Register
  , Registers
  , Label
  , Optimised(..)
  , literal
  , label
  , issueFallThrough
  , issueCondJump
  , issueUncondJump
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
import qualified Control.Monad.Trans.State.Strict as S
import Data.Bifunctor (first)
import Data.Bits
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
import Glean.RTS.Foreign.Bytecode (Subroutine, subroutine)

-- | A basic block
newtype Block = Block
  { -- | Instructions (reversed)
    blockInsns :: [Insn]
  }

data CodeS = CodeS
  { -- | Label of current block
    csLabel :: {-# UNPACK #-} !Label

    -- | Instructions in current block (reversed)
  , csInsns :: [Insn]

    -- | Blocks produced so far (reversed)
  , csBlocks :: [Block]

    -- | All constant values used by the subroutine. These will be preloaded
    -- into registers at the start.
  , csConstants :: [Word64]

    -- | Cached `length csConstants`
  , csNextConstant :: !(Register 'Word)

    -- | Map known constants to their registers
  , csConstantMap :: IntMap (Register 'Word)

    -- | All literals in the subroutine.
  , csLiterals :: HashMap ByteString Word64

    -- | Cached `length csLiterals`
  , csLiteralsSize :: !Word64

    -- | Currently used number of local registers
  , csNextLocal :: {-# UNPACK #-} !(Register 'Word)

    -- | Maximum number of local registers used so far
  , csMaxLocal :: {-# UNPACK #-} !(Register 'Word)

    -- | Currently used number of binary::Output registers
  , csNextOutput :: {-# UNPACK #-} !(Register 'BinaryOutputPtr)

    -- | Maximum number of binary::Output registers
  , csMaxOutputs :: {-# UNPACK #-} !(Register 'BinaryOutputPtr)
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
  case IntMap.lookup (fromIntegral w) csConstantMap of
    Just r -> return r
    Nothing -> do
      S.put s
        { csConstants = w : csConstants
        , csNextConstant = succ csNextConstant
        , csConstantMap =
            IntMap.insert (fromIntegral w) csNextConstant csConstantMap
        }
      return csNextConstant

-- | Generate a chunk of code with a reserved fresh register. The register can
-- be reused afterwards.
local :: (Register t -> Code a) -> Code a
local f = do
  CodeS{..} <- Code S.get
  Code $ S.modify' $ \s -> s
    { csNextLocal = succ csNextLocal
    , csMaxLocal = max csMaxLocal (succ csNextLocal) }
  x <- f $ castRegister csNextLocal
  Code $ S.modify' $ \s -> s { csNextLocal = csNextLocal }
  return x

locals :: Int -> ([Register t] -> Code a) -> Code a
locals 0 f = f []
locals n f = do
  CodeS{..} <- Code S.get
  let !next = offsetRegister csNextLocal n
  Code $ S.modify' $ \s -> s
    { csNextLocal = next
    , csMaxLocal = max csMaxLocal next }
  x <- f $ coerce [csNextLocal .. pred next]
  Code $ S.modify' $ \s -> s { csNextLocal = csNextLocal }
  return x

-- | Declare a register of type @Register 'BinaryOutputPtr@.  The register
-- is one of the inputs to the subroutine. Use 'resetOutput' to reset the
-- byte array stored in this register to empty.
outputs :: Int -> ([Register 'BinaryOutputPtr] -> Code a) -> Code a
outputs 0 f = f []
outputs n f = do
  CodeS{..} <- Code S.get
  let !next = offsetRegister csNextOutput n
  Code $ S.modify' $ \s -> s
    { csNextOutput = next
    , csMaxOutputs = max csMaxOutputs next }
  x <- f [csNextOutput .. pred next]
  Code $ S.modify' $ \s -> s { csNextOutput = csNextOutput }
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
data CallSite = CallSite
  { callSiteNextLocal :: Register 'Word
  , callSiteNextOutput :: Register 'BinaryOutputPtr
  }

callSite :: Code CallSite
callSite = do
  CodeS{..} <- Code S.get
  return (CallSite csNextLocal csNextOutput)

calledFrom :: [CallSite] -> Code a -> Code a
calledFrom frames inner = do
  CodeS{..} <- Code S.get
  Code $ S.modify' $ \s -> s
    { csNextLocal = maximum (map callSiteNextLocal frames)
    , csNextOutput = maximum (map callSiteNextOutput frames) }
  x <- inner
  Code $ S.modify' $ \s -> s
    { csNextLocal = csNextLocal
    , csNextOutput = csNextOutput }
  return x

-- | Tuples of registers
class Registers a where
  registers :: Register 'Word -> (a, Register 'Word)

instance Registers () where
  registers r = ((), r)

instance Registers (Register t) where
  registers r = (castRegister r, succ r)

instance (Registers a, Registers b) => Registers (a,b) where
  registers i0 =
    let (a, i1) = registers i0
        (b, i2) = registers i1
    in
    ((a,b), i2)

instance (Registers a, Registers b, Registers c) => Registers (a,b,c) where
  registers i0 =
    let (a, i1) = registers i0
        (b, i2) = registers i1
        (c, i3) = registers i2
    in
    ((a,b,c), i3)

instance
    (Registers a, Registers b, Registers c, Registers d)
    => Registers (a,b,c,d) where
  registers i0 =
    let (a, i1) = registers i0
        (b, i2) = registers i1
        (c, i3) = registers i2
        (d, i4) = registers i3
    in
    ((a,b,c,d), i4)

instance
    (Registers a, Registers b, Registers c, Registers d, Registers e)
    => Registers (a,b,c,d,e) where
  registers i0 =
    let (a, i1) = registers i0
        (b, i2) = registers i1
        (c, i3) = registers i2
        (d, i4) = registers i3
        (e, i5) = registers i4
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
  | (input, nextInput) <- registers (register Input 0) =
  case S.runState (runCode $ gen input) CodeS
        { csLabel = Label 0
        , csInsns = []
        , csBlocks = []
        , csConstants = []
        , csConstantMap = IntMap.empty
        , csNextConstant = register Constant 0
        , csLiterals = HashMap.empty
        , csLiteralsSize = 0
        , csNextLocal = register Local 0
        , csMaxLocal = register Local 0
        , csNextOutput = castRegister nextInput
        , csMaxOutputs = castRegister nextInput } of
    ((), CodeS{..}) -> do
      -- sanity check
      when (not $ null csInsns) $ fail "unterminated basic block"
      let -- output registers go after input registers
          finalInputSize = registerIndex csMaxOutputs
          constantsSize = registerIndex csNextConstant

          get_label pc label = (offsets VP.! fromLabel label) - pc

          get_reg :: forall ty . Register ty -> Word64
          get_reg r = case registerSegment r of
            Input -> assert (n < finalInputSize) n
            Constant -> assert (n < constantsSize) (n + finalInputSize)
            Local -> n + finalInputSize + constantsSize
            where
              !n = registerIndex r

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
        (finalInputSize - registerIndex nextInput)
        (registerIndex csMaxLocal + constantsSize)
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
              Block { blockInsns = mapLabels relabel <$> inline i blockInsns })
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

    -- | Insn stream (all reversed)
  , layoutInsns :: [[Insn]]

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
layout :: CFG -> ([Insn], VP.Vector Word64)
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
    emit :: Label -> Layout s -> ST s ([Insn], VP.Vector Word64)
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

-- | Frame segment which a register belongs to
data Segment = Input | Constant | Local
  deriving(Eq,Ord,Enum,Bounded,Show)

-- A register has a 'Segment' and an index within the segment, packed into
-- a word.

-- | Create a register with the given 'Segment' and index
register :: Segment -> Word64 -> Register a
register s i = Register $ (fromIntegral (fromEnum s) `shiftL` 62) .|. i

-- | Get the 'Segment' of the register
registerSegment :: Register a  -> Segment
registerSegment (Register i) = toEnum $ fromIntegral (i `shiftR` 62)

-- | Get the index of the register within its segment
registerIndex :: Register a -> Word64
registerIndex (Register i) = i .&. 0x3FFFFFFFFFFFFFFF

-- | Produce a register with its index offset by `n` from the original one
offsetRegister :: Register a -> Int -> Register a
offsetRegister (Register i) n = Register (i + fromIntegral n)

castRegister :: Register a -> Register b
castRegister = coerce

-- | loadReg is fixed to Word, so make a polymorphic version
move :: Register a -> Register a -> Code ()
move src dst = issueFallThrough $ LoadReg (castRegister src) (castRegister dst)

advancePtr :: Register 'DataPtr -> Register 'Word -> Code ()
advancePtr ptr off = issueFallThrough $ Add off (castRegister ptr)

-- | Start a new basic block. The previous block will be terminated by the
-- supplied unconditional jump instruction or, if none is provided, by a jump
-- to the new block.
newBlock :: Maybe Insn -> Code ()
newBlock terminator = Code $ do
  s@CodeS{..} <- S.get
  let !label = succ csLabel
      insn = fromMaybe (Jump label) terminator
  S.put $! s
    { csLabel = label
    , csInsns = []
    , csBlocks = Block (insn : csInsns) : csBlocks
    }

-- | Add a literal to the literal table and yield its index
literal :: ByteString -> Code Word64
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

-- | Yield a label for the current position in the code
label :: Code Label
label = do
  insns <- Code $ S.gets csInsns
  when (not $ null insns) $ newBlock Nothing
  Code $ S.gets csLabel

issue :: Insn -> Code ()
issue insn = Code $ S.modify' $ \s@CodeS{..} -> s { csInsns = insn : csInsns }

-- | Issue an instruction which doesn't modify the program counter
issueFallThrough :: Insn -> Code ()
issueFallThrough = issue

-- | Issue an instruction which might modify the program counter
issueCondJump :: Insn -> Code ()
issueCondJump insn = do
  issue insn
  newBlock Nothing

-- | Issue an instruction which always modifies the program counter
issueUncondJump :: Insn -> Code ()
issueUncondJump = newBlock . Just
