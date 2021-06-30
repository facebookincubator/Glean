-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE RecursiveDo #-}
module Glean.RTS.Traverse
  ( CompiledTraversal
  , genTraversal
  ) where

import Control.Monad

import Glean.Bytecode.Types
import Glean.RTS.Foreign.Bytecode
import Glean.RTS.Types
import Glean.RTS.Bytecode.Code
import Glean.RTS.Bytecode.Gen.Issue

-- | Type tag for Subroutine
data CompiledTraversal

traversal
  :: Register ('Fun '[ 'Word, 'Word ] 'Void)
  -> Register 'DataPtr
  -> Register 'DataPtr
  -> Type
  -> Code ()
traversal callback input inputend ty = go False (repType ty)
  where
    -- if refs is True, we *must* leave the input pointer pointing
    -- after the value, because we're going to traverse more data.
    --
    -- If refs is False, we *must not* traverse the value unless
    -- (hasRefs ty) is True, because we might not have traversed the
    -- previous value.
    go refs ByteRep = when refs $ addConst 1 (castRegister input)
    go refs NatRep = when refs $ inputSkipNat input inputend
    go refs StringRep = when refs $ inputSkipTrustedString input inputend
    go refs (ArrayRep elty) = when (refs || hasRefs elty) $ local $ \size -> do
      inputNat input inputend size
      case elty of
        ByteRep -> add size (castRegister input)
        _ -> mdo
          jumpIf0 size end
          loop <- label
          go True elty
          decrAndJumpIfNot0 size loop
          end <- label
          return ()
    go refs (TupleRep tys) = mapM_ (uncurry go) $ zip
      (drop 1 $ scanr (\ty refs -> hasRefs ty || refs) refs tys)
      tys
    go refs (SumRep tys)
      | all isUnit tys = when refs $ if length tys <= 127
          then addConst 1 (castRegister input)
          else inputSkipNat input inputend
      | otherwise = when (refs || any hasRefs tys) $ mdo
          local $ \sel -> do
            -- TODO: read byte rather than Nat if arity <= 127
            inputNat input inputend sel
            select sel alts
          raise "selector out of range"
          alts <- forM tys $ \ty -> do
            alt <- label
            go refs ty
            jump end
            return alt
          end <- label
          return ()
    go _ (PredicateRep (Pid pid)) = local $ \ide -> do
      inputNat input inputend ide
      pidr <- constant (fromIntegral pid)
      callFun_2_0 callback ide pidr

-- | Generate a subroutine which traverses a clause (fact key + value)
-- and invokes the supplied callback function for each fact ID
-- contained in it.
--
-- NOTE: The subroutine assumes that the clause is type correct.
genTraversal :: Type -> Type -> IO (Subroutine CompiledTraversal)
genTraversal key_ty val_ty =
  generate Optimised $
    \(handler, (clause_begin, key_end, clause_end)) -> do
    traversal handler clause_begin key_end key_ty
    traversal handler key_end clause_end val_ty
    ret

hasRefs :: Rep a -> Bool
hasRefs ByteRep = False
hasRefs NatRep = False
hasRefs StringRep = False
hasRefs (ArrayRep elty) = hasRefs elty
hasRefs (TupleRep tys) = any hasRefs tys
hasRefs (SumRep tys) = any hasRefs tys
hasRefs PredicateRep{} = True

isUnit :: Rep a -> Bool
isUnit ByteRep = False
isUnit NatRep = False
isUnit StringRep = False
isUnit ArrayRep{} = False
isUnit (TupleRep tys) = all isUnit tys
isUnit SumRep{} = False
isUnit PredicateRep{} = False
