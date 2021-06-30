-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE RecursiveDo #-}
module Glean.RTS.Typecheck
  ( CompiledTypecheck
  , checkType
  , checkSignature
  ) where

import Control.Monad

import Glean.Angle.Types hiding (Type)
import Glean.Bytecode.Types
import Glean.RTS.Foreign.Bytecode
import Glean.RTS.Types
import Glean.RTS.Bytecode.Code
import Glean.RTS.Bytecode.Gen.Issue

-- | Type tag for Subroutine
data CompiledTypecheck

typecheck
  :: Register ('Fun '[ 'Word, 'Word ] 'Word)
  -> Register 'DataPtr
  -> Register 'DataPtr
  -> Register 'BinaryOutputPtr
  -> Type
  -> Code ()
typecheck rename input inputend output = tc
  where
    tc Byte = do
      size <- constant 1
      local $ \ptr -> do
        move input ptr
        inputBytes input inputend size
        outputBytes ptr input output
    tc Nat = local $ \reg -> do
      inputNat input inputend reg
      outputNat reg output
    tc String =
      local $ \ptr -> do
        move input ptr
        inputSkipUntrustedString input inputend
        outputBytes ptr input output
    tc (Array elty) = local $ \size -> do
      inputNat input inputend size
      outputNat size output
      case derefType elty of
        Byte -> local $ \ptr -> do
          move input ptr
          inputBytes input inputend size
          outputBytes ptr input output
        _ -> mdo
          jumpIf0 size end
          loop <- label
          tc elty
          decrAndJumpIfNot0 size loop
          end <- label
          return ()
    tc (Record fields) = mapM_ (tc . fieldDefType) fields
    tc (Sum fields) = mdo
      local $ \sel -> do
        inputNat input inputend sel
        outputNat sel output
        select sel alts
      raise "selector out of range"
      alts <- forM fields $ \(FieldDef _ ty) -> do
        alt <- label
        tc ty
        jump end
        return alt
      end <- label
      return ()
    tc (Predicate (PidRef (Pid pid) _)) = local $ \ide -> do
      t <- constant $ fromIntegral pid
      inputNat input inputend ide
      callFun_2_1 rename ide t ide
      outputNat ide output
    tc (NamedType (ExpandedType _ ty)) = tc ty
    tc (Maybe ty) = mdo
      local $ \sel -> do
        inputNat input inputend sel
        outputNat sel output
        select sel [end,just]
      raise "maybe selector out of range"
      just <- label
      tc ty
      end <- label
      return ()
    tc (Enumerated names) = tcEnum $ fromIntegral $ length names
    tc Boolean = tcEnum 2

    tcEnum arity = mdo
      k <- constant arity
      local $ \sel -> do
        inputNat input inputend sel
        outputNat sel output
        jumpIfLt sel k end
        raise "selector out of range"
      end <- label
      return ()

-- | Generate a subroutine which typechecks and substitutes a value. It has
-- the following arguments:
--
-- std::function<Id(Id id, Id type)> - fact substitution
-- binary::Input * - value
-- binary::Output * - substituted value
--
checkType :: Type -> IO (Subroutine CompiledTypecheck)
checkType ty = generate Optimised  $ \(rename, input, inputend, output) -> do
  typecheck rename input inputend output ty
  ret

-- | Generate a subroutine which typechecks and substitutes a clause. It has
-- the following arguments:
--
-- std::function<Id(Id id, Id type)> - fact substitution
-- const void * - begin of clause/key
-- const void * - end of key/begin of value
-- const void * - end of clause/value
-- binary::Output * - substituted clause
-- uint64_t * - size of substituted key
--
checkSignature :: Type -> Type -> IO (Subroutine CompiledTypecheck)
checkSignature key_ty val_ty =
  generate Optimised $
    \( rename
     , (clause_begin, key_end, clause_end)
     , out
     , out_key_size ) -> mdo
    typecheck rename clause_begin key_end out key_ty
    check "key" clause_begin key_end
    local $ \size -> do
      getOutputSize out size
      storeWord size out_key_size
    typecheck rename clause_begin clause_end out val_ty
    check "value" clause_begin clause_end
    ret
  where
  check str reg1 reg2 = mdo
    jumpIfNe (castRegister reg1) (castRegister reg2) bad
    -- The basic block layout algorithm will make the happy path sequential and
    -- delete this jump.
    jump ok
    bad <- label
    raise $ "extra bytes at end of " <> str
    ok <- label
    return ()
