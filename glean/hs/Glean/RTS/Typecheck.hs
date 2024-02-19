{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

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
  :: Register ('Fun '[ 'Word, 'Word, 'WordPtr ])
  -> Register 'DataPtr
  -> Register 'DataPtr
  -> Register 'BinaryOutputPtr
  -> Type
  -> Code ()
typecheck rename input inputend output = tc
  where
    tc ByteTy = do
      size <- constant 1
      local $ \ptr -> do
        move input ptr
        inputBytes input inputend size
        outputBytes ptr input output
    tc NatTy = local $ \reg -> do
      inputNat input inputend reg
      outputNat reg output
    tc StringTy =
      local $ \ptr -> do
        move input ptr
        inputSkipUntrustedString input inputend
        outputBytes ptr input output
    tc (ArrayTy elty) = local $ \size -> do
      inputNat input inputend size
      outputNat size output
      case derefType elty of
        ByteTy -> local $ \ptr -> do
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
    tc (RecordTy fields) = mapM_ (tc . fieldDefType) fields
    tc (SumTy fields) = mdo
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
    tc (SetTy _elty) = undefined
    tc (PredicateTy (PidRef (Pid pid) _)) = local $ \ide -> do
      t <- constant $ fromIntegral pid
      inputNat input inputend ide
      callFun_2_1 rename ide t ide
      outputNat ide output
    tc (NamedTy (ExpandedType _ ty)) = tc ty
    tc (MaybeTy ty) = mdo
      local $ \sel -> do
        inputNat input inputend sel
        outputNat sel output
        select sel [end,just]
      raise "maybe selector out of range"
      just <- label
      tc ty
      end <- label
      return ()
    tc (EnumeratedTy names) = tcEnum $ fromIntegral $ length names
    tc BooleanTy = tcEnum 2

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
checkType ty = checkSignature ty $ RecordTy []

-- | Generate a subroutine which typechecks and substitutes a clause. It has
-- the following arguments:
--
-- std::function<Id(Id id, Id type)> - fact substitution
-- const void * - begin of clause/key
-- const void * - end of key/begin of value
-- const void * - end of clause/value
--
-- It returns the substituted clause in the first output and size of the
-- substituted key in the first local register.
--
checkSignature :: Type -> Type -> IO (Subroutine CompiledTypecheck)
checkSignature key_ty val_ty =
  fmap snd $ generate Optimised $
    \rename clause_begin key_end clause_end -> output $ \out ->
    -- We return the key size in the first local register
    local $ \key_size -> mdo
    typecheck rename clause_begin key_end out key_ty
    check "key" clause_begin key_end
    getOutputSize out key_size
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
