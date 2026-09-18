module Glean.LLVM (
    Glean.LLVM.getPointerToFunction,
  ) where

import qualified LLVM.FFI.ExecutionEngine as FFI

import LLVM.Core as LLVM
import LLVM.ExecutionEngine as LLVM

import LLVM.Core.CodeGen as LLVM.Private
import LLVM.ExecutionEngine.Engine as LLVM.Private

import Foreign

-- breaking the EngineAccess abstraction from llvm-tf because I don't
-- want to thread the EngineAccess monad through the whole of
-- Glean. Also it probably isn't possible because the server's request
-- handler would have to run in this monad too.

withEngine :: ExecutionEngine -> (FFI.ExecutionEngineRef -> IO a) -> IO a
withEngine = withForeignPtr . LLVM.Private.fromEngine

getPointerToFunction
  :: LLVM.ExecutionEngine
  -> LLVM.Function f
  -> IO (FunPtr f)
getPointerToFunction ee (LLVM.Private.Value f) =
  withEngine ee $ \eePtr -> FFI.getPointerToFunction eePtr f
