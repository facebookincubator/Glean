{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Bytecode.SysCalls
  ( userQuerySysCalls
  , typecheckSysCalls
  ) where

-- This list has to sync up with the list in CodeGen.hs or
-- the wrong name will be printed
userQuerySysCalls :: [String]
userQuerySysCalls =
  ["seek","seekWithinSection","currentSeek", "endSeek"
  ,"next", "lookupKeyValue", "result", "resultWithPid"
  ,"newDerivedFact", "firstFreeId"
  ,"newSet", "insertOutputSet", "setToArray", "freeSet"
  ,"newWordSet", "insertWordSet", "wordSetToArray", "byteSetToArray"
  ,"freeWordSet"]

-- This list has to sync up with the list in Glean.RTS.Typecheck or
-- the wrong name will be printed
typecheckSysCalls :: [String]
typecheckSysCalls =
  ["rename", "newSet", "insertOutputSet", "setToArray", "freeSet"
  ,"newWordSet_", "insertBytesWordSet_", "wordSetToArray_"
  ,"byteSetToByteArray_", "freeWordSet_"
  ]
