{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Codegen.QueryRegs
  ( QueryRegs(..)
  ) where

import Glean.Bytecode.Types
import Glean.RTS.Bytecode.Code

data QueryRegs = QueryRegs
  {
    -- | Start a new traversal of facts beginning with a given prefix
    seek
     :: Register 'Word -- predicate id
     -> Register 'DataPtr -- prefix
     -> Register 'DataPtr -- prefix end
     -> Register 'Word -- (output) token
     -> Code ()

  , seekWithinSection
     :: Register 'Word -- predicate id
     -> Register 'DataPtr -- prefix
     -> Register 'DataPtr -- prefix end
     -> Register 'Word -- section start
     -> Register 'Word -- section end
     -> Register 'Word -- (output) token
     -> Code ()

    -- | Fetch the current seek token
  , currentSeek
     :: Register 'Word
     -> Code ()

    -- | Release the state associated with an iterator token
  , endSeek
     :: Register 'Word
     -> Code ()

    -- | Grab the next fact in a traversal
  , next
     :: Register 'Word     -- token
     -> Bool               -- do we need the value?
     -> Register 'Word     -- result
     -> Register 'DataPtr  -- clause begin
     -> Register 'DataPtr  -- key end
     -> Register 'DataPtr  -- clause end
     -> Register 'Word     -- id
     -> Code ()

    -- | Fact lookup
  , lookupKeyValue
     :: Register 'Word
     -> Register 'BinaryOutputPtr
     -> Register 'BinaryOutputPtr
     -> Register 'Word
     -> Code ()

    -- | Record a result
  , result
     :: Register 'Word
     -> Register 'BinaryOutputPtr
     -> Register 'BinaryOutputPtr
     -> Register 'Word
     -> Code ()

    -- | Record a result, with a given pid and optional recursive expansion
  , resultWithPid
     :: Register 'Word
     -> Register 'BinaryOutputPtr
     -> Register 'BinaryOutputPtr
     -> Register 'Word
     -> Register 'Word
     -> Code ()

    -- | Record a new derived fact
  , newDerivedFact
     :: Register 'Word
     -> Register 'BinaryOutputPtr
     -> Register 'Word
     -> Register 'Word
     -> Code ()

  , firstFreeId
    :: Register 'Word -- first free id
    -> Code ()

  , newSet
    :: Register 'Word -- (output) set token
    -> Code ()

  , insertOutputSet
    :: Register 'Word -- set token
    -> Register 'BinaryOutputPtr
    -> Code ()

  , setToArray
    :: Register 'Word -- set token
    -> Register 'BinaryOutputPtr -- (output) array
    -> Code ()

  , freeSet
    :: Register 'Word -- set token (invalid after this call)
    -> Code ()

  , newWordSet
    :: Register 'Word -- (output) set token
    -> Code ()

  , insertWordSet
    :: Register 'Word -- set token
    -> Register 'Word
    -> Code ()

  , wordSetToArray
    :: Register 'Word -- set token
    -> Register 'BinaryOutputPtr -- (output) array
    -> Code ()

  , byteSetToByteArray
    :: Register 'Word -- set token
    -> Register 'BinaryOutputPtr -- (output) array
    -> Code ()

  , freeWordSet
    :: Register 'Word -- set token (invalid after this call)
    -> Code ()

    -- | Unused, temporarily kept for backwards compatibility
  , saveState :: Register 'Word

    -- | Maximum number of results to return
  , maxResults :: Register 'Word

    -- | Maximum number of bytes to return
  , maxBytes :: Register 'Word
  }
