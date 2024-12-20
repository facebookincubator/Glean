{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric, DeriveTraversable #-}
module Glean.RTS.Term
  ( Term(..)
  , Value
  ) where

import Data.ByteString (ByteString)
import Data.Hashable
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Compat.Prettyprinter
import Data.Word (Word8, Word64)
import GHC.Generics hiding (Rep)

import Glean.Display
import Glean.RTS.Types (Fid)

-- -----------------------------------------------------------------------------
-- Runtime terms

-- | Haskell representation of a Glean term, i.e. the key or value of
-- a fact. These are stored binary-encoded in the DB (see
-- "Glean.Typed.Binary").
--
-- Parameterised over @ref@, the representation of a fact
-- reference. This type is often extended by instantiating @ref@. For
-- example we represent query patterns by instantiating @ref@ with a
-- pattern type in the query engine, see "Glean.Query.Typecheck.Types".

data Term ref
  = Byte {-# UNPACK #-} !Word8
  | Nat {-# UNPACK #-} !Word64
  | Array [Term ref]
  | ByteArray {-# UNPACK #-} !ByteString
  | Tuple [Term ref]
  | Alt {-# UNPACK #-} !Word64 (Term ref)
  | String {-# UNPACK #-} !ByteString -- utf8-encoded
  | Ref ref
  deriving(Eq, Ord, Generic, Show, Functor, Foldable, Traversable)

instance Hashable ref => Hashable (Term ref)

instance Display ref => Display (Term ref) where
  display _ (Byte x) = "#" <> pretty x
  display _ (Nat x) = pretty x
  display opts (Array xs) =
    align $ encloseSep "[" "]" "," $ map (display opts) xs
  display _ (ByteArray xs) = pretty (show xs) <> "#"
  display opts (Tuple xs) =
    align $ encloseSep "{" "}" "," $ map (display opts) xs
  display opts (Alt s x) = "(" <> pretty s <> "|" <> display opts x <> ")"
  -- display opts (String s) = "\"" <> display opts s <> "\""
  display _ (String s) =
    "\"" <> pretty (Text.decodeUtf8With Text.lenientDecode s) <> "\""
  display opts (Ref ref) = display opts ref

  displayAtom opts (Ref ref) = displayAtom opts ref
  displayAtom opts other = display opts other

type Value = Term Fid
