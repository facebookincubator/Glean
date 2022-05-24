{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

{-# LANGUAGE DeriveGeneric, DeriveTraversable #-}
module Glean.RTS.Term
  ( Term(..)
  , Match(..)
  , Value
  , Pattern
  ) where

import Data.ByteString (ByteString)
import Data.Hashable
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Encoding.Error as Text
import Data.Text.Prettyprint.Doc
import Data.Word (Word8, Word64)
import GHC.Generics hiding (Rep)

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
  deriving(Eq, Generic, Show, Functor, Foldable, Traversable)

instance Hashable ref => Hashable (Term ref)

instance Pretty ref => Pretty (Term ref) where
  pretty (Byte x) = "#" <> pretty x
  pretty (Nat x) = pretty x
  pretty (Array xs) = align $ encloseSep "[" "]" "," $ map pretty xs
  pretty (ByteArray xs) = pretty (show xs) <> "#"
  pretty (Tuple xs) = align $ encloseSep "{" "}" "," $ map pretty xs
  pretty (Alt s x) = "(" <> pretty s <> "|" <> pretty x <> ")"
  -- pretty (String s) = "\"" <> pretty s <> "\""
  pretty (String s) =
    "\"" <> pretty (Text.decodeUtf8With Text.lenientDecode s) <> "\""
  pretty (Ref ref) = pretty ref

-- | Typically 'ref' is 'Fid', 'Nested', 'Deep'
data Match ref
  = Variable
  | Wildcard
  | MatchTerm ref
  | PrefixVariable ByteString
  | PrefixWildcard ByteString
  deriving(Foldable,Functor,Traversable,Show)

instance Pretty ref => Pretty (Match ref) where
  pretty Variable = "?"
  pretty Wildcard = "_"
  pretty (MatchTerm ref) = pretty ref
  pretty (PrefixVariable s) = "\"" <> pretty (Text.decodeUtf8 s) <> "\"?.."
  pretty (PrefixWildcard s) = "\"" <> pretty (Text.decodeUtf8 s) <> "\"..."

type Value = Term Fid

type Pattern ref = Term (Match ref)
