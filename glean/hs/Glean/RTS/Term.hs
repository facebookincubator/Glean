-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE DeriveFoldable, DeriveFunctor, DeriveGeneric, DeriveTraversable #-}
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

-- | Decoded data for a key or value of a predicate.
-- The 'ref' varies: 'Fid', 'Match'
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
