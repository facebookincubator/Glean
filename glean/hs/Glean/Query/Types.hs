{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

module Glean.Query.Types
  ( SourceQuery_(..)
  , SourceStatement_(..)
  , Name
  , FieldName
  , SourcePat_(..)
  , Field(..)
  , IsWild(..)
  , IsSrcSpan(..)
  , SrcSpan(..)
  , SrcLoc(..)
  , sourcePatSpan
  , spanBetween
  ) where

import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc
import Data.Word

-- -----------------------------------------------------------------------------
-- Source Query Types

data SourceQuery_ head stmt = SourceQuery
  { srcQueryHead :: Maybe head
  , srcQueryStmts :: [stmt]
  }
  deriving (Eq, Show)

data SourceStatement_ pat = SourceStatement pat pat
  deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Source Query Types

type Name = Text

type FieldName = Text

-- | A 'SrcSpan' delimits a portion of a text file.
-- The end position is the column /after/ the end of the span.
-- That is, a span of (1,1)-(1,2) is one character long, and a span of
-- (1,1)-(1,1) is zero characters long.
data SrcSpan = SrcSpan
  { spanStart :: {-# UNPACK #-} !SrcLoc
  , spanEnd   :: {-# UNPACK #-} !SrcLoc
  }
  deriving (Eq, Show)

-- | A point in a source text file.
data SrcLoc = SrcLoc
  { locLine :: {-# UNPACK #-} !Int
  , locCol  :: {-# UNPACK #-} !Int
  }
  deriving (Show, Eq)

data SourcePat_ s v t
  = Nat s Word64
  | String s Text
  | StringPrefix s Text
  | ByteArray s ByteString
    -- ^ There's no concrete syntax for this (yet), but it can be used
    -- via the DSL.
  | Array s [SourcePat_ s v t]
  | Tuple s [SourcePat_ s v t]
  | Struct s [Field s v t]
  | App s (SourcePat_ s v t) [SourcePat_ s v t]
  | KeyValue s (SourcePat_ s v t) (SourcePat_ s v t)
  | Wildcard s
  | Variable s v
  | ElementsOfArray s (SourcePat_ s v t)
  | OrPattern s (SourcePat_ s v t) (SourcePat_ s v t)
  | NestedQuery s
      (SourceQuery_ (SourcePat_ s v t) (SourceStatement_ (SourcePat_ s v t)))
  | Negation s (SourcePat_ s v t)
  | FactId s (Maybe Text) Word64
  | TypeSignature s (SourcePat_ s v t) t
  | Never s
 deriving (Eq, Show)

data Field s v t = Field FieldName (SourcePat_ s v t)
  deriving (Eq, Show)

sourcePatSpan :: SourcePat_ s v t -> s
sourcePatSpan = \case
  Nat s _ -> s
  String s _ -> s
  StringPrefix s _ -> s
  ByteArray s _ -> s
  Array s _ -> s
  Tuple s _ -> s
  Struct s _ -> s
  App s _ _ -> s
  KeyValue s _ _ -> s
  Wildcard s -> s
  Variable s _ -> s
  ElementsOfArray s _ -> s
  OrPattern s _ _ -> s
  NestedQuery s _ -> s
  Negation s _ -> s
  FactId s _ _ -> s
  TypeSignature s _ _ -> s
  Never s -> s

-- ---------------------------------------------------------------------------
-- Pretty printing

class IsWild pat where
  isWild :: pat -> Bool

instance IsWild (SourcePat_ s v t) where
  isWild Wildcard{} = True
  isWild _ = False

-- | Types that represent the source location of a term in the AST
class (Pretty a, Pretty (Loc a)) => IsSrcSpan a where
  type Loc a :: *
  startLoc   :: a -> Loc a
  endLoc     :: a -> Loc a
  mkSpan     :: Loc a -> Loc a -> a

-- space encompassing from start of first span to end of second.
spanBetween :: IsSrcSpan a => a -> a-> a
spanBetween x y = mkSpan (startLoc x) (endLoc y)

instance IsSrcSpan SrcSpan where
  type Loc SrcSpan = SrcLoc
  startLoc = spanStart
  endLoc = spanEnd
  mkSpan = SrcSpan

instance Pretty SrcSpan where
  pretty s =
    pretty (spanStart s)
    <> pretty (" - " :: String)
    <> pretty (spanEnd s)

instance Pretty SrcLoc where
  pretty (SrcLoc line col) =
    "line " <> pretty  line <> ", column " <> pretty col

instance (Pretty v, Pretty t) => Pretty (SourcePat_ s v t) where
  pretty (Nat _ w) = pretty w
  pretty (String _ str) =
    pretty (Text.decodeUtf8 (BL.toStrict (Aeson.encode (Aeson.String str))))
  pretty (StringPrefix s str) =
    pretty (String s str :: SourcePat_ s v t) <> ".."
  pretty (ByteArray _ b) = pretty (show b)
  pretty (Array _ pats) = brackets $ hsep (punctuate "," (map pretty pats))
  pretty (Tuple _ pats) = braces $ hsep (punctuate "," (map pretty pats))
  pretty (Struct _ fs) = cat [ nest 2 $ cat [ "{", fields fs], "}"]
    where
    fields = sep . punctuate "," . map field
    field (Field name pat) = pretty name <+> "=" <+> pretty pat
  pretty (App _ l pats) = pretty l <+> hsep (punctuate " " (map prettyArg pats))
  pretty (KeyValue _ k v) = prettyArg k <+> "->" <+> prettyArg v
  pretty (Wildcard _) = "_"
  pretty (Variable _ name) = pretty name
  pretty (ElementsOfArray _ pat) = pretty pat <> "[..]"
  pretty (OrPattern _ lhs rhs) = sep [prettyArg lhs <+> "|", prettyArg rhs]
  pretty (NestedQuery _ q) = parens $ pretty q
  pretty (Negation _ q) = "!" <> parens (pretty q)
  pretty (FactId _ Nothing n) = "$" <> pretty n
  pretty (FactId _ (Just p) n) = "$" <> pretty p <+> pretty n
  pretty (TypeSignature _ p t) = prettyArg p <+> ":" <+> pretty t
  pretty (Never _) = "never"

instance (Pretty pat, Pretty stmt) => Pretty (SourceQuery_ pat stmt) where
  pretty (SourceQuery maybeHead stmts) = case stmts of
    [] -> pretty maybeHead
    _ -> case maybeHead of
      Just head -> hang 2 (sep (pretty head <+> "where" : pstmts))
      Nothing -> sep pstmts
    where
    pstmts = punctuate ";" (map pretty stmts)

instance Pretty pat => Pretty (SourceStatement_ pat) where
  pretty (SourceStatement lhs rhs) =
    hang 2 $ sep [pretty lhs <+> "=", pretty rhs]

prettyArg :: (Pretty v, Pretty t) => SourcePat_ s v t -> Doc ann
prettyArg pat = case pat of
  App{} -> parens $ pretty pat
  KeyValue{} -> parens $ pretty pat
  OrPattern{} -> parens $ pretty pat
  TypeSignature{} -> parens $ pretty pat
  Nat{} -> pretty pat
  String{} -> pretty pat
  StringPrefix{} -> pretty pat
  ByteArray{} -> pretty pat
  Array{} -> pretty pat
  Tuple{} -> pretty pat
  Struct{} -> pretty pat
  ElementsOfArray{} -> pretty pat
  Wildcard{} -> pretty pat
  Variable{} -> pretty pat
  NestedQuery{} -> pretty pat
  Negation{} -> pretty pat
  FactId{} -> pretty pat
  Never{} -> pretty pat
