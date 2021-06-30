-- Copyright (c) Facebook, Inc. and its affiliates.

{-# LANGUAGE DeriveFunctor, DeriveTraversable #-}
module Glean.Query.Parser.Types
  ( P
  , ParsedQuery
  , ParsedStatement
  , ParsedGenerator
  , Name
  , FieldName
  , Predicate
  , ParsedPat_(..)
  , ParsedPat
  , Field(..)
  , parseError
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Text.Prettyprint.Doc
import Data.Word

import Glean.Angle.Lexer
import Glean.Query.Codegen (Query_(..), Statement(..), Generator(..))

type P a = Alex a

type ParsedQuery = Query_ Predicate ParsedPat ParsedPat
type ParsedStatement = Statement Predicate ParsedPat ParsedPat
type ParsedGenerator = Generator Predicate ParsedPat ParsedPat

type Name = Text

type FieldName = Text

-- | Predicate with an optional version. We'll parse it later when we
-- know what version to use by default.
type Predicate = Text

data ParsedPat_ v
  = Nat Word64
  | String Text
  | StringPrefix Text
  | StringPrefixBind v Text
  | Array [ParsedPat_ v]
  | Tuple [ParsedPat_ v]
  | Struct [Field v]
  | App Name (ParsedPat_ v)
  | Wildcard
  | Variable v
  deriving (Show, Functor, Foldable, Traversable)

type ParsedPat = ParsedPat_ Name

data Field v = Field FieldName (ParsedPat_ v)
  deriving (Show, Functor, Foldable, Traversable)

parseError :: Token -> P a
parseError (Token b t) =
  alexError $ "parse error in query at: " <> case t of
    T_EOF -> "end of string"
    _ -> BL.unpack b


instance Pretty ParsedPat where
  pretty (Nat w) = pretty w
  pretty (String str) =
    pretty (Text.decodeUtf8 (BL.toStrict (Aeson.encode (Aeson.String str))))
  pretty (StringPrefix str) = pretty (String str :: ParsedPat) <> ".."
  pretty (StringPrefixBind name str) =
    pretty (StringPrefix str :: ParsedPat) <> pretty name
  pretty (Array pats) =
    surround (hsep (punctuate "," (map pretty pats))) "[" "]"
  pretty (Tuple pats) =
    surround (hsep (punctuate "," (map pretty pats))) "(" ")"
  pretty (Struct fs) = cat [ nest 2 $ cat [ "{", fields fs], "}"]
    where
    fields = sep . punctuate "," . map field
    field (Field name pat) = pretty name <+> "=" <+> pretty pat
  pretty (App name pat) = pretty name <+> prettyArg pat
  pretty Wildcard = "_"
  pretty (Variable name) = pretty name

prettyArg :: ParsedPat -> Doc ann
prettyArg pat@App{} = "(" <> pretty pat <> ")"
prettyArg pat = pretty pat
