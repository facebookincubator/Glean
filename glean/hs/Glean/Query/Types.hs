module Glean.Query.Types
  ( SourceQuery_(..)
  , SourceStatement_(..)
  , Name
  , FieldName
  , SourcePat_(..)
  , Field(..)
  , IsWild(..)
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

data SourcePat_ v t
  = Nat Word64
  | String Text
  | StringPrefix Text
  | ByteArray ByteString
    -- ^ There's no concrete syntax for this (yet), but it can be used
    -- via the DSL.
  | Array [SourcePat_ v t]
  | Tuple [SourcePat_ v t]
  | Struct [Field v t]
  | App (SourcePat_ v t) [SourcePat_ v t]
  | KeyValue (SourcePat_ v t) (SourcePat_ v t)
  | Wildcard
  | Variable v
  | ElementsOfArray (SourcePat_ v t)
  | OrPattern (SourcePat_ v t) (SourcePat_ v t)
  | NestedQuery
      (SourceQuery_ (SourcePat_ v t) (SourceStatement_ (SourcePat_ v t)))
  | FactId (Maybe Text) Word64
  | TypeSignature (SourcePat_ v t) t
 deriving (Eq, Show)

data Field v t = Field FieldName (SourcePat_ v t)
  deriving (Eq, Show)


-- ---------------------------------------------------------------------------
-- Pretty printing

class IsWild pat where
  isWild :: pat -> Bool

instance IsWild (SourcePat_ v t) where
  isWild Wildcard = True
  isWild _ = False

instance (Pretty v, Pretty t) => Pretty (SourcePat_ v t) where
  pretty (Nat w) = pretty w
  pretty (String str) =
    pretty (Text.decodeUtf8 (BL.toStrict (Aeson.encode (Aeson.String str))))
  pretty (StringPrefix str) = pretty (String str :: SourcePat_ v t) <> ".."
  pretty (ByteArray b) = pretty (show b)
  pretty (Array pats) = brackets $ hsep (punctuate "," (map pretty pats))
  pretty (Tuple pats) = braces $ hsep (punctuate "," (map pretty pats))
  pretty (Struct fs) = cat [ nest 2 $ cat [ "{", fields fs], "}"]
    where
    fields = sep . punctuate "," . map field
    field (Field name pat) = pretty name <+> "=" <+> pretty pat
  pretty (App l pats) = pretty l <+> hsep (punctuate " " (map prettyArg pats))
  pretty (KeyValue k v) = prettyArg k <+> "->" <+> prettyArg v
  pretty Wildcard = "_"
  pretty (Variable name) = pretty name
  pretty (ElementsOfArray pat) = pretty pat <> "[..]"
  pretty (OrPattern lhs rhs) = sep [prettyArg lhs <+> "++", prettyArg rhs]
    -- Temporary: we're stil pretty-printing OrPattern as ++, so that
    -- when we pretty-print the schema into a DB, older versions of
    -- the server and tools can parse it.
  pretty (NestedQuery q) = parens $ pretty q
  pretty (FactId Nothing n) = "$" <> pretty n
  pretty (FactId (Just p) n) = "$" <> pretty p <+> pretty n
  pretty (TypeSignature p t) = prettyArg p <+> ":" <+> pretty t

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

prettyArg :: (Pretty v, Pretty t) => SourcePat_ v t -> Doc ann
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
  FactId{} -> pretty pat
