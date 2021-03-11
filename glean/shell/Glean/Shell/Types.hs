{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Glean.Shell.Types (
  Parse(..), Statement(..), JSONQuery(..), AngleQuery(..)
) where

import Glean.RTS.Types

import qualified Text.Parsec as P
import qualified Text.Parsec.Language as P
import qualified Text.Parsec.Token as P

data Statement pat
  = Command String String
  | Pattern pat
  | FactRef Bool Fid

type Parser = P.Parsec String ()

class Parse a where
  parse :: Parser a

instance Parse Fid where
  parse = (Fid . fromInteger) <$> P.braces lexer (P.natural lexer)

instance Parse pat => Parse (Statement pat) where
  parse = P.choice [command, P.try factref, ptrn]
    where
      command = split <$> (P.char ':' *> P.getInput <* P.setInput "")
      ptrn = Pattern <$> parse
      factref = FactRef
        <$> P.option False (P.char '!' *> pure True)
        <*> parse
      split s
        | (cmd,' ':arg) <- break (==' ') s = Command cmd arg
        | otherwise = Command s ""

data AngleQuery = AngleQuery
  { angleQueryRec :: Bool
  , angleQueryStored :: Bool
  , angleQuery :: String
  }

instance Parse AngleQuery where
  parse = AngleQuery
    <$> P.option False (P.char '!' *> pure True)
    <*> P.option False (P.char '*' *> pure True)
    <*> P.many P.anyChar

data JSONQuery = JSONQuery
  { jsonQueryPred :: String
  , jsonQueryRec :: Bool
  , jsonQueryStored :: Bool
  , jsonQuery :: String
  }

instance Parse JSONQuery where
  parse = JSONQuery
    <$> P.identifier lexer
    <*> P.option False (P.char '!' *> pure True)
    <*> P.option False (P.char '*' *> pure True)
    <*> P.many P.anyChar

lexer :: P.TokenParser st
lexer = P.makeTokenParser P.emptyDef
  { P.identLetter = P.alphaNum P.<|> P.oneOf "_." }
