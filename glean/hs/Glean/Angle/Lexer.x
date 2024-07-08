-- Copyright (c) Facebook, Inc. and its affiliates.

{
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveFunctor #-}
module Glean.Angle.Lexer
  ( Token(..)
  , TokenType(..)
  , Located(..)
  , AlexInput
  , alexGetInput
  , AlexPosn(..)
  , runAlex
  , lexer
  , Alex(..)
  , alexError
  , alexMonadScan
  , getFile
  , encodeTextForAngle
  , getVersion
  , setVersion
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Parser
import Data.Attoparsec.ByteString (parseOnly)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as Lazy
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word64)

import Glean.Angle.Types (SrcSpan(..), SrcLoc(..), AngleVersion(..), latestAngleVersion)
}

%wrapper "monadUserState-bytestring"

$all = [.\n]
$digit = [0-9]

-- The # in identifiers is reserved for "special" uses. Currently
-- there are two: <predicate>#new and <predicate>#old for
-- restricting queries to the stacked and base DB respectively, when
-- writing incremental derivers.
@ident = [a-zA-Z_] [a-zA-Z0-9_]*

@string = \" (\\ $all | $all # [\"\\] )* \"

-- A qualified name with an optional version:
--    VALID: "a" "a.b" "a.b.3" "a.b#new" "a.b.3#old"
--    INVALID: "a." "a..b" "a.3.b" "a.3b"
@qident = @ident (\. @ident)* (\. $digit+)? (\# @ident)?

tokens :-
  $white+       ;
  "#FILE " .* \n { setFile }
  "#" .* \n     ;

  $digit+       { tokenContent $ T_NatLit . number  }
  @string       { tokenContentP $ \b -> T_StringLit <$> parseString b }

  "bool"        { basicToken T_Bool }
  "byte"        { basicToken T_Byte }
  "default"     { basicToken T_Default }
  "derive"      { basicToken T_Derive }
  "enum"        { basicToken T_Enum }
  "import"      { basicToken T_Import }
  "maybe"       { basicToken T_Maybe }
  "nat"         { basicToken T_Nat }
  "predicate"   { basicToken T_Predicate }
  "schema"      { basicToken T_Schema }
  "set"         { versionDependentToken (AngleVersion 8) T_Set (T_Ident . ByteString.toStrict) }
  "elements"    { versionDependentToken (AngleVersion 8) T_Elements (T_Ident . ByteString.toStrict) }
  "all"         { versionDependentToken (AngleVersion 8) T_All (T_Ident . ByteString.toStrict) }
  "string"      { basicToken T_String }
  "type"        { basicToken T_Type }
  "stored"      { basicToken T_Stored }
  "where"       { basicToken T_QueryDef }
  "evolves"     { basicToken T_Evolves }
  "never"       { basicToken T_Never }
  "if"          { basicToken T_If }
  "then"        { basicToken T_Then }
  "else"        { basicToken T_Else }
  "++"          { basicToken T_Append }
  ("."){2,}     { basicTokenDotDot }
  "->"          { basicToken T_RightArrow }
  ","           { basicToken T_Comma }
  "|"           { basicToken T_Bar }
  ":"           { basicToken T_Colon }
  "("           { basicToken T_LeftParen }
  ")"           { basicToken T_RightParen }
  "["           { basicToken T_LeftSquare }
  "]"           { basicToken T_RightSquare }
  "{"           { basicToken T_LeftCurly }
  "}"           { basicToken T_RightCurly }
  "="           { basicToken T_Equals }
  "!"           { basicToken T_Negate }
  "!=="         { basicToken T_NotEquals }
  "!="          { basicToken T_NotEqualsSingle }
  ">"           { basicToken T_GreaterThan }
  ">="          { basicToken T_GreaterThanOrEquals }
  "<"           { basicToken T_LessThan }
  "<="          { basicToken T_LessThanOrEquals }
  "+"           { basicToken T_Plus }
  ";"           { basicToken T_Semi }
  "_"           { basicToken T_Underscore }
  "$"           { basicToken T_Dollar }

  @qident        { tokenContent $ T_Ident . ByteString.toStrict }
{
data AlexUserState = AlexUserState
  { angleVersion :: AngleVersion
  , currentFile :: FilePath
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState latestAngleVersion ""

-- | A value with its source location.
data Located a = L
  { lspan :: SrcSpan
  , lval  :: a
  }
  deriving (Eq, Show, Functor)

data Token = Token ByteString TokenType

data TokenType
  = T_Bool
  | T_Byte
  | T_Derive
  | T_Default
  | T_Enum
  | T_Import
  | T_Maybe
  | T_Nat
  | T_Predicate
  | T_Schema
  | T_Set
  | T_Elements
  | T_All
  | T_String
  | T_Type
  | T_Stored
  | T_Where
  | T_Ident Strict.ByteString
  | T_StringLit Text
  | T_NatLit Word64
  | T_QueryDef
  | T_Evolves
  | T_Never
  | T_If
  | T_Then
  | T_Else
  | T_Append
  | T_DotDot
  | T_RightArrow
  | T_Comma
  | T_Bar
  | T_Colon
  | T_LeftParen
  | T_RightParen
  | T_LeftSquare
  | T_RightSquare
  | T_LeftCurly
  | T_RightCurly
  | T_Equals
  | T_Negate
  | T_NotEquals
  | T_NotEqualsSingle
  | T_GreaterThan
  | T_GreaterThanOrEquals
  | T_LessThan
  | T_LessThanOrEquals
  | T_Plus
  | T_Semi
  | T_Underscore
  | T_Dollar
  | T_EOF
  deriving Show

-- setFile :: AlexAction a
setFile inp@(_,_,b,_) len = do
  let filename = ByteString.drop 6 $ ByteString.take (len-1) b
  Alex $ \state -> Right (
    state { alex_ust = (alex_ust state) {
              currentFile = UTF8.toString (ByteString.toStrict filename) }
          , alex_pos = alexStartPos }, ())
  skip inp len

getFile :: Alex FilePath
getFile = Alex $ \state -> Right (state, currentFile (alex_ust state))

getVersion :: Alex AngleVersion
getVersion = Alex $ \state -> Right (state, angleVersion (alex_ust state))

setVersion :: AngleVersion -> Alex ()
setVersion ver = Alex $ \state -> Right
  (state { alex_ust = (alex_ust state) { angleVersion = ver }}, ())

basicToken :: TokenType -> AlexAction (SrcLoc, Token)
basicToken t (AlexPn _ line col,_,b,_) len =
  return $ (SrcLoc line col, Token (ByteString.take len b) t)

versionDependentToken :: AngleVersion -> TokenType -> (ByteString -> TokenType) -> AlexAction (SrcLoc, Token)
versionDependentToken firstSupportedVersion newToken oldToken =
  tokenContentP $ \bs -> do
    currentVersion <- getVersion
    if currentVersion >= firstSupportedVersion
      then return newToken
      else return $ oldToken bs

basicTokenDotDot :: AlexAction (SrcLoc, Token)
basicTokenDotDot i@(AlexPn _ line col,_,_,_) len =
  if len > 2 then
    alexError $ "Too many dots at line " ++ show line ++
      ", column " ++ show col ++
      ". Try using '..' instead of '" ++ (replicate (fromIntegral len) '.') ++ "'"
  else
    basicToken T_DotDot i len

tokenContent :: (ByteString -> TokenType) -> AlexAction (SrcLoc, Token)
tokenContent f = tokenContentP (return . f)

tokenContentP :: (ByteString -> Alex TokenType) -> AlexAction (SrcLoc, Token)
tokenContentP f (AlexPn _ line col,_,b,_) len =
  (SrcLoc line col,) . Token content <$> f content
  where
    content = ByteString.take len b

number :: ByteString -> Word64
number = ByteString.foldl' f 0 where
  f x y = x * 10 + fromIntegral (y - fromIntegral (Data.Char.ord '0'))

alexEOF :: Alex (SrcLoc, Token)
alexEOF = do
  (AlexPn _ line col,_,_,_) <- alexGetInput
  return (SrcLoc line col, Token "" T_EOF)

-- | We'll use JSON syntax for strings, as a reasonably fast way to support
-- some escaping syntax.
parseString :: ByteString -> Alex Text
parseString b =
  case parseOnly jstring (ByteString.toStrict b) of
    Left{} -> alexError "lexical error in string"
    Right a -> return a

-- | encode a Text value into an Angle string, with appropriate escaping and
-- surrounded by double quotes. e.g.
--
-- > ghci> encodeTextForAngle "ab\"\NUL"
-- > "\"ab\\\"\\u0000\""
--
encodeTextForAngle :: Text -> Text
encodeTextForAngle =
  Text.decodeUtf8 . Lazy.toStrict . Aeson.encode . Aeson.String

getToken :: Alex (SrcLoc, Token)
getToken = Alex $ \state ->
  case unAlex alexMonadScan state of
    Left err ->
      let file = currentFile (alex_ust state) in
      if null file
        then Left err
        else Left (file <> ": " <> err)
    Right a -> Right a

lexer :: (Located Token -> Alex a) -> Alex a
lexer f = do
  (start, tok) <- getToken
  (AlexPn _ eline ecol,_,_,_) <- alexGetInput
  let end = SrcLoc eline ecol
  f $ L (SrcSpan start end) tok

}
