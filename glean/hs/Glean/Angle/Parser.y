{
module Glean.Angle.Parser
  ( parseQuery
  , parseQueryWithVersion
  , parseSchema
  , parseType
  ) where

import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Data.Word

import Glean.Angle.Lexer
import Glean.Schema.Util
import Glean.Query.Types
import Glean.Types hiding (Query, Nat)
import qualified Glean.Angle.Types as Schema
import Glean.Angle.Types (AngleVersion, SourcePat, SourceStatement, SourceQuery, SourceType)
}

%tokentype { Located Token }
%token
  'bool'        { L _ (Token _ T_Bool) }
  'byte'        { L _ (Token _ T_Byte) }
  'derive'      { L _ (Token _ T_Derive) }
  'default'     { L _ (Token _ T_Default) }
  'enum'        { L _ (Token _ T_Enum) }
  'import'      { L _ (Token _ T_Import) }
  'maybe'       { L _ (Token _ T_Maybe) }
  'nat'         { L _ (Token _ T_Nat) }
  'predicate'   { L _ (Token _ T_Predicate) }
  'schema'      { L _ (Token _ T_Schema) }
  'string'      { L _ (Token _ T_String) }
  'stored'      { L _ (Token _ T_Stored) }
  'type'        { L _ (Token _ T_Type) }
  'where'       { L _ (Token _ T_QueryDef) }

  '++'          { L _ (Token _ T_Append) }
  '..'          { L _ (Token _ T_DotDot) }
  '->'          { L _ (Token _ T_RightArrow) }
  ','           { L _ (Token _ T_Comma) }
  '|'           { L _ (Token _ T_Bar) }
  ':'           { L _ (Token _ T_Colon) }
  ';'           { L _ (Token _ T_Semi) }
  '('           { L _ (Token _ T_LeftParen) }
  ')'           { L _ (Token _ T_RightParen) }
  '['           { L _ (Token _ T_LeftSquare) }
  ']'           { L _ (Token _ T_RightSquare) }
  '{'           { L _ (Token _ T_LeftCurly) }
  '}'           { L _ (Token _ T_RightCurly) }
  '='           { L _ (Token _ T_Equals) }
  '!=='         { L _ (Token _ T_NotEquals) }
  '>'           { L _ (Token _ T_GreaterThan) }
  '>='          { L _ (Token _ T_GreaterThanOrEquals) }
  '<'           { L _ (Token _ T_LessThan) }
  '<='          { L _ (Token _ T_LessThanOrEquals) }
  '+'           { L _ (Token _ T_Plus) }
  '_'           { L _ (Token _ T_Underscore) }
  '$'           { L _ (Token _ T_Dollar) }

  IDENT         { L _ (Token _ (T_Ident $$)) }
  STRING        { L _ (Token _ (T_StringLit $$)) }
  NAT           { L _ (Token _ (T_NatLit $$)) }


%name query query
%name schema schemas
%name type_ type
%monad { P }
%lexer { lexer } { L _ (Token _ T_EOF)}
%error { parseError }
%expect 0

%%

query :: { SourceQuery }
query
  : pattern 'where' seplist_(statement,';') { SourceQuery (Just $1) $3 }
  | seplist_(statement,';')  { SourceQuery Nothing $1 }

statement :: { SourceStatement }
statement
  : pattern '=' pattern  { SourceStatement $1 $3 }
  | pattern    { SourceStatement Wildcard $1 }

pattern :: { SourcePat }
pattern
  : gen '++' pattern  { OrPattern $1 $3 } -- deprecated syntax
  | gen '|' pattern  { OrPattern $1 $3 }
  | gen  { $1 }

gen :: { SourcePat }
gen
  : plus  { $1 }
  | plus '!==' plus  { App (Variable "prim.neNat") [$1, $3] }
  | plus '>' plus  { App (Variable "prim.gtNat") [$1, $3] }
  | plus '>=' plus  { App (Variable "prim.geNat") [$1, $3] }
  | plus '<' plus  { App (Variable "prim.ltNat") [$1, $3] }
  | plus '<=' plus  { App (Variable "prim.leNat") [$1, $3] }
  | kv '[' '..' ']'  { ElementsOfArray $1 }
    -- NB. kv to resolve shift-reduce conflict
  | plus ':' type  { TypeSignature $1 $3 }

plus :: { SourcePat }
plus
  : plus '+' app  { App (Variable "prim.addNat") [$1, $3] }
  | app  { $1 }

app :: { SourcePat }
app
  : list1(kv)  { case $1 of [f] -> f; f:args -> App f args }

-- K -> V binds tighter than application, so that e.g.
--   p K -> V
-- can be used to match facts of the functional predicate p.
kv :: { SourcePat }
kv
  : apat '->' apat  { KeyValue $1 $3 }
  | apat  { $1 }

apat :: { SourcePat }
apat
  : NAT  { Nat $1 }
  | STRING  { String  $1 }
  | STRING '..' { StringPrefix $1 }
  | '$' NAT   { FactId Nothing $2 }
  | '$' var NAT   { FactId (Just $2) $3 }
  | '[' seplist0(pattern,',') ']'  { Array $2 }
  | '{' seplist2(pattern,',') '}'  { Tuple $2 }
  | '{' seplist0_(field,',') '}'  { Struct $2 }
  | '_'  { Wildcard }
  | var  { Variable $1 }
  | '(' query ')'
    { case $2 of
        SourceQuery Nothing [SourceStatement Wildcard pat] -> pat
        _other -> NestedQuery $2 }
  -- OLD version 1 constructs:
  | '(' ')'  {% ifVersionOrOlder 1 $2 (Tuple []) }
  | '(' seplist2(pattern,',') ')'  {% ifVersionOrOlder 1 $3 (Tuple $2) }

field :: { Field Name SourceType }
field
  : fieldname '=' pattern  { Field $1 $3 }

var :: { Text }
var : IDENT  { Text.decodeUtf8 $1 }


-- -----------------------------------------------------------------------------
-- Schema

schemas :: { [Schema.SourceSchema] }
schemas : list(schemadef)  { $1 }

schemadef :: { Schema.SourceSchema }
schemadef
  : 'schema' name inherit '{' list(schemadecl) '}'
    { Schema.SourceSchema $2 $3 (concat $5) }

inherit :: { [Name] }
inherit
  : ':' seplist_(name, ',')  { $2 }
  | {- empty -}  { [] }

schemadecl :: { [Schema.SourceDecl] }
schemadecl
  : 'import' name  { [Schema.SourceImport $2] }
  | typedef  { [$1] }
  | predicate  { $1 }
  | derivedecl  { [$1] }

predicate :: { [Schema.SourceDecl] }
predicate
  : 'predicate' fieldname ':' type optval maybe(deriving)
    { let ref = parseRef $2 in
      Schema.SourcePredicate Schema.PredicateDef
        { predicateDefRef = ref
        , predicateDefKeyType = $4
        , predicateDefValueType = $5
        , predicateDefDeriving = Schema.NoDeriving }
        : map (Schema.SourceDeriving ref) (maybeToList $6)
    }

deriving :: { Schema.SourceDerivingInfo }
deriving
  : derivewhen query { Schema.Derive $1 $2 }

derivewhen :: { Schema.DeriveWhen }
derivewhen
  : {- empty -}  { Schema.DeriveOnDemand }
  | 'stored'  { Schema.DerivedAndStored }
  | 'default'  { Schema.DeriveIfEmpty }

derivedecl :: { Schema.SourceDecl }
derivedecl
  : 'derive' fieldname deriving  { Schema.SourceDeriving (parseRef $2) $3 }

optval :: { Schema.SourceType }
optval
  : {- empty -} { unit }
  | '->' type { $2 }

type :: { Schema.SourceType }
type
  : 'byte'  { Schema.Byte }
  | 'nat'  { Schema.Nat }
  | 'string'  { Schema.String }
  | '[' type ']'  { Schema.Array $2 }
  | '{' seplist0_(fielddef, ',') '}'  { Schema.Record $2 }
  | '{' fielddef '|' '}' { Schema.Sum [$2] }
  | '{' seplist2_(fielddef, '|')  '}' { Schema.Sum $2 }
  | 'enum' '{' seplist_(fieldname, '|') '}'  { Schema.Enumerated $3 }
  | name  { Schema.Predicate (parseRef $1) }
     -- resolved to typedef/predicate later
  | 'maybe' type  { Schema.Maybe $2 }
  | 'bool'  { Schema.Boolean }
  | '(' type ')' { $2 }

fielddef :: { Schema.SourceFieldDef }
fielddef
  : fieldname ':' type { Schema.FieldDef $1 $3 }
  | fieldname  { Schema.FieldDef $1 unit }

-- Allow keywords to be used as fieldnames
fieldname :: { Name }
fieldname
  : name  { $1 }
  | 'bool'  { "bool" }
  | 'byte'  { "byte" }
  | 'enum'  { "enum" }
  | 'import'  { "import" }
  | 'maybe'  { "maybe" }
  | 'nat'  { "nat" }
  | 'predicate'  { "predicate" }
  | 'schema'  { "schema" }
  | 'string'  { "string" }
  | 'type'  { "type" }
  | 'where'  { "where" }

typedef :: { Schema.SourceDecl }
typedef
  : 'type' name '=' type
    { Schema.SourceType Schema.TypeDef
        { typeDefRef = parseRef $2
        , typeDefType = $4 }
    }

name :: { Schema.Name }
name : IDENT { Text.decodeUtf8 $1 }

-- -----------------------------------------------------------------------------
-- Utils

-- Optional item
maybe(p)
  : {- empty -}  { Nothing }
  | p  { Just $1 }

-- List with no separator
list(p)
  : {- empty -} { [] }
  | p list(p) { $1 : $2 }

-- List with no separator, >=1 elements
list1(p)
  : p  { [$1] }
  | p list1(p) { $1 : $2 }

-- List with a separator, >=1 elements
seplist(p,sep)
  : p sep seplist(p,sep) { $1 : $3 }
  | p { [$1] }

-- List with a separator, >=0 elements
seplist0(p,sep)
  : {- empty -} { [] }
  | seplist(p,sep) { $1 }

-- List with a separator, >=2 elements
seplist2(p,sep)
  : p sep seplist(p,sep) { $1 : $3 }

-- List with a separator, >=1 elements, optional final separator
seplist_(p,sep)
  : p sep seplist_(p,sep) { $1 : $3 }
  | p maybe(sep) { [$1] }

-- List with a separator, >=0 elements, optional final separator
seplist0_(p,sep)
  : {- empty -} { [] }
  | seplist_(p,sep) { $1 }

-- List with a separator, >=2 elements, optional final separator
seplist2_(p,sep)
  : p sep seplist_(p,sep) { $1 : $3 }


{
parseQuery :: ByteString -> Either String SourceQuery
parseQuery bs = runAlex (LB.fromStrict bs) $ query

parseType :: ByteString -> Either String Schema.SourceType
parseType bs = runAlex (LB.fromStrict bs) $ type_

parseQueryWithVersion :: AngleVersion -> ByteString -> Either String SourceQuery
parseQueryWithVersion ver bs =
  runAlex (LB.fromStrict bs) (setVersion ver >> query)

parseSchema :: ByteString -> Either String Schema.SourceSchemas
parseSchema bs
  | Just bs1 <- B.stripPrefix "version: " bs
  , Just (ver, bs2) <- B.readInt bs1 = parseWith bs2 ver
  | otherwise = parseWith bs 1
  -- The "version" header was added at version 1, so if the header is
  -- omitted, assume we are using syntax version 1 for backwards compatibility.
  where
  parseWith bs ver =
    runAlex (LB.fromStrict bs) $ do
      setVersion ver
      src <- schema
      return Schema.SourceSchemas { srcAngleVersion = ver, srcSchemas = src }

type P a = Alex a

invalid :: SourcePat
invalid = Nat (fromIntegral iNVALID_ID)

parseError :: Located Token -> P a
parseError (L (SrcSpan (SrcLoc line col) _) (Token b t)) = do
  filename <- getFile
  alexError $
    (if null filename then "" else filename <> ": ") <>
    "line " <> show line <> ", column " <> show col <>
    ": parse error at: " <> case t of
      T_EOF -> "end of string"
      _ -> LB.unpack b

-- Accept older constructs for backwards-compability only when we're
-- parsing the appropriate version(s) of the syntax.
ifVersionOrOlder :: AngleVersion -> Located Token -> a -> Alex a
ifVersionOrOlder v tok r = do
  thisVer <- getVersion
  when (thisVer > v) $ parseError tok
  return r
}
