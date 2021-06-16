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
import Data.Text.Prettyprint.Doc (pretty)

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

  IDENT_        { L _ (Token _ (T_Ident _)) }
  STRING_       { L _ (Token _ (T_StringLit _)) }
  NAT_          { L _ (Token _ (T_NatLit _)) }


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
  | pattern              { SourceStatement (Wildcard $ sspan $1) $1 }

pattern :: { SourcePat }
pattern
  : gen '++' pattern  { OrPattern (s $1 $3) $1 $3 } -- deprecated syntax
  | gen '|' pattern  { OrPattern (s $1 $3) $1 $3 }
  | gen  { $1 }

gen :: { SourcePat }
gen
  : plus  { $1 }
  | plus '!==' plus  { App (s $1 $3) (Variable (sspan $2) "prim.neNat") [$1, $3] }
  | plus '>' plus    { App (s $1 $3) (Variable (sspan $2) "prim.gtNat") [$1, $3] }
  | plus '>=' plus   { App (s $1 $3) (Variable (sspan $2) "prim.geNat") [$1, $3] }
  | plus '<' plus    { App (s $1 $3) (Variable (sspan $2) "prim.ltNat") [$1, $3] }
  | plus '<=' plus   { App (s $1 $3) (Variable (sspan $2) "prim.leNat") [$1, $3] }
  | kv '[' '..' ']'  { ElementsOfArray (s $1 $4) $1 }
    -- NB. kv to resolve shift-reduce conflict
  | plus ':' type  { TypeSignature (s $1 $3) $1 (lval $3) }

plus :: { SourcePat }
plus
  : plus '+' app  { App (s $1 $3) (Variable (sspan $2) "prim.addNat") [$1, $3] }
  | app  { $1 }

app :: { SourcePat }
app
  : list1(kv)  { case $1 of [f] -> f; f:args -> App (s f $ last args) f args }

-- K -> V binds tighter than application, so that e.g.
--   p K -> V
-- can be used to match facts of the functional predicate p.
kv :: { SourcePat }
kv
  : apat '->' apat  { KeyValue (s $1 $3) $1 $3 }
  | apat  { $1 }

apat :: { SourcePat }
apat
  : NAT                             { Nat (sspan $1) (lval $1) }
  | STRING                          { String (sspan $1) (lval $1) }
  | STRING '..'                     { StringPrefix (s $1 $2) (lval $1) }
  | '$' NAT                         { FactId (s $1 $2) Nothing (lval $2)  }
  | '$' var NAT                     { FactId (s $1 $3) (Just $ lval $2) (lval $3) }
  | '[' seplist0(pattern,',') ']'   { Array (s $1 $3) $2 }
  | '{' seplist2(pattern,',') '}'   { Tuple (s $1 $3) $2 }
  | '{' seplist0_(field,',') '}'    { Struct (s $1 $3) $2 }
  | '_'                             { Wildcard (sspan $1) }
  | var                             { Variable (sspan $1) (lval $1) }
  | '(' query ')'
    { case $2 of
        SourceQuery Nothing [SourceStatement (Wildcard _) pat] -> pat
        _other -> NestedQuery (s $1 $3) $2 }
  -- OLD version 1 constructs:
  | '(' ')'                         {% ifVersionOrOlder 1 $2 (Tuple (s $1 $2) []) }
  | '(' seplist2(pattern,',') ')'   {% ifVersionOrOlder 1 $3 (Tuple (s $1 $3) $2) }

field :: { Field SrcSpan Name SourceType }
field
  : fieldname '=' pattern  { Field $1 $3 }

var :: { Located Text }
var : IDENT  { fmap Text.decodeUtf8 $1 }

IDENT :: { Located ByteString }
IDENT : IDENT_ { let L span (Token _ (T_Ident val)) = $1 in L span val }

STRING :: { Located Text }
STRING : STRING_ { let L span (Token _ (T_StringLit val)) = $1 in L span val }

NAT :: { Located Word64 }
NAT : NAT_ { let L span (Token _ (T_NatLit val)) = $1 in L span val }

-- -----------------------------------------------------------------------------
-- Schema

schemas :: { [Schema.SourceSchema] }
schemas : list(schemadef)  { $1 }

schemadef :: { Schema.SourceSchema }
schemadef
  : 'schema' name inherit '{' list(schemadecl) '}'
    { Schema.SourceSchema (lval $2) $3 (concat $5) }

inherit :: { [Name] }
inherit
  : ':' seplist_(name, ',')  { map lval $2 }
  | {- empty -}  { [] }

schemadecl :: { [Schema.SourceDecl] }
schemadecl
  : 'import' name  { [Schema.SourceImport (lval $2)] }
  | typedef  { [$1] }
  | predicate  { $1 }
  | derivedecl  { [$1] }

predicate :: { [Schema.SourceDecl] }
predicate
  : 'predicate' fieldname ':' type optval maybe(deriving)
    { let ref = parseRef $2 in
      Schema.SourcePredicate Schema.PredicateDef
        { predicateDefRef = ref
        , predicateDefKeyType = lval $4
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
  | '->' type { lval $2 }

type :: { Located Schema.SourceType }
type
  : 'byte'                                  { L (sspan $1)   Schema.Byte }
  | 'nat'                                   { L (sspan $1)   Schema.Nat }
  | 'string'                                { L (sspan $1)   Schema.String }
  | '[' type ']'                            { L (s $1 $3)  $ Schema.Array (lval $2) }
  | '{' seplist0_(fielddef, ',') '}'        { L (s $1 $3)  $ Schema.Record $2 }
  | '{' fielddef '|' '}'                    { L (s $1 $4)  $ Schema.Sum [$2] }
  | '{' seplist2_(fielddef, '|')  '}'       { L (s $1 $3)  $ Schema.Sum $2 }
  | 'enum' '{' seplist_(fieldname, '|') '}' { L (s $1 $4)  $ Schema.Enumerated $3 }
  | name                                    { L (sspan $1) $ Schema.Predicate (parseRef $ lval $1) }
     -- resolved to typedef/predicate later
  | 'maybe' type                            { L (s $1 $2)  $ Schema.Maybe (lval $2) }
  | 'bool'                                  { L (sspan $1) $ Schema.Boolean }
  | '(' type ')'                            { L (s $1 $3)  $ lval $2 }

fielddef :: { Schema.SourceFieldDef }
fielddef
  : fieldname ':' type { Schema.FieldDef $1 (lval $3) }
  | fieldname  { Schema.FieldDef $1 unit }

-- Allow keywords to be used as fieldnames
fieldname :: { Name }
fieldname
  : name  { lval $1 }
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
        { typeDefRef = parseRef (lval $2)
        , typeDefType = lval $4 }
    }

name :: { Located Schema.Name }
name : IDENT { fmap Text.decodeUtf8 $1 }

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
parseType bs = runAlex (LB.fromStrict bs) $ fmap lval type_

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

class HasSpan a where
  sspan :: a -> SrcSpan
instance HasSpan (Located a) where
  sspan (L span _) = span
instance HasSpan (SourcePat_ SrcSpan a b) where
  sspan = sourcePatSpan

s :: (HasSpan a, HasSpan b) => a -> b -> SrcSpan
s from to = spanBetween (sspan from) (sspan to)

-- | Located between two items
lbetween :: Located a -> Located b -> c -> Located c
lbetween (L from _) (L to _) = L (spanBetween from to)

invalid :: SourcePat
invalid = Nat invalidSrcSpan (fromIntegral iNVALID_ID)
  where
    invalidSrcSpan = SrcSpan invalidLoc invalidLoc
    invalidLoc = SrcLoc (-1) (-1)

parseError :: Located Token -> P a
parseError (L (SrcSpan loc _) (Token b t)) = do
  filename <- getFile
  alexError $
    show (pretty loc) <> "\n" <>
    (if null filename then "" else filename <> ": ")
    <> "parse error at: "
    <> case t of
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
