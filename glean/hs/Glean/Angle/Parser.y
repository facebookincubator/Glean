-- Copyright (c) Facebook, Inc. and its affiliates.

{
module Glean.Angle.Parser
  ( parseQuery
  , parseQueryWithVersion
  , parseSchema
  , stripAngleVersion
  , parseSchemaWithVersion
  , parseType
  ) where

import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Text (Text)
import Data.Word
import Data.Text.Prettyprint.Doc (pretty)

import Glean.Angle.Lexer
import Glean.Schema.Util
import Glean.Types hiding (Query, Nat)
import Glean.Angle.Types as Schema
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
  'set'         { L _ (Token _ T_Set) }
  'elements'    { L _ (Token _ T_Elements) }
  'all'         { L _ (Token _ T_All) }
  'string'      { L _ (Token _ T_String) }
  'stored'      { L _ (Token _ T_Stored) }
  'type'        { L _ (Token _ T_Type) }
  'where'       { L _ (Token _ T_QueryDef) }
  'evolves'     { L _ (Token _ T_Evolves) }
  'never'       { L _ (Token _ T_Never) }
  'if'          { L _ (Token _ T_If) }
  'then'        { L _ (Token _ T_Then) }
  'else'        { L _ (Token _ T_Else) }
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
  '!'           { L _ (Token _ T_Negate) }
  '!=='         { L _ (Token _ T_NotEquals) }
  '!='          { L _ (Token _ T_NotEqualsSingle) }
  '>'           { L _ (Token _ T_GreaterThan) }
  '>='          { L _ (Token _ T_GreaterThanOrEquals) }
  '<'           { L _ (Token _ T_LessThan) }
  '<='          { L _ (Token _ T_LessThanOrEquals) }
  '+'           { L _ (Token _ T_Plus) }
  '_'           { L _ (Token _ T_Underscore) }
  '$'           { L _ (Token _ T_Dollar) }

  SELECT_       { L _ (Token _ (T_Select _)) }
  SELECTALT_    { L _ (Token _ (T_SelectAlt _)) }
  UIDENT_       { L _ (Token _ (T_UIdent _)) }
  LIDENT_       { L _ (Token _ (T_LIdent _)) }
  QIDENT_       { L _ (Token _ (T_QIdent _)) }
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
  : gen '++' pattern { OrPattern (s $1 $3) $1 $3 } -- deprecated syntax
  | gen '|' pattern  { OrPattern (s $1 $3) $1 $3 }
  | '!' gen          { Negation (s $1 $2) $2 }
  | 'if' pattern
    'then' pattern
    'else' pattern   { IfPattern (s $1 $6) $2 $4 $6 }
  | gen              { $1 }

gen :: { SourcePat }
gen
  : op { $1 }
  | op ':' type    { TypeSignature (s $1 $3) $1 (lval $3) }

op :: { SourcePat }
op
  : plus  { $1 }
  | plus '!=' plus   { App (s $1 $3) (Variable (sspan $2) "prim.neExpr") [$1, $3] }
  | plus '!==' plus  { App (s $1 $3) (Variable (sspan $2) "prim.neNat") [$1, $3] }
  | plus '>' plus    { App (s $1 $3) (Variable (sspan $2) "prim.gtNat") [$1, $3] }
  | plus '>=' plus   { App (s $1 $3) (Variable (sspan $2) "prim.geNat") [$1, $3] }
  | plus '<' plus    { App (s $1 $3) (Variable (sspan $2) "prim.ltNat") [$1, $3] }
  | plus '<=' plus   { App (s $1 $3) (Variable (sspan $2) "prim.leNat") [$1, $3] }
  | kv '[' '..' ']'  { ElementsOfArray (s $1 $4) $1 }
    -- NB. kv to resolve shift-reduce conflict

plus :: { SourcePat }
plus
  : plus '+' app  { App (s $1 $3) (Variable (sspan $2) "prim.addNat") [$1, $3] }
  | app  { $1 }

app :: { SourcePat }
app
  : list1(kv)  { case $1 of [f] -> f; f:args -> App (s f $ last args) f args }
  | 'elements' kv { Elements (s $1 $2) $2 }
  | 'all' kv { All (s $1 $2) $2 }

-- K -> V binds tighter than application, so that e.g.
--   p K -> V
-- can be used to match facts of the functional predicate p.
kv :: { SourcePat }
kv
  : select '->' select  { KeyValue (s $1 $3) $1 $3 }
  | select  { $1 }

select :: { SourcePat }
select
  : select SELECT { FieldSelect (s $1 $2) $1 (lval $2) Record }
  | select SELECTALT { FieldSelect (s $1 $2) $1 (lval $2) Sum }
  | apat  { $1 }

apat :: { SourcePat }
apat
  : NAT                             { Nat (sspan $1) (lval $1) }
  | STRING                          { String (sspan $1) (lval $1) }
  | STRING '..'                     { StringPrefix (s $1 $2) (lval $1) }
  | '$' NAT                         { FactId (s $1 $2) Nothing (lval $2)  }
  | '$' var NAT                     { FactId (s $1 $3) (Just $ lval $2) (lval $3) }
  | '[' seplist0(pattern,',') ']'   { Array (s $1 $3) $2 }
  | '[' seplist__(pattern,',') '..' ']'  { ArrayPrefix (s $1 $4) (let (h:t) = $2 in h:|t) }
  | '{' seplist2(pattern,',') '}'   { Tuple (s $1 $3) $2 }
  | '{' seplist0_(field,',') '}'    { Struct (s $1 $3) $2 }
  | '_'                             { Wildcard (sspan $1) }
  | var                             { Variable (sspan $1) (lval $1) }
  | lident                          { Enum (sspan $1) (lval $1) }
  | 'never'                         { Never (sspan $1) }
  | '(' query ')'                   { nestedQuery (s $1 $3) $2 }
  -- OLD version 1 constructs:
  | '(' ')'                         {% ifVersionOrOlder (AngleVersion 1) $2 (Tuple (s $1 $2) []) }
  | '(' seplist2(pattern,',') ')'   {% ifVersionOrOlder (AngleVersion 1) $3 (Tuple (s $1 $3) $2) }

field :: { Field SrcSpan SourceRef SourceRef }
field
  : fieldname '=' pattern  { Field $1 $3 }

var :: { Located Text }
var
  : qident { $1 }
  | uident { $1 }

lident :: { Located Text }
lident : LIDENT  { fmap Text.decodeUtf8 $1 }

uident :: { Located Text }
uident : UIDENT  { fmap Text.decodeUtf8 $1 }

qident :: { Located Text }
qident : QIDENT  { fmap Text.decodeUtf8 $1 }

UIDENT :: { Located ByteString }
UIDENT : UIDENT_ { let L span (Token _ (T_UIdent val)) = $1 in L span val }

LIDENT :: { Located ByteString }
LIDENT : LIDENT_ { let L span (Token _ (T_LIdent val)) = $1 in L span val }

QIDENT :: { Located ByteString }
QIDENT : QIDENT_ { let L span (Token _ (T_QIdent val)) = $1 in L span val }

STRING :: { Located Text }
STRING : STRING_ { let L span (Token _ (T_StringLit val)) = $1 in L span val }

SELECT :: { Located Text }
SELECT : SELECT_ {
  let L span (Token _ (T_Select val)) = $1 in
  L span (Text.decodeUtf8 val) }

SELECTALT :: { Located Text }
SELECTLAT : SELECTALT_ {
  let L span (Token _ (T_SelectAlt val)) = $1 in
  L span (Text.decodeUtf8 val) }

NAT :: { Located Word64 }
NAT : NAT_ { let L span (Token _ (T_NatLit val)) = $1 in L span val }

-- -----------------------------------------------------------------------------
-- Schema

schemas :: { [Either Schema.SourceEvolves Schema.SourceSchema] }
schemas
  : schemas schemadef { Right $2 : $1 }
  | schemas evolves { Left $2 : $1 }
  | {- empty -}  { [] }

evolves :: { Schema.SourceEvolves }
evolves
  : 'schema' qname 'evolves' qname
    { Schema.SourceEvolves (s $1 $4) (lval $2) (lval $4) }

schemadef :: { Schema.SourceSchema }
schemadef
  : 'schema' qname inherit '{' list(schemadecl) '}'
    { Schema.SourceSchema (lval $2) $3 (concat $5) }

inherit :: { [Name] }
inherit
  : ':' seplist_(qname, ',')  { map lval $2 }
  | {- empty -}  { [] }

schemadecl :: { [Schema.SourceDecl] }
schemadecl
  : 'import' qname  { [Schema.SourceImport (lval $2)] }
  | typedef  { [$1] }
  | predicate  { $1 }
  | derivedecl  { [$1] }

predicate :: { [Schema.SourceDecl] }
predicate
  : 'predicate' predicatename ':' type optval maybe(deriving)
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
  : 'derive' qname deriving  { Schema.SourceDeriving (parseRef (lval $2)) $3 }

optval :: { Schema.SourceType }
optval
  : {- empty -} { unit }
  | '->' type { lval $2 }

type :: { Located Schema.SourceType }
type
  : 'byte'                                  { L (sspan $1)   Schema.ByteTy }
  | 'nat'                                   { L (sspan $1)   Schema.NatTy }
  | 'string'                                { L (sspan $1)   Schema.StringTy }
  | '[' type ']'                            { L (s $1 $3)  $ Schema.ArrayTy (lval $2) }
  | '{' seplist0_(fielddef, ',') '}'        { L (s $1 $3)  $ Schema.RecordTy $2 }
  | '{' fielddef '|' '}'                    { L (s $1 $4)  $ Schema.SumTy [$2] }
  | '{' seplist2_(fielddef, '|')  '}'       { L (s $1 $3)  $ Schema.SumTy $2 }
  | 'set' type                              { L (s $1 $2)  $ Schema.SetTy (lval $2) }
  | 'enum' '{' seplist_(fieldname, '|') '}' { L (s $1 $4)  $ Schema.EnumeratedTy $3 }
  | qname                                    { L (sspan $1) $ Schema.PredicateTy (parseRef $ lval $1) }
     -- resolved to typedef/predicate later
  | 'maybe' type                            { L (s $1 $2)  $ Schema.MaybeTy (lval $2) }
  | 'bool'                                  { L (sspan $1) $ Schema.BooleanTy }
  | '(' type ')'                            { L (s $1 $3)  $ lval $2 }

fielddef :: { Schema.SourceFieldDef }
fielddef
  : fieldname ':' type { Schema.FieldDef $1 (lval $3) }
  | fieldname  { Schema.FieldDef $1 unit }

predicatename :: { Name }
predicatename
  : fieldname { $1 }
  | qident { lval $1 }

-- Allow keywords to be used as fieldnames
fieldname :: { Name }
fieldname
  : lident  { lval $1 }
  | uident  { lval $1 } -- we want to deprecate this
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
  : 'type' predicatename '=' type
    { Schema.SourceType Schema.TypeDef
        { typeDefRef = parseRef $2
        , typeDefType = lval $4 }
    }

qname :: { Located Schema.Name }
qname
  : uident { $1 }
  | qident { $1 }
  | lident { $1 } -- probably shouldn't be allowed

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

-- List with a separator, >=1 elements, mandatory final separator
seplist__(p,sep)
  : p sep seplist__(p,sep) { $1 : $3 }
  | p sep { [$1] }

-- List with a separator, >= 1 elements.
-- The span of the elements is also returned.
seplistSpan(p,sep)
  : p sep seplistSpan(p,sep) { (sspan $1, $1) : $3 }
  | p { [(sspan $1, $1)] }

-- List with a separator, >=0 elements.
-- The span of the elements is also returned.
seplistSpan0(p,sep)
  : {- empty -} { [] }
  | seplistSpan(p,sep) { $1 }

{
parseQuery :: ByteString -> Either String SourceQuery
parseQuery bs = runAlex (LB.fromStrict bs) $ query

parseType :: ByteString -> Either String Schema.SourceType
parseType bs = runAlex (LB.fromStrict bs) $ fmap lval type_

parseQueryWithVersion
  :: AngleVersion
  -> ByteString
  -> Either String SourceQuery
parseQueryWithVersion ver bs =
  runAlex (LB.fromStrict bs) (setVersion ver >> query)

parseSchema :: ByteString -> Either String Schema.SourceSchemas
parseSchema bs = parseSchemaWithVersion ver rest
  where (ver, rest) = stripAngleVersion bs

stripAngleVersion :: ByteString -> (AngleVersion, ByteString)
stripAngleVersion bs
  | Just bs1 <- B.stripPrefix "version: " bs
  , Just (ver, bs2) <- B.readInt bs1 = (Schema.AngleVersion ver, bs2)
  | otherwise = (latestAngleVersion, bs)
  -- if the header is omitted, assume we are using the latest version

parseSchemaWithVersion
  :: AngleVersion
  -> ByteString
  -> Either String Schema.SourceSchemas
parseSchemaWithVersion ver bs =
  runAlex (LB.fromStrict bs) $ do
    setVersion ver
    (srcEvolves, srcSchemas) <- partitionEithers <$> schema
    return Schema.SourceSchemas
      { srcAngleVersion = ver
      , srcSchemas = reverse srcSchemas
      , srcEvolves = reverse srcEvolves
      }

type P a = Alex a

class HasSpan a where
  sspan :: a -> SrcSpan
instance HasSpan (Located a) where
  sspan (L span _) = span
instance HasSpan (SourcePat_ SrcSpan a b) where
  sspan = sourcePatSpan
instance HasSpan (SourceStatement_ SrcSpan p t) where
  sspan (SourceStatement p1 p2) = s p1 p2
instance HasSpan (SourceQuery_ SrcSpan p t) where
  sspan (SourceQuery Nothing stmts) = s (head stmts) (last stmts)
  sspan (SourceQuery (Just pat) stmts) = s pat (last stmts)

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

-- | Smart constructor for NestedQuery
nestedQuery :: SrcSpan -> SourceQuery -> SourcePat
nestedQuery _s (SourceQuery Nothing [SourceStatement (Wildcard _) pat]) = pat
nestedQuery s q = NestedQuery s q

-- Accept older constructs for backwards-compability only when we're
-- parsing the appropriate version(s) of the syntax.
ifVersionOrOlder :: AngleVersion -> Located Token -> a -> Alex a
ifVersionOrOlder v tok r = do
  thisVer <- getVersion
  when (thisVer > v) $ parseError tok
  return r
}
