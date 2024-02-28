{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

-- |
-- Small domain-specific language for building Angle queries
-- programmatically.
--
{-# LANGUAGE TypeApplications, TypeOperators, AllowAmbiguousTypes #-}
module Glean.Query.Angle
  ( Angle
  , AngleStatement
  , var
  , Type
  , vars
  , predicate
  , where_
  , not_
  , (.=)
  , stmt
  , (.|)
  , or_
  , (.->)
  , query
  , nat
  , byte
  , enum
  , string
  , stringPrefix
  , byteArray
  , array
  , tuple
  , wild
  , never
  , field
  , end
  , rec
  , alt
  , asPredicate
  , sig
  , factId
  , factIds
  , factIdsArray
  , elementsOf
  , arrayPrefix
  , unit
  , if_
  , true
  , false
  , bool
  , just
  , nothing
  , display
  , RecordFields
  , SumFields
  , TFields(..)
  , HasFields
  , HasField
  , AngleEnum(..)
  , AngleVars
  , new
  , old
  ) where

import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Proxy
import Data.String
import qualified Data.List.Extra as List (nubOrd)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty (fromList, toList)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc hiding ((<>))
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Word
import GHC.TypeLits hiding (Nat)
import TextShow

import Glean.Angle.Types hiding
  (SourcePat, SourceStatement, SourceQuery, Field, Type)
import qualified Glean.Angle.Types as Angle
import qualified Glean.Display as Display
import Glean.Query.Thrift.Internal as Thrift
import Glean.Typed hiding (end)
import Glean.Types (Nat, Byte)

data SpanAngleDSL = DSL
  deriving (Show, Eq)

instance IsSrcSpan SpanAngleDSL where
  type Loc SpanAngleDSL = SpanAngleDSL
  startLoc _ = DSL
  endLoc _ = DSL
  mkSpan _ _ = DSL

instance Pretty SpanAngleDSL where
  pretty DSL = "angle DSL"

type SourcePat = SourcePat' SpanAngleDSL
type SourceStatement = SourceStatement' SpanAngleDSL
type SourceQuery = SourceQuery' SpanAngleDSL

newtype Angle t = Angle { gen :: State Int SourcePat }

newtype AngleStatement =
  AngleStatement { genStmt :: State Int SourceStatement }

-- | Render to angle text, to aid logging and debugging
display :: Angle t -> Text
display = render . build

render :: SourceQuery -> Text
render q = renderStrict (layoutCompact (Display.displayDefault q))

build :: Angle t -> SourceQuery
build (Angle m) =
  case evalState m 0 of
    NestedQuery _ q -> q
    t -> Angle.SourceQuery (Just t) []

-- | Build a query. It can returns facts of a predicate:
--
-- >    query $ predicate @Pp.Define (field @"macro" "NULL" end)
--
-- Or arbitrary values:
--
-- >    query $ var $ \n ->
-- >      n `where_` [
-- >        stmt $ predicate @Hack.MethodDeclaration $
-- >          rec $
-- >            field @"name" "foo" $
-- >            field @"container"
-- >              (rec $
-- >                 field @"class_"
-- >                    (rec (field @"name" n end))
-- >                 end)
-- >      ]
query :: (Type t) => Angle t -> Query t
query = Thrift.angleData . display

class AngleVars f r where
  -- | Use `vars` to batch up a series of nested `var` calls:
  --
  -- > let foo :: Angle a -> Angle b -> Angle c -> Angle d -> Angle e
  -- > vars foo == var $ \a -> var $ \b -> var $ \c -> var $ \d -> foo a b c d
  vars :: f -> Angle r

instance (a ~ b) => AngleVars (Angle a) b where
  vars = id

instance (Type a, AngleVars s r) => AngleVars (Angle a -> s) r where
  vars f = var $ \ a -> vars (f a)

-- | Introduce an Angle variable
var :: forall a b . Type a => (Angle a -> Angle b) -> Angle b
var f = Angle $ do
  x <- get
  modify (+1)
  let v = Angle (pure (Variable DSL ("X" <> showt x)))
  ty <- gen (sig v)
  r <- gen (f v)
  let stmt = Angle.SourceStatement (Wildcard DSL) ty
  case r of
    Angle.NestedQuery DSL (Angle.SourceQuery q stmts) ->
      return (Angle.NestedQuery DSL (Angle.SourceQuery q (stmt : stmts)))
    _ ->
      return (Angle.NestedQuery DSL (Angle.SourceQuery (Just r) [stmt]))

predicate :: forall p . Predicate p => Angle (KeyType p) -> Angle p
predicate (Angle pat) = Angle $ do
  p <- pat
  return $ App DSL (Variable DSL (predicateRef (Proxy @p))) [p]

-- | Build a query of the form `X where statements`
where_ :: Angle t -> [AngleStatement] -> Angle t
where_ t stmts = Angle $
  nest <$> gen t <*> mapM genStmt stmts
  where
  -- merge adjacent where-clauses
  nest (Angle.NestedQuery DSL (Angle.SourceQuery (Just q) stmts1)) stmts2 =
    Angle.NestedQuery DSL $
      Angle.SourceQuery (Just q) (stmts1 <> stmts2)
  nest q stmts =
    Angle.NestedQuery DSL (Angle.SourceQuery (Just q) stmts)

not_ :: [AngleStatement] -> AngleStatement
not_ stmts = unit' .= Angle (Negation DSL <$> gen t)
  where
    t = unit' `where_` stmts
    unit' = sig unit

-- | Build a statement, `A = B`
(.=) :: Angle t -> Angle t -> AngleStatement
l .= r = AngleStatement $ Angle.SourceStatement <$> gen l <*> gen r

-- | Build a statement from a plain Angle expression, ignoring the result.
-- `stmt A` is equivalent in Angle to the statement `_ = A`. For example
--
-- > var $ \x ->
-- >   x `where_` [
-- >      stmt $ predicate @Src.File x
-- >   ]
--
stmt :: Angle t -> AngleStatement
stmt = (wild .=)

infix 1 .=
infixr 2 `or_`
infixr 3 .|

class ToExpr a where
  type Result a
  toExpr :: a -> Angle (Result a)

instance ToExpr (Angle a) where
  type Result (Angle a) = a
  toExpr = id

instance ToExpr AngleStatement where
  type Result AngleStatement = ()
  toExpr stmt = sig unit `where_` [stmt]

instance ToExpr [AngleStatement] where
  type Result [AngleStatement] = ()
  toExpr stmts = sig unit `where_` stmts

-- | Build an or-pattern, `A | B`
(.|) :: (Result a ~ Result b, ToExpr a, ToExpr b) => a -> b -> Angle (Result a)
a .| b = Angle $ OrPattern DSL <$> gen (toExpr a) <*> gen (toExpr b)

-- | Build an or-pattern between statements (deprecated: just use '.|' instead)
or_ :: [AngleStatement] -> [AngleStatement] -> Angle ()
or_ = (.|)

-- | Build a key-value pattern, `A -> B`
(.->) :: Angle a -> Angle b -> Angle c
a .-> b = Angle $ KeyValue DSL <$> gen a <*> gen b

instance IsString (Angle Text) where
  fromString s = Angle (pure (String DSL (Text.pack s)))

-- | A string expression. Note that literal strings may be used as Angle
-- expressions directly if the @OverloadedStrings@ extension is
-- enabled.
string :: Text -> Angle Text
string t = Angle (pure (String DSL t))

nat :: Word64 -> Angle Nat
nat w = Angle (pure (Nat DSL w))

byte :: Word8 -> Angle Byte
byte w = Angle (pure (Nat DSL (fromIntegral w)))

-- | HACK: A typeclass to represent enums generated from Angle. This is used
-- to splice values of these Angle-generated Thrift enum values back into Angle
-- queries.
--
-- Ideally implementations of these should be codegenerated or we should come
-- up with a better way of embedding enum values in this DSL.
class AngleEnum a where
  type AngleEnumTy a
  enumName :: a -> Text

enum :: AngleEnum a => a -> Angle (AngleEnumTy a)
enum e = Angle (pure (Variable DSL (enumName e)))

stringPrefix :: Text -> Angle Text
stringPrefix t = Angle (pure (StringPrefix DSL t))

-- | A query for a literal value of type [byte]
byteArray :: ByteString -> Angle ByteString
byteArray b = Angle (pure (String DSL (Text.decodeUtf8 b)))

-- | Build an array expression
array :: [Angle t] -> Angle [t]
array xs = Angle $ Array DSL <$> mapM gen xs

-- | Build a tuple
tuple :: AngleTuple a => a -> Angle (AngleTupleTy a)
tuple = fromTuple

type SourceField = Angle.Field SpanAngleDSL SourceRef SourceRef

-- | Match a record. Zero or more of the fields may be matched.
--
-- >   rec $
-- >     field @"abc" x $
-- >     field @"def" y $
-- >   end
--
rec :: HasFields f (RecordFields t) => Fields f -> Angle t
rec fs = Angle $ Struct DSL <$> go fs []
  where
  go :: forall f . Fields f -> [SourceField] -> State Int [SourceField]
  go f acc = case f of
    NoFields -> return acc
    Field l v rest  -> do
      v' <- gen v
      go rest (Angle.Field l v' : acc)

-- ToDo: we will also probably want a way to cast a record to a
-- subtype, and to emit a type signature.

-- | Match an alternative of a sum type
--
-- >   alt @"abc" value .| alt @"def" value
--
alt
  :: forall (l :: Symbol) v r .
     (KnownSymbol l, HasField l v (SumFields r)) =>
     Angle v
  -> Angle r
alt val = Angle $ do
  v <- gen val
  return $ Struct DSL
    [Angle.Field (Text.pack (symbolVal (Proxy @l))) v]

data Fields :: TFields -> * where
  NoFields :: Fields 'TNoFields
  Field :: Text -> Angle v -> Fields fs -> Fields ('TField l v fs)

field
  :: forall (l :: Symbol) v fs . KnownSymbol l =>
     Angle v
  -> Fields fs
  -> Fields ('TField l v fs)
field val rest = Field (Text.pack (symbolVal (Proxy @l))) val rest

end :: Fields 'TNoFields
end = NoFields

wild :: Angle t
wild = Angle $ pure $ Wildcard DSL

never :: Angle t
never = Angle $ pure (Variable DSL "never")

-- | Use this when you want a variable to match a nested predicate
-- rather than its key.
--
-- >  var $ \(n :: Angle Hack.Name) ->
-- >    n `where_` [
-- >      stmt $ predicate @Hack.MethodDeclaration $
-- >        rec $
-- >          field @"name" (asPredicate n) $
-- >          field @"container"
-- >             (rec $
-- >                field @"class_"
-- >                   (rec (field @"name" "MyClass" end))
-- >              end) $
-- >         end
-- >    ]
--
asPredicate :: Angle p -> Angle (KeyType p)
asPredicate (Angle a) = Angle a

-- | Sometimes the Angle typechecker needs a type signature.
-- This adds a type signature for any Angle type.
sig :: forall p. Type p => Angle p -> Angle p
sig a = Angle $ do
  ta <- gen a
  return $ TypeSignature DSL ta $ sourceType (Proxy @p)

class AngleTuple a where
  type AngleTupleTy a
  fromTuple :: a -> Angle (AngleTupleTy a)

instance AngleTuple (Angle a, Angle b) where
  type AngleTupleTy (Angle a, Angle b) = (a,b)
  fromTuple (a, b) = Angle $ do
    ta <- gen a
    tb <- gen b
    return $ Tuple DSL [ta,tb]

instance AngleTuple (Angle a, Angle b, Angle c) where
  type AngleTupleTy (Angle a, Angle b, Angle c) = (a,b,c)
  fromTuple (a,b,c) = Angle $ do
    ta <- gen a
    tb <- gen b
    tc <- gen c
    return $ Tuple DSL [ta,tb,tc]

instance AngleTuple (Angle a, Angle b, Angle c, Angle d) where
  type AngleTupleTy (Angle a, Angle b, Angle c, Angle d) = (a,b,c,d)
  fromTuple (a,b,c,d) = Angle $ do
    ta <- gen a
    tb <- gen b
    tc <- gen c
    td <- gen d
    return $ Tuple DSL [ta,tb,tc,td]

instance AngleTuple (Angle a, Angle b, Angle c, Angle d, Angle e) where
  type AngleTupleTy (Angle a, Angle b, Angle c, Angle d, Angle e) = (a,b,c,d,e)
  fromTuple (a,b,c,d,e) = Angle $ do
    ta <- gen a
    tb <- gen b
    tc <- gen c
    td <- gen d
    te <- gen e
    return $ Tuple DSL [ta,tb,tc,td,te]

-- | Build a fact id with an explicit type @$123 : Type.2@
factId :: forall p. Predicate p => IdOf p -> Angle p
factId = sig . factId_

-- | Build a fact id without an explicit type, @$ 123@
factId_ :: forall p. IdOf p -> Angle p
factId_ = Angle
  . return
  . FactId DSL Nothing
  . fromIntegral
  . fromFid
  . idOf

-- | Convert @factIds (1 :| [2,3]) :: NonEmpty (IdOf p)@ into
-- @($1 : p.v) ++ $2 ++ $3@ for building angle
-- queries, where @v@ is the version of predicate @p@.
-- Uses disjuction, and nubOrds the input list
factIds :: forall p. Predicate p => NonEmpty (IdOf p) -> Angle p
factIds xs = sig $ foldr1 (.|) (fmap factId_ (nubOrd' xs))
  where
    -- Our version of package extra lacks Data.List.NonEmpty.Extra
    nubOrd' :: Ord a => NonEmpty a -> NonEmpty a
    nubOrd' = NonEmpty.fromList . List.nubOrd . NonEmpty.toList

-- | Similar to `factIds` but generate an array of ids with the signature of the
-- first element only. This doesn't nubOrd the input set
factIdsArray :: forall p. Predicate p => [IdOf p] -> Angle [p]
factIdsArray [] = array []
factIdsArray (x:xs) = array (sig (factId_ x) : map factId_ xs)

elementsOf :: Angle [x] -> Angle x
elementsOf listOfX = Angle $ do
  xs <- gen listOfX
  return (ElementsOfArray DSL xs)

arrayPrefix :: NonEmpty (Angle x) -> Angle x
arrayPrefix pats = Angle $ do
  pats <- traverse gen pats
  return (ArrayPrefix DSL pats)

unit :: Angle ()
unit = Angle $ pure (Variable DSL "{}")

if_ :: Angle cond -> Angle a -> Angle a -> Angle a
if_ cond t e = Angle $ IfPattern DSL <$> gen cond <*> gen t <*> gen e

true :: Angle Bool
true = Angle $ pure (Variable DSL "true")

false :: Angle Bool
false = Angle $ pure (Variable DSL "false")

-- | Lift a boolean literal into Angle
bool :: Bool -> Angle Bool
bool True = true
bool False = false

just :: Angle a -> Angle (Maybe a)
just x = Angle $ do
  tx <- gen x
  pure (Struct DSL [Angle.Field "just" tx])

nothing :: Angle (Maybe a)
nothing = Angle $ pure (Variable DSL "nothing")

{-
  TODO:
  | KeyValue (SourcePat_ v t) (SourcePat_ v t)
-}


-- -----------------------------------------------------------------------------
-- Special queries

-- | Query only new facts of the given predicate, i.e. facts in the
-- topmost DB in the stack.
--
-- (This is a special-purpose query tool for building incremental fact
-- derivers)
new :: Predicate p => Angle p -> Angle p
new = specialPredicateQuery "new"

-- | Query only old facts of the given predicate, i.e. facts from all
-- but the topmost DB in the stack.
--
-- (This is a special-purpose query tool for building incremental fact
-- derivers)
old :: Predicate p => Angle p -> Angle p
old = specialPredicateQuery "old"

specialPredicateQuery :: Predicate p => Text -> Angle p -> Angle p
specialPredicateQuery suffix (Angle pat) = Angle $ do
  p <- pat
  case p of
    App DSL (Variable DSL p) args ->
      return $ App DSL (Variable DSL (p <> "#" <> suffix)) args
    _ -> error $ ": not a predicate"

-- -----------------------------------------------------------------------------
-- Type checking for records and sum types

-- | Maps a Thrift-generated record type to its fields
type family RecordFields t :: TFields

-- | Maps a Thrift-generated union type to its fields
type family SumFields t :: TFields

-- | @HasFields f t@ asserts that all the fields @f@ are present in
-- @t@, in any order.
class HasFields (f :: TFields) (t :: TFields)

instance HasFields 'TNoFields f
instance (HasField l v f, HasFields r f) => HasFields ('TField l v r) f

-- | @HasField l v f@ asserts that field @l@ is present in @f@ with type @v@
class HasField (l :: Symbol) v (f :: TFields)

instance {-# OVERLAPPING #-} (v ~ v') => HasField l v ('TField l v' f)
instance HasField l v f => HasField l v ('TField l' v' f)

data TFields where
  TNoFields :: TFields
  TField :: Symbol -> v -> TFields -> TFields
