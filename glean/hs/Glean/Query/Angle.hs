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
  , vars
  , predicate
  , where_
  , not_
  , (.=)
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
  , elementsOf
  , arrayPrefix
  , unit
  , true
  , false
  , just
  , nothing
  , display
  , RecordFields
  , SumFields
  , TFields(..)
  , HasFields
  , HasField
  , AngleEnum(..)
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

import Glean.Angle.Types (SourcePat', SourceType,
  SourceStatement', SourceQuery')
import Glean.Query.Types hiding (Field, SourceStatement)
import qualified Glean.Query.Types as Angle
import Glean.Query.Thrift.Internal as Thrift hiding (query)
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
display (Angle m) = render $
  case evalState m 0 of
    NestedQuery _ q -> q
    t -> SourceQuery (Just t) []

-- | Build a query. It can returns facts of a predicate:
--
-- >    query $ predicate @Pp.Define (field @"macro" "NULL" end)
--
-- Or arbitrary values:
--
-- >    query $ var $ \n ->
-- >      n `where_` [
-- >        wild .= predicate @Hack.MethodDeclaration $
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

render :: SourceQuery -> Text
render q = renderStrict (layoutCompact (pretty q))

class AngleVars f r where
  -- | Use `vars` to batch up a series of nested `var` calls:
  --
  -- > let foo :: Angle a -> Angle b -> Angle c -> Angle d -> Angle e
  -- > vars foo == var $ \a -> var $ \b -> var $ \c -> var $ \d -> foo a b c d
  vars :: f -> r

instance AngleVars (Angle b) (Angle b) where
  vars = id

instance (AngleVars s (Angle r)) => AngleVars (Angle a -> s) (Angle r) where
  vars f = var $ \ a -> vars (f a)

-- | Introduce an Angle variable
var :: forall a b . (Angle a -> Angle b) -> Angle b
var f = Angle $ do
  x <- get
  modify (+1)
  gen $ f (Angle (pure (Variable DSL ("X" <> showt x))))

predicate :: forall p . Predicate p => Angle (KeyType p) -> Angle p
predicate (Angle pat) = Angle $ do
  p <- pat
  return $ App DSL (Variable DSL (predicateRef (Proxy @p))) [p]

-- | Build a query of the form `X where statements`
where_ :: Angle t -> [AngleStatement] -> Angle t
where_ t stmts = Angle $
  NestedQuery DSL <$>
    (SourceQuery <$> (Just <$> gen t) <*> mapM genStmt stmts)

not_ :: [AngleStatement] -> AngleStatement
not_ stmts = unit' .= Angle (Negation DSL <$> gen t)
  where
    t = unit' `where_` stmts
    unit' = sig unit

-- | Build a statement, `A = B`
(.=) :: Angle t -> Angle t -> AngleStatement
l .= r = AngleStatement $ Angle.SourceStatement <$> gen l <*> gen r

-- | Build an or-pattern, `A | B`
(.|) :: Angle a -> Angle a -> Angle a
a .| b = Angle $ OrPattern DSL <$> gen a <*> gen b

-- | Build an or-pattern between statements
or_ :: [AngleStatement] -> [AngleStatement] -> Angle ()
or_ left right = (unit' `where_` left) .| (unit' `where_` right)
  where
  unit' = sig unit

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

type SourceField = Angle.Field SpanAngleDSL Name SourceType

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
-- >      wild .= predicate @Hack.MethodDeclaration $
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
factIds :: forall p. Predicate p => NonEmpty (IdOf p) -> Angle p
factIds xs = sig $ foldr1 (.|) (fmap factId_ (nubOrd' xs))
  where
    -- Our version of package extra lacks Data.List.NonEmpty.Extra
    nubOrd' :: Ord a => NonEmpty a -> NonEmpty a
    nubOrd' = NonEmpty.fromList . List.nubOrd . NonEmpty.toList

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

true :: Angle Bool
true = Angle $ pure (Variable DSL "true")

false :: Angle Bool
false = Angle $ pure (Variable DSL "false")

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
