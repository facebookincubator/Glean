{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- Unit tests for C++ symbol ids
--
module Glean.Glass.Test.ValidateCxxSymbolIds (main) where

import Test.HUnit ( Test(..), (@=?) )
import Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Maybe

import TestRunner ( testRunner )
import Glean.Init ( withUnitTest )

import Glean.Glass.SymbolId.Cxx.Parse

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "namespace1" namespace1
  , TestLabel "struct1_decl" struct1_decl
  , TestLabel "struct1_defn" struct1_defn
  , TestLabel "scope_decl" scope_decl
  , TestLabel "default_ctor" default_ctor
  , TestLabel "ctor_sig1" ctor_sig1
  , TestLabel "ctor_sig2" ctor_sig2
  , TestLabel "ctor_param" ctor_param
  , TestLabel "operator_1" operator_decl
  , TestLabel "anon_ns" anon_ns
  , TestLabel "nested comma" ctor_sig3_nested_comma
  , TestLabel "const_fn" const_fn
  , TestLabel "const_fn_decl" const_fn_decl
  , TestLabel "const_fn_decl_qual" const_fn_decl_qual
  , TestLabel "const_fn_decl_qual2" const_fn_decl_qual2
  , TestLabel "const_fn_decl_qual_rvalue" const_fn_decl_qual_rvalue
  , TestLabel "unary_fn" unary_fn
  , TestLabel "unary_fn_decl_qual2" unary_fn_decl_qual2
  , TestLabel "binary_fn" binary_fn
  ]

namespace1 :: Test
namespace1 = test
  (ok "fbcode" [] "folly")
  (sym "fbcode/folly")

struct1_decl :: Test
struct1_decl = test
  (ok_decl "fbcode" ["folly"] "dynamic")
  (sym "fbcode/folly/dynamic/.decl")

struct1_defn :: Test
struct1_defn = test
  (ok "fbcode" ["folly"] "dynamic")
  (sym "fbcode/folly/dynamic")

scope_decl :: Test
scope_decl = test
  (ok_decl "fbcode" ["folly","dynamic"] "field")
  (sym "fbcode/folly/dynamic/field/.decl")

default_ctor :: Test
default_ctor = test
  (ok_ctor_decl "fbcode" ["folly","dynamic"])
  (sym "fbcode/folly/dynamic/.c/.decl")

ctor_sig1 :: Test
ctor_sig1 = test
  (ok_ctor_sig_decl "fbcode" ["folly","dynamic"] ["std::nullptr_t"])
  (sym "fbcode/folly/dynamic/.c/std::nullptr_t/.decl")

ctor_sig2 :: Test
ctor_sig2 = test
  (ok_ctor_sig_decl "fbcode" ["folly","dynamic"] ["int *","char *"])
  (sym "fbcode/folly/dynamic/.c/int+*,char+*/.decl")

-- test nested comma escaping
ctor_sig3_nested_comma :: Test
ctor_sig3_nested_comma = test
  (ok_ctor_sig_decl "fbcode" ["folly","dynamic"]
    ["int *","std::pair<char *, bool>"])
  (sym "fbcode/folly/dynamic/.c/int+*,std::pair<char+* +bool>/.decl")

ctor_param :: Test
ctor_param = test
  (ctor <$> ok_decl "fbcode" ["folly","dynamic"] "t")
  (sym "fbcode/folly/dynamic/.ctor/t/.decl")

operator_decl :: Test
operator_decl = test
  (ok_decl "fbcode" ["std", "__shared_ptr_access"] "operator-%3E")
  (sym "fbcode/std/__shared_ptr_access/operator-%3E/.decl")

anon_ns :: Test
anon_ns = test
  (ok "fbcode" ["folly",""] "Test")
  (sym "fbcode/folly//Test")

const_fn :: Test
const_fn = test
  (fun_sig [] [] <$> ok "fbcode" ["folly", "dynamic"] "foo")
  (sym "fbcode/folly/dynamic/foo/.f/")

const_fn_decl :: Test
const_fn_decl = test
  (fun_sig [] [] <$> ok_decl "fbcode" ["folly", "dynamic"] "foo")
  (sym "fbcode/folly/dynamic/foo/.f//.decl")

const_fn_decl_qual :: Test
const_fn_decl_qual = test
  (fun_sig [] ["const"] <$> ok_decl "fbcode" ["folly", "dynamic"] "foo")
  (sym "fbcode/folly/dynamic/foo/.f//const/.decl")

const_fn_decl_qual_rvalue :: Test
const_fn_decl_qual_rvalue = test
  (fun_sig [] ["rvalue"] <$> ok_decl "fbcode" ["folly", "dynamic"] "foo")
  (sym "fbcode/folly/dynamic/foo/.f//rvalue/.decl")

const_fn_decl_qual2 :: Test
const_fn_decl_qual2 = test
  (fun_sig [] ["const","lvalue"] <$> ok_decl "fbcode" ["folly", "dynamic"] "foo")
  (sym "fbcode/folly/dynamic/foo/.f//const,lvalue/.decl")

unary_fn_decl_qual2 :: Test
unary_fn_decl_qual2 = test
  (fun_sig ["const folly::dynamic &"] ["const","lvalue"] <$>
    ok_decl "fbcode" ["folly", "dynamic"] "foo") (sym
      "fbcode/folly/dynamic/foo/.f/const+folly::dynamic+&/const,lvalue/.decl")

unary_fn :: Test
unary_fn = test
  (fun_sig ["bool"] [] <$> ok_decl "fbcode" ["folly", "dynamic"] "foo")
  (sym "fbcode/folly/dynamic/foo/.f/bool/.decl")

binary_fn :: Test
binary_fn = test
  (fun_sig ["bool","bool"] [] <$> ok_decl "fbcode" ["folly", "dynamic"] "foo")
  (sym "fbcode/folly/dynamic/foo/.f/bool,bool/.decl")

-- Helpers

test :: (Eq a, Show a) => a -> a -> Test
test expected actual = TestCase (expected @=? actual)

sym :: Text -> Either [Text] SymbolEnv
sym xs = validateSymbolId (Text.split (=='/') xs)

ok :: Text -> [Text] -> Text -> Either [Text] SymbolEnv
ok path scope name =
  Right $ SymbolEnv {
      path = path,
      scopes = map Name scope,
      localname = Just (Name name),
      declaration = False,
      tag = Nothing,
      params = [],
      qualifiers = mempty,
      errors = []
    }

ok_decl :: Text -> [Text] -> Text -> Either [Text] SymbolEnv
ok_decl a b c = (\e -> e { declaration = True }) <$> ok a b c

ok_ctor_decl :: Text -> [Text] -> Either [Text] SymbolEnv
ok_ctor_decl a b = ctor_sig <$> ok_decl a b ""

ok_ctor_sig_decl :: Text -> [Text] -> [Text] -> Either [Text] SymbolEnv
ok_ctor_sig_decl a b ps =
   (\e -> e { params = map Name ps })
   . ctor_sig
   <$> ok_decl a b ""

ctor :: SymbolEnv -> SymbolEnv
ctor e = e { tag = Just Constructor }

ctor_sig :: SymbolEnv -> SymbolEnv
ctor_sig e = e { tag = Just CTorSignature, localname = Nothing }

fun_sig :: [Text] -> [Text] -> SymbolEnv -> SymbolEnv
fun_sig params quals e =
   e { tag = Just Function
     , params = map Name params
     , qualifiers = Set.fromList (mapMaybe toQualifier quals)
     }
