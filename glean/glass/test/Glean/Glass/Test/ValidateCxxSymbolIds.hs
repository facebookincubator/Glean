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
import Data.Int ( Int64 )

import TestRunner ( testRunner )
import Glean.Init ( withUnitTest )

import Glean.Glass.SymbolId.Cxx
  ( sidlMarker, sidlSymbolTokens, sidlKindToken, sidlKindFromToken
  , parseSidlLine, splitSidlSymbolId )
import Glean.Glass.SymbolId.Cxx.Parse
import qualified Glean.Schema.Cxx1.Types as Cxx

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
  , TestLabel "literal_operator_1" literal_operator_defn
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
  , TestLabel "conv_fn_1" conv_fn_1
  , TestLabel "conv_fn_2" conv_fn_2
  , TestLabel "conv_fn_3" conv_fn_3
  , TestLabel "marker_tokens" marker_tokens
  , TestLabel "sidl_field" sidl_field
  , TestLabel "sidl_kind_tokens" sidl_kind_tokens
  , TestLabel "sidl_kind_roundtrip" sidl_kind_roundtrip
  , TestLabel "sidl_kind_unknown" sidl_kind_unknown
  , TestLabel "sidl_roundtrip" sidl_roundtrip
  , TestLabel "sidl_encode_rejects" sidl_encode_rejects
  , TestLabel "sidl_line" sidl_line
  , TestLabel "sidl_line_rejects" sidl_line_rejects
  , TestLabel "sidl_split" sidl_split
  , TestLabel "sidl_split_rejects" sidl_split_rejects
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

literal_operator_defn :: Test
literal_operator_defn = test
  (op_sig ["const char *", "size_t"] [] <$>
    ok "fbcode" ["folly", "literals"] "operator%22%22_sp")
  (sym "fbcode/folly/literals/operator%22%22_sp/.o/const+char+*,size_t")

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

conv_fn_1 :: Test
conv_fn_1 = test
  (conv_sig "T" ["const", "lvalue"] <$>
    anon_defn "fbcode" ["foo", "anything"]) (sym
      "fbcode/foo/anything/.t/T/const,lvalue")

conv_fn_2 :: Test
conv_fn_2 = test
  (conv_sig "T" ["const", "rvalue"] <$>
    anon_defn "fbcode" ["foo", "anything"]) (sym
      "fbcode/foo/anything/.t/T/const,rvalue")

conv_fn_3 :: Test
conv_fn_3 = test
  (conv_sig "T &" [] <$>
    anon_defn "fbcode" ["foo", "anything"]) (sym
      "fbcode/foo/anything/.t/T+&")

marker_tokens :: Test
marker_tokens = test expected [ (t, isMarkerToken t) | (t, _) <- expected ]
  where
    expected =
      [ (t, True) | t <- [".decl", ".ctor", ".d", ".c", ".f", ".o", ".t"] ] ++
      [ (t, False) | t <- ["foo", "", ".", ".sidl", "d", ".x"] ]

sidl_field :: Test
sidl_field = test
  (ok ".sidl" ["field","fbcode","foo","bar.sidl","id"] "42")
  (sym ".sidl/field/fbcode/foo/bar.sidl/id/42")

sidl_kinds :: [(Cxx.SidlKind, Text)]
sidl_kinds =
  [ (Cxx.SidlKind_Struct, "struct")
  , (Cxx.SidlKind_Enum, "enum")
  , (Cxx.SidlKind_Enumerator, "enumerator")
  , (Cxx.SidlKind_Alias, "alias")
  , (Cxx.SidlKind_Const, "const")
  , (Cxx.SidlKind_Interface, "interface")
  , (Cxx.SidlKind_Method, "method")
  , (Cxx.SidlKind_Field, "field")
  ]

sidl_kind_tokens :: Test
sidl_kind_tokens = test
  (map (Just . snd) sidl_kinds)
  (map (sidlKindToken . fst) sidl_kinds)

sidl_kind_roundtrip :: Test
sidl_kind_roundtrip = test
  (map (Just . snd) sidl_kinds)
  (map (\(_, tok) -> sidlKindFromToken tok >>= sidlKindToken) sidl_kinds)

sidl_kind_unknown :: Test
sidl_kind_unknown = test (Nothing, Nothing, Nothing)
  ( sidlKindFromToken "unknown"
  , sidlKindFromToken "bogus"
  , sidlKindToken (Cxx.SidlKind__UNKNOWN 99)
  )

sidl_line :: Test
sidl_line = test
  [Just 42, Just 0]
  [parseSidlLine "42", parseSidlLine "0"]

sidl_line_rejects :: Test
sidl_line_rejects = test
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  [ parseSidlLine "-1"
  , parseSidlLine "042"
  , parseSidlLine "(42)"
  , parseSidlLine ""
  , parseSidlLine "bogus"
  , parseSidlLine "42 "
  , parseSidlLine " 42"
  , parseSidlLine "99999999999999999999999"
  ]

sidl_split :: Test
sidl_split = test
  (Just (Cxx.SidlKind_Field, "fbcode/foo/bar.sidl", "id"))
  (splitSidlSymbolId ["field","fbcode","foo","bar.sidl","id"])

sidl_split_rejects :: Test
sidl_split_rejects = test
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
  [ splitSidlSymbolId ["field","id"]
  , splitSidlSymbolId ["field","fbcode",".d","bar.sidl","id"]
  , splitSidlSymbolId ["field","fbcode","foo","bar.sidl",".f"]
  , splitSidlSymbolId ["bogus","fbcode","foo","bar.sidl","id"]
  , splitSidlSymbolId ["field","fbcode","foo","bar.sidl",""]
  , splitSidlSymbolId ["field","fbcode","","bar.sidl","id"]
  , splitSidlSymbolId ["field","fbcode","fo/o","bar.sidl","id"]
  , splitSidlSymbolId ["field","fbcode","foo","bar.sidl","i/d"]
  ]

sidl_roundtrip :: Test
sidl_roundtrip = test (map (Right . Just) decls)
  [ decodeSidl <$> sidlSymbolTokens kind file name line
  | (kind, file, name, line) <- decls
  ]
  where
    decls :: [(Cxx.SidlKind, Text, Text, Int64)]
    decls =
      [ (kind, file, name, line)
      | kind <- map fst sidl_kinds
      , (file, name, line) <-
          [ ("fbcode/foo/bar.sidl", "id", 42)
          , ("bar.sidl", "id", 0)
          , ("fbcode/.sidl/a.b/bar.sidl", "T+&", 7)
          , ("fbcode/foo/bar.sidl", "id", maxBound)
          ]
      ]

sidl_encode_rejects :: Test
sidl_encode_rejects = test
  [ Left "unknown SIDL kind"
  , Left "bad SIDL name"
  , Left "bad SIDL name"
  , Left "bad SIDL name"
  , Left "bad SIDL file path"
  , Left "bad SIDL file path"
  , Left "bad SIDL file path"
  , Left "bad SIDL line"
  ]
  [ sidlSymbolTokens (Cxx.SidlKind__UNKNOWN 99) "fbcode/bar.sidl" "id" 1
  , field "fbcode/bar.sidl" "" 1
  , field "fbcode/bar.sidl" "i/d" 1
  , field "fbcode/bar.sidl" ".f" 1
  , field "fbcode/.d/bar.sidl" "id" 1
  , field "fbcode//bar.sidl" "id" 1
  , field "/fbcode/bar.sidl" "id" 1
  , field "fbcode/bar.sidl" "id" (-1)
  ]
  where
    field = sidlSymbolTokens Cxx.SidlKind_Field

decodeSidl :: [Text] -> Maybe (Cxx.SidlKind, Text, Text, Int64)
decodeSidl toks = case sym (Text.intercalate "/" toks) of
  Right env
    | Right (CxxDefn (CxxAny path scope (Name lineTok))) <-
        compileSymbolEnv env
    , path == sidlMarker -> do
        (kind, file, name) <- splitSidlSymbolId (unNames scope)
        line <- parseSidlLine lineTok
        return (kind, file, name, line)
  _ -> Nothing

-- Helpers

test :: (Eq a, Show a) => a -> a -> Test
test expected actual = TestCase (expected @=? actual)

sym :: Text -> Either [Text] SymbolEnv
sym xs = validateSymbolId (Text.split (=='/') xs)

ok :: Text -> [Text] -> Text -> Either [Text] SymbolEnv
ok path scope name =
  (\e -> e { localname = Just (Name name) } ) <$> anon_defn path scope

anon_defn :: Text -> [Text] -> Either [Text] SymbolEnv
anon_defn path scope =
  Right $ SymbolEnv {
      path = path,
      scopes = map Name scope,
      localname = Nothing,
      declaration = False,
      tag = Nothing,
      params = [],
      returns = Nothing,
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

op_sig :: [Text] -> [Text] -> SymbolEnv -> SymbolEnv
op_sig params quals e =
   e { tag = Just Operator
     , params = map Name params
     , qualifiers = Set.fromList (mapMaybe toQualifier quals)
     }

conv_sig :: Text -> [Text] -> SymbolEnv -> SymbolEnv
conv_sig retTy quals e =
   e { tag = Just TypeConversionOperator
     , localname = Nothing
     , returns = Just (Name retTy)
     , qualifiers = Set.fromList (mapMaybe toQualifier quals)
     }
