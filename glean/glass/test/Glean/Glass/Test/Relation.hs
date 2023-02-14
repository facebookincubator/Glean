{-
  Copyright (c) Meta Platforms, Inc. and affiliates.
  All rights reserved.

  This source code is licensed under the BSD-style license found in the
  LICENSE file in the root directory of this source tree.
-}

--
-- Test inheritance-based name resolution logic for Hack
--
module Glean.Glass.Test.Relation (main) where

import Test.HUnit ( Test(..), (@=?) )

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet

import TestRunner ( testRunner )
import Glean.Init ( withUnitTest )

import Glean.Glass.Types
import qualified Glean.Glass.Relations.Hack as S

main :: IO ()
main = withUnitTest $ testRunner $ TestList
  [ TestLabel "no-conflict-is-identity" (mkTest test1)
  , TestLabel "no-conflict-is-identity-empty" (mkTest test2)
  , TestLabel "one-shadow" (mkTest test3)
  , TestLabel "full-shadow" (mkTest test4)
  , TestLabel "duplicate-full-shadow" (mkTest test5)
  , TestLabel "traits-beat-classes" (mkTest test6)
  , TestLabel "traits-recursive-order" (mkTest test7)
  , TestLabel "traits-independent" (mkTest test8)
  ]

classA, classB, classC, traitB, traitC, interfaceC :: SymAndKind
classA = sym "ClassA" `kind` SymbolKind_Class_
classB = sym "ClassB" `kind` SymbolKind_Class_
classC = sym "ClassC" `kind` SymbolKind_Class_
traitB = sym "TraitB" `kind` SymbolKind_Trait
traitC = sym "TraitC" `kind` SymbolKind_Trait
interfaceC = sym "InterfaceC" `kind` SymbolKind_Interface

test1 :: TestSpec
test1 = TestSpec contents graph (NE.tail contents) mempty
  where
    contents =
      (classA, ["a"]) :|
        [ (traitB, ["f"])
        , (interfaceC, ["g"])
        ]
    graph = (classA , [ traitB, interfaceC]) :| []

test2 :: TestSpec
test2 = TestSpec contents graph (NE.tail contents) mempty
  where
    contents =
      (classA, []) :| -- empty, still works
        [ (traitB, ["f"])
        , (interfaceC, ["g"])
        ]
    graph = (classA , [ traitB, interfaceC]) :| []

test3 :: TestSpec
test3 = TestSpec contents graph expected
    [(sym "f", qsym traitB "f")]
  where
    contents =
      (classA, ["f"]) :| -- partial shadow
        [ (traitB, ["f"])
        , (interfaceC, ["g"])
        ]
    graph = (classA , [ traitB, interfaceC]) :| []
    expected =
        [ (traitB, []) -- "f" is removed
        , (interfaceC, ["g"])
        ]

test4 :: TestSpec
test4 = TestSpec contents graph expected
    [(sym "f", qsym traitB "f")
    ,(sym "g", qsym interfaceC "g")]
  where
    contents =
      (classA, ["f", "g"]) :| -- shadows all
        [ (traitB, ["f"])
        , (interfaceC, ["g"])
        ]
    graph = (classA , [ traitB, interfaceC]) :| []
    expected =
        [ (traitB, []) -- "f" is removed
        , (interfaceC, []) -- and g"
        ]

test5 :: TestSpec
test5 = TestSpec contents graph expected
    [(sym "f", qsym traitB "f")] -- first occurrence is the override
  where
    contents =
      (classA, ["f"]) :| -- shadows all
        [ (traitB, ["f"])
        , (interfaceC, ["f"])
        ]
    graph = (classA , [ traitB, interfaceC]) :| []
    expected =
        [ (traitB, []) -- "f" is removed
        , (interfaceC, []) -- and g"
        ]

test6 :: TestSpec
test6 = TestSpec contents graph expected
  [(symbolIdOf (qsym traitB "f"), qsym classB "f")]
  where
    contents =
      (classA, []) :|
        [ (classB, ["f"])
        , (traitB, ["f"]) -- trait should win
        ]
    graph = (classA , [ classB, traitB ]) :| []
    expected =
        [ (traitB, ["f"])
        , (classB, []) -- "f" is removed and remapped to trait
        ]

test7 :: TestSpec
test7 = TestSpec contents graph expected remappings
  where
    contents =
      (classA, []) :|
        [ (classB, ["f"])
        , (traitB, ["f"]) -- trait B should win
        , (traitC, ["f"])
        ]
    graph = (classA , [ classB, traitB ]) :|
            [ (traitB , [ traitC ]) ]

    expected =
        [ (traitB, ["f"])
        , (traitC, []) -- "f" is removed
        , (classB, []) -- "f" is removed and remapped to trait
        ]
    remappings =
        [(symbolIdOf (qsym traitB "f"), qsym classB "f")
           -- ordering, traitB hides f in sibling classB
           -- if we inline traits recurisvely first, this would be traitC
        ]

test8 :: TestSpec
test8 = TestSpec contents graph expected remappings
  where
    contents =
      (classA, []) :|
        [ (classB, [])
        , (classC, [])
        , (traitB, ["f"])
        , (traitC, ["f"]) -- these are independently imported
        ]
    graph = (classA , [ classB, classC ]) :|
             [ (classB , [ traitB ])
             , (classC , [ traitC ]) ]
    expected =
        [ (traitB, ["f"])
        , (traitC, []) -- "f" is removed
        , (classB, [])
        , (classC, [])
        ]
    remappings =
        [(symbolIdOf (qsym traitB "f"), qsym traitC "f")
        ]

------------------------------------------------------------------------

mkTest :: TestSpec -> Test
mkTest TestSpec{..} = TestCase (expected @=? actual)
  where
    actual = S.difference
      (topoMap (NE.toList graph))
      (kindMap (NE.toList graph))
      (fst (fst (NE.head contents)))
      (map (mk Nothing . sym) (snd (NE.head contents)))
      (mkEnvOf (NE.tail contents))

    expected = (mkEnvOf result, HashMap.fromList overrides)

data TestSpec =
  TestSpec {
      -- inputs
    contents :: NE.NonEmpty (SymAndKind,[Text]),
    graph :: NE.NonEmpty (SymAndKind, [SymAndKind]),
      -- outputs
    result :: [(SymAndKind,[Text])],
    overrides :: [(SymbolId, TestEntity)]
  }

type SymAndKind = (SymbolId, SymbolKind)

qsym :: SymAndKind -> Text -> TestEntity
qsym parent name = case fst parent of
  SymbolId p -> TestEntity (sym (p <> "/" <> name)) name

sym :: Text -> SymbolId
sym x = SymbolId x

kind :: a -> b -> (a, b)
s `kind` k = (s, k)

mkEnvOf :: [(SymAndKind, [Text])] -> [(TestEntity, [TestEntity])]
mkEnvOf [] = []
mkEnvOf (((p,_), cs): rest) =
  (mk Nothing p , map (mk (Just p). sym) cs) : mkEnvOf rest

-- map of containers to parents
topoMap :: [(SymAndKind,[SymAndKind])] -> S.TopoMap
topoMap xs = HashMap.fromList
  [ (c, HashSet.fromList (map fst ps))
  | ((c,_), ps) <- xs
  ]

kindMap :: [(SymAndKind,[SymAndKind])] -> S.TopoKinds
kindMap xs = HashMap.fromList (map fst xs)

-- build simple fake symbol ids  with zero or one parent scope
mk :: Maybe SymbolId -> SymbolId -> TestEntity
mk Nothing theSym@(SymbolId s) = TestEntity theSym s
mk (Just (SymbolId p)) (SymbolId name) =
  TestEntity (SymbolId (p <> "/" <> name)) name

-- stub of a located entity
data TestEntity = TestEntity { symbolIdOf :: SymbolId, nameOf :: Text }
  deriving (Eq, Show)

instance S.NamedSymbol TestEntity where
  symIdOf (TestEntity symId _) = symId
  localNameOf (TestEntity _ name) = name
