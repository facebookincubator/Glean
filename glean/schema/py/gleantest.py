# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Type, Union, TypeVar
import json
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


from glean.schema.gleantest.types import (
    TestBar,
    TestRevStringPairs,
    TestStoredRevStringPair,
    TestRef,
    TestKeyValue,
    TestIsThree,
    TestEdgeWrapper,
    TestRefRef,
    TestNothingTest,
    TestFooToFoo,
    TestExpr,
    TestRevRevStringPair,
    TestEdge,
    TestSameString,
    TestStoredRevStringPairWithA,
    TestLeftOr,
    TestRevStringPair,
    TestDerivedKeyValue,
    TestViaStringPair,
    TestQux,
    TestStoredDualStringPair,
    TestIsGlean,
    TestDerivedKeyValue2,
    TestRevStringPairRec,
    TestTree,
    TestPredicate,
    TestLeftOr2,
    TestNode,
    TestTreeToTree,
    TestStringPair,
    TestName,
    TestSkipRevEdge,
    TestStoredRevStringPairWithRev,
    TestMatchOneAlt,
    TestRevEdge,
    TestStoredRevStringPairSum,
    TestEmptyStoredStringPair,
    TestUnbound,
    TestPredicate,
    TestDualStringPair,
    TestUnbound2,
    TestStringPairBox,
    TestReflStringPair,
    TestFoo,
)


class GleanTestBar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Bar.5 {json.dumps(key)}", TestBar

  @staticmethod
  def angle_query(*, arg: str) -> "GleanTestBar":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairs.1 {{ x = _, r = _ }}", TestRevStringPairs

  @staticmethod
  def angle_query(*, x: str, r: Tuple[()]) -> "GleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPair.1 {{ fst = _, snd = _ }}", TestStoredRevStringPair

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Ref.5 {json.dumps(key)}", TestRef

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "GleanTestRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.KeyValue.1 {{ kstring = _, knat = _ }}", TestKeyValue

  @staticmethod
  def angle_query(*, kstring: str, knat: int) -> "GleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.IsThree.1 {json.dumps(key)}", TestIsThree

  @staticmethod
  def angle_query(*, arg: int) -> "GleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.EdgeWrapper.5 {{ edge = _ }}", TestEdgeWrapper

  @staticmethod
  def angle_query(*, edge: Tuple[()]) -> "GleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RefRef.5 {json.dumps(key)}", TestRefRef

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "GleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.nothingTest.5 {{ a = _, b = _ }}", TestNothingTest

  @staticmethod
  def angle_query(*, a: Tuple[()], b: int) -> "GleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.FooToFoo.5 {json.dumps(key)}", TestFooToFoo

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "GleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")

class GleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Expr.1 {json.dumps(key)}", TestExpr

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevRevStringPair.1 {{ fst = _, snd = _ }}", TestRevRevStringPair

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Edge.5 {{ parent = _, child = _ }}", TestEdge

  @staticmethod
  def angle_query(*, parent: Tuple[()], child: Tuple[()]) -> "GleanTestEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.SameString.1 {{ x = _, y = _ }}", TestSameString

  @staticmethod
  def angle_query(*, x: str, y: str) -> "GleanTestSameString":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithA.1 {{ fst = _, snd = _ }}", TestStoredRevStringPairWithA

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr.1 {{ x = _, y = _ }}", TestLeftOr

  @staticmethod
  def angle_query(*, x: str, y: int) -> "GleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPair.1 {{ fst = _, snd = _ }}", TestRevStringPair

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue.1 {{ kstring = _, knat = _, vnat = _, vstring = _ }}", TestDerivedKeyValue

  @staticmethod
  def angle_query(*, kstring: str, knat: int, vnat: int, vstring: str) -> "GleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.ViaStringPair.1 {{ fst = _, snd = _ }}", TestViaStringPair

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Qux.5 {json.dumps(key)}", TestQux

  @staticmethod
  def angle_query(*, arg: str) -> "GleanTestQux":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredDualStringPair.1 {{ fst = _, snd = _ }}", TestStoredDualStringPair

  @staticmethod
  def angle_query(*, fst: Tuple[()], snd: Tuple[()]) -> "GleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.IsGlean.1 {json.dumps(key)}", TestIsGlean

  @staticmethod
  def angle_query(*, arg: str) -> "GleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue2.1 {{ kstring = _, knat = _ }}", TestDerivedKeyValue2

  @staticmethod
  def angle_query(*, kstring: str, knat: int) -> "GleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairRec.1 {{ fst = _, snd = _ }}", TestRevStringPairRec

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Tree.5 {{ node = _, left = _, right = _ }}", TestTree

  @staticmethod
  def angle_query(*, node: Tuple[()], left: Tuple[()], right: Tuple[()]) -> "GleanTestTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.5 {json.dumps(key)}", TestPredicate

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr2.1 {{ x = _, y = _ }}", TestLeftOr2

  @staticmethod
  def angle_query(*, x: str, y: int) -> "GleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Node.5 {{ label = _ }}", TestNode

  @staticmethod
  def angle_query(*, label: str) -> "GleanTestNode":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.TreeToTree.5 {json.dumps(key)}", TestTreeToTree

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "GleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StringPair.1 {{ fst = _, snd = _ }}", TestStringPair

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Name.1 {json.dumps(key)}", TestName

  @staticmethod
  def angle_query(*, arg: str) -> "GleanTestName":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.SkipRevEdge.5 {{ child = _, grandparent = _ }}", TestSkipRevEdge

  @staticmethod
  def angle_query(*, child: Tuple[()], grandparent: Tuple[()]) -> "GleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithRev.1 {{ fst = _, snd = _ }}", TestStoredRevStringPairWithRev

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")

class GleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.MatchOneAlt.1 {{ x = _, y = _ }}", TestMatchOneAlt

  @staticmethod
  def angle_query(*, x: Tuple[()], y: int) -> "GleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevEdge.5 {{ child = _, parent = _ }}", TestRevEdge

  @staticmethod
  def angle_query(*, child: Tuple[()], parent: Tuple[()]) -> "GleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairSum.1 {{ fst = _, snd = _ }}", TestStoredRevStringPairSum

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.EmptyStoredStringPair.1 {{ fst = _, snd = _ }}", TestEmptyStoredStringPair

  @staticmethod
  def angle_query(*, fst: str, snd: str) -> "GleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Unbound.1 {{ x = _, y = _ }}", TestUnbound

  @staticmethod
  def angle_query(*, x: str, y: str) -> "GleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.1 {json.dumps(key)}", TestPredicate

  @staticmethod
  def angle_query(*, arg: Tuple[()]) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.DualStringPair.1 {{ fst = _, snd = _ }}", TestDualStringPair

  @staticmethod
  def angle_query(*, fst: Tuple[()], snd: Tuple[()]) -> "GleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Unbound2.1 {{ x = _, y = _ }}", TestUnbound2

  @staticmethod
  def angle_query(*, x: str, y: str) -> "GleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StringPairBox.1 {{ box = _ }}", TestStringPairBox

  @staticmethod
  def angle_query(*, box: Tuple[()]) -> "GleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")

class GleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.ReflStringPair.1 {json.dumps(key)}", TestReflStringPair

  @staticmethod
  def angle_query(*, arg: str) -> "GleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Foo.5 {json.dumps(key)}", TestFoo

  @staticmethod
  def angle_query(*, arg: str) -> "GleanTestFoo":
    raise Exception("this function can only be called from @angle_query")


