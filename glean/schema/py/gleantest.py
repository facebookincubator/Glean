# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union
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
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestBar":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairs.1 {{ x = _, r = _ }}", TestRevStringPairs

  @staticmethod
  def angle_query(*, x: Optional[str] = None, r: Optional[Tuple[()]] = None) -> "GleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPair.1 {{ fst = _, snd = _ }}", TestStoredRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Ref.5 {json.dumps(key)}", TestRef

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.KeyValue.1 {{ kstring = _, knat = _ }}", TestKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.IsThree.1 {json.dumps(key)}", TestIsThree

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "GleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.EdgeWrapper.5 {{ edge = _ }}", TestEdgeWrapper

  @staticmethod
  def angle_query(*, edge: Optional[Tuple[()]] = None) -> "GleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RefRef.5 {json.dumps(key)}", TestRefRef

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.nothingTest.5 {{ a = _, b = _ }}", TestNothingTest

  @staticmethod
  def angle_query(*, a: Optional[Tuple[()]] = None, b: Optional[int] = None) -> "GleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.FooToFoo.5 {json.dumps(key)}", TestFooToFoo

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")

class GleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Expr.1 {json.dumps(key)}", TestExpr

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevRevStringPair.1 {{ fst = _, snd = _ }}", TestRevRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Edge.5 {{ parent = _, child = _ }}", TestEdge

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "GleanTestEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.SameString.1 {{ x = _, y = _ }}", TestSameString

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestSameString":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithA.1 {{ fst = _, snd = _ }}", TestStoredRevStringPairWithA

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr.1 {{ x = _, y = _ }}", TestLeftOr

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPair.1 {{ fst = _, snd = _ }}", TestRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue.1 {{ kstring = _, knat = _, vnat = _, vstring = _ }}", TestDerivedKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None, vnat: Optional[int] = None, vstring: Optional[str] = None) -> "GleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.ViaStringPair.1 {{ fst = _, snd = _ }}", TestViaStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Qux.5 {json.dumps(key)}", TestQux

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestQux":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredDualStringPair.1 {{ fst = _, snd = _ }}", TestStoredDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional[Tuple[()]] = None, snd: Optional[Tuple[()]] = None) -> "GleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.IsGlean.1 {json.dumps(key)}", TestIsGlean

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue2.1 {{ kstring = _, knat = _ }}", TestDerivedKeyValue2

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairRec.1 {{ fst = _, snd = _ }}", TestRevStringPairRec

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Tree.5 {{ node = _, left = _, right = _ }}", TestTree

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, left: Optional[Tuple[()]] = None, right: Optional[Tuple[()]] = None) -> "GleanTestTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.5 {json.dumps(key)}", TestPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr2.1 {{ x = _, y = _ }}", TestLeftOr2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Node.5 {{ label = _ }}", TestNode

  @staticmethod
  def angle_query(*, label: Optional[str] = None) -> "GleanTestNode":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.TreeToTree.5 {json.dumps(key)}", TestTreeToTree

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StringPair.1 {{ fst = _, snd = _ }}", TestStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Name.1 {json.dumps(key)}", TestName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestName":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.SkipRevEdge.5 {{ child = _, grandparent = _ }}", TestSkipRevEdge

  @staticmethod
  def angle_query(*, child: Optional[Tuple[()]] = None, grandparent: Optional[Tuple[()]] = None) -> "GleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithRev.1 {{ fst = _, snd = _ }}", TestStoredRevStringPairWithRev

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")

class GleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.MatchOneAlt.1 {{ x = _, y = _ }}", TestMatchOneAlt

  @staticmethod
  def angle_query(*, x: Optional[Tuple[()]] = None, y: Optional[int] = None) -> "GleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.RevEdge.5 {{ child = _, parent = _ }}", TestRevEdge

  @staticmethod
  def angle_query(*, child: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "GleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairSum.1 {{ fst = _, snd = _ }}", TestStoredRevStringPairSum

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.EmptyStoredStringPair.1 {{ fst = _, snd = _ }}", TestEmptyStoredStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Unbound.1 {{ x = _, y = _ }}", TestUnbound

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.1 {json.dumps(key)}", TestPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.DualStringPair.1 {{ fst = _, snd = _ }}", TestDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional[Tuple[()]] = None, snd: Optional[Tuple[()]] = None) -> "GleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Unbound2.1 {{ x = _, y = _ }}", TestUnbound2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.StringPairBox.1 {{ box = _ }}", TestStringPairBox

  @staticmethod
  def angle_query(*, box: Optional[Tuple[()]] = None) -> "GleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")

class GleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.ReflStringPair.1 {json.dumps(key)}", TestReflStringPair

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> Tuple[str, Struct]:
    return f"glean.test.Foo.5 {json.dumps(key)}", TestFoo

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestFoo":
    raise Exception("this function can only be called from @angle_query")


