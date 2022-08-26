# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List
from thrift.py3 import Struct
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, concatenateFields


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
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Bar.5 {{ }}", TestBar
    return f"glean.test.Bar.5 {key}", TestBar

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestBar":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.RevStringPairs.1 {{ }}", TestRevStringPairs
    return f"glean.test.RevStringPairs.1 { concatenateFields(key) }", TestRevStringPairs

  @staticmethod
  def angle_query(*, x: Optional[str] = None, r: Optional[Tuple[()]] = None) -> "GleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.StoredRevStringPair.1 {{ }}", TestStoredRevStringPair
    return f"glean.test.StoredRevStringPair.1 { concatenateFields(key) }", TestStoredRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Ref.5 {{ }}", TestRef
    return f"glean.test.Ref.5 {key}", TestRef

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.KeyValue.1 {{ }}", TestKeyValue
    return f"glean.test.KeyValue.1 { concatenateFields(key) }", TestKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.IsThree.1 {{ }}", TestIsThree
    return f"glean.test.IsThree.1 {key}", TestIsThree

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "GleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.EdgeWrapper.5 {{ }}", TestEdgeWrapper
    return f"glean.test.EdgeWrapper.5 { concatenateFields(key) }", TestEdgeWrapper

  @staticmethod
  def angle_query(*, edge: Optional[Tuple[()]] = None) -> "GleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.RefRef.5 {{ }}", TestRefRef
    return f"glean.test.RefRef.5 {key}", TestRefRef

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.nothingTest.5 {{ }}", TestNothingTest
    return f"glean.test.nothingTest.5 { concatenateFields(key) }", TestNothingTest

  @staticmethod
  def angle_query(*, a: Optional[Tuple[()]] = None, b: Optional[int] = None) -> "GleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.FooToFoo.5 {{ }}", TestFooToFoo
    return f"glean.test.FooToFoo.5 {key}", TestFooToFoo

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")

class GleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Expr.1 {{ }}", TestExpr
    return f"glean.test.Expr.1 {key}", TestExpr

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.RevRevStringPair.1 {{ }}", TestRevRevStringPair
    return f"glean.test.RevRevStringPair.1 { concatenateFields(key) }", TestRevRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Edge.5 {{ }}", TestEdge
    return f"glean.test.Edge.5 { concatenateFields(key) }", TestEdge

  @staticmethod
  def angle_query(*, parent: Optional[Tuple[()]] = None, child: Optional[Tuple[()]] = None) -> "GleanTestEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.SameString.1 {{ }}", TestSameString
    return f"glean.test.SameString.1 { concatenateFields(key) }", TestSameString

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestSameString":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.StoredRevStringPairWithA.1 {{ }}", TestStoredRevStringPairWithA
    return f"glean.test.StoredRevStringPairWithA.1 { concatenateFields(key) }", TestStoredRevStringPairWithA

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.LeftOr.1 {{ }}", TestLeftOr
    return f"glean.test.LeftOr.1 { concatenateFields(key) }", TestLeftOr

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.RevStringPair.1 {{ }}", TestRevStringPair
    return f"glean.test.RevStringPair.1 { concatenateFields(key) }", TestRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.DerivedKeyValue.1 {{ }}", TestDerivedKeyValue
    return f"glean.test.DerivedKeyValue.1 { concatenateFields(key) }", TestDerivedKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None, vnat: Optional[int] = None, vstring: Optional[str] = None) -> "GleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.ViaStringPair.1 {{ }}", TestViaStringPair
    return f"glean.test.ViaStringPair.1 { concatenateFields(key) }", TestViaStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Qux.5 {{ }}", TestQux
    return f"glean.test.Qux.5 {key}", TestQux

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestQux":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.StoredDualStringPair.1 {{ }}", TestStoredDualStringPair
    return f"glean.test.StoredDualStringPair.1 { concatenateFields(key) }", TestStoredDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional[Tuple[()]] = None, snd: Optional[Tuple[()]] = None) -> "GleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.IsGlean.1 {{ }}", TestIsGlean
    return f"glean.test.IsGlean.1 {key}", TestIsGlean

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.DerivedKeyValue2.1 {{ }}", TestDerivedKeyValue2
    return f"glean.test.DerivedKeyValue2.1 { concatenateFields(key) }", TestDerivedKeyValue2

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.RevStringPairRec.1 {{ }}", TestRevStringPairRec
    return f"glean.test.RevStringPairRec.1 { concatenateFields(key) }", TestRevStringPairRec

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Tree.5 {{ }}", TestTree
    return f"glean.test.Tree.5 { concatenateFields(key) }", TestTree

  @staticmethod
  def angle_query(*, node: Optional[Tuple[()]] = None, left: Optional[Tuple[()]] = None, right: Optional[Tuple[()]] = None) -> "GleanTestTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Predicate.5 {{ }}", TestPredicate
    return f"glean.test.Predicate.5 {key}", TestPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.LeftOr2.1 {{ }}", TestLeftOr2
    return f"glean.test.LeftOr2.1 { concatenateFields(key) }", TestLeftOr2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Node.5 {{ }}", TestNode
    return f"glean.test.Node.5 { concatenateFields(key) }", TestNode

  @staticmethod
  def angle_query(*, label: Optional[str] = None) -> "GleanTestNode":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.TreeToTree.5 {{ }}", TestTreeToTree
    return f"glean.test.TreeToTree.5 {key}", TestTreeToTree

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.StringPair.1 {{ }}", TestStringPair
    return f"glean.test.StringPair.1 { concatenateFields(key) }", TestStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Name.1 {{ }}", TestName
    return f"glean.test.Name.1 {key}", TestName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestName":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.SkipRevEdge.5 {{ }}", TestSkipRevEdge
    return f"glean.test.SkipRevEdge.5 { concatenateFields(key) }", TestSkipRevEdge

  @staticmethod
  def angle_query(*, child: Optional[Tuple[()]] = None, grandparent: Optional[Tuple[()]] = None) -> "GleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.StoredRevStringPairWithRev.1 {{ }}", TestStoredRevStringPairWithRev
    return f"glean.test.StoredRevStringPairWithRev.1 { concatenateFields(key) }", TestStoredRevStringPairWithRev

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")

class GleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.MatchOneAlt.1 {{ }}", TestMatchOneAlt
    return f"glean.test.MatchOneAlt.1 { concatenateFields(key) }", TestMatchOneAlt

  @staticmethod
  def angle_query(*, x: Optional[Tuple[()]] = None, y: Optional[int] = None) -> "GleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.RevEdge.5 {{ }}", TestRevEdge
    return f"glean.test.RevEdge.5 { concatenateFields(key) }", TestRevEdge

  @staticmethod
  def angle_query(*, child: Optional[Tuple[()]] = None, parent: Optional[Tuple[()]] = None) -> "GleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.StoredRevStringPairSum.1 {{ }}", TestStoredRevStringPairSum
    return f"glean.test.StoredRevStringPairSum.1 { concatenateFields(key) }", TestStoredRevStringPairSum

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.EmptyStoredStringPair.1 {{ }}", TestEmptyStoredStringPair
    return f"glean.test.EmptyStoredStringPair.1 { concatenateFields(key) }", TestEmptyStoredStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Unbound.1 {{ }}", TestUnbound
    return f"glean.test.Unbound.1 { concatenateFields(key) }", TestUnbound

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Predicate.1 {{ }}", TestPredicate
    return f"glean.test.Predicate.1 {key}", TestPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.DualStringPair.1 {{ }}", TestDualStringPair
    return f"glean.test.DualStringPair.1 { concatenateFields(key) }", TestDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional[Tuple[()]] = None, snd: Optional[Tuple[()]] = None) -> "GleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Unbound2.1 {{ }}", TestUnbound2
    return f"glean.test.Unbound2.1 { concatenateFields(key) }", TestUnbound2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.StringPairBox.1 {{ }}", TestStringPairBox
    return f"glean.test.StringPairBox.1 { concatenateFields(key) }", TestStringPairBox

  @staticmethod
  def angle_query(*, box: Optional[Tuple[()]] = None) -> "GleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")

class GleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.ReflStringPair.1 {{ }}", TestReflStringPair
    return f"glean.test.ReflStringPair.1 {key}", TestReflStringPair

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()], List[Tuple[str, str]]]) -> Tuple[str, Struct]:
    if key is None:
      return f"glean.test.Foo.5 {{ }}", TestFoo
    return f"glean.test.Foo.5 {key}", TestFoo

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestFoo":
    raise Exception("this function can only be called from @angle_query")


