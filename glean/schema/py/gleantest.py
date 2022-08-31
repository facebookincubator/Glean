# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R


from glean.schema.gleantest.types import (
    testBar,
    testRevStringPairs,
    testStoredRevStringPair,
    testRef,
    testKeyValue,
    testIsThree,
    testEdgeWrapper,
    testRefRef,
    testnothingTest,
    testFooToFoo,
    testExpr,
    testRevRevStringPair,
    testEdge,
    testSameString,
    testStoredRevStringPairWithA,
    testLeftOr,
    testRevStringPair,
    testDerivedKeyValue,
    testViaStringPair,
    testQux,
    testStoredDualStringPair,
    testIsGlean,
    testDerivedKeyValue2,
    testRevStringPairRec,
    testTree,
    testPredicate,
    testLeftOr2,
    testNode,
    testTreeToTree,
    testStringPair,
    testName,
    testSkipRevEdge,
    testStoredRevStringPairWithRev,
    testMatchOneAlt,
    testRevEdge,
    testStoredRevStringPairSum,
    testEmptyStoredStringPair,
    testUnbound,
    testPredicate,
    testDualStringPair,
    testUnbound2,
    testStringPairBox,
    testReflStringPair,
    testFoo,
)


class GleanTestBar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Bar.5 {angle_for(__env, arg)}", testBar

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestBar":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, r: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairs.1 {{ x = {angle_for(__env, x)}, r = {angle_for(__env, r)} }}", testRevStringPairs

  @staticmethod
  def angle_query(*, x: Optional[str] = None, r: Optional["GleanTestRevStringPair"] = None) -> "GleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testStoredRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Ref.5 {angle_for(__env, arg)}", testRef

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestPredicate"] = None) -> "GleanTestRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.KeyValue.1 {{ kstring = {angle_for(__env, kstring)}, knat = {angle_for(__env, knat)} }}", testKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.IsThree.1 {angle_for(__env, arg)}", testIsThree

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "GleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], edge: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.EdgeWrapper.5 {{ edge = {angle_for(__env, edge)} }}", testEdgeWrapper

  @staticmethod
  def angle_query(*, edge: Optional["GleanTestEdge"] = None) -> "GleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RefRef.5 {angle_for(__env, arg)}", testRefRef

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestRef"] = None) -> "GleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], a: ast.Expr, b: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.nothingTest.5 {{ a = {angle_for(__env, a)}, b = {angle_for(__env, b)} }}", testnothingTest

  @staticmethod
  def angle_query(*, a: Optional[Tuple[()]] = None, b: Optional[int] = None) -> "GleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.FooToFoo.5 {angle_for(__env, arg)}", testFooToFoo

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestFoo"] = None) -> "GleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")

class GleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Expr.1 {angle_for(__env, arg)}", testExpr

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevRevStringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testRevRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Edge.5 {{ parent = {angle_for(__env, parent)}, child = {angle_for(__env, child)} }}", testEdge

  @staticmethod
  def angle_query(*, parent: Optional["GleanTestNode"] = None, child: Optional["GleanTestNode"] = None) -> "GleanTestEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.SameString.1 {{ x = {angle_for(__env, x)}, y = {angle_for(__env, y)} }}", testSameString

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestSameString":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithA.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testStoredRevStringPairWithA

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr.1 {{ x = {angle_for(__env, x)}, y = {angle_for(__env, y)} }}", testLeftOr

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr, vnat: ast.Expr, vstring: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue.1 {{ kstring = {angle_for(__env, kstring)}, knat = {angle_for(__env, knat)}, vnat = {angle_for(__env, vnat)}, vstring = {angle_for(__env, vstring)} }}", testDerivedKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None, vnat: Optional[int] = None, vstring: Optional[str] = None) -> "GleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.ViaStringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testViaStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Qux.5 {angle_for(__env, arg)}", testQux

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestQux":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredDualStringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testStoredDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional["GleanTestStringPair"] = None, snd: Optional["GleanTestStringPair"] = None) -> "GleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.IsGlean.1 {angle_for(__env, arg)}", testIsGlean

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue2.1 {{ kstring = {angle_for(__env, kstring)}, knat = {angle_for(__env, knat)} }}", testDerivedKeyValue2

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairRec.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testRevStringPairRec

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, left: ast.Expr, right: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Tree.5 {{ node = {angle_for(__env, node)}, left = {angle_for(__env, left)}, right = {angle_for(__env, right)} }}", testTree

  @staticmethod
  def angle_query(*, node: Optional["GleanTestNode"] = None, left: Optional[Tuple[()]] = None, right: Optional[Tuple[()]] = None) -> "GleanTestTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.5 {angle_for(__env, arg)}", testPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr2.1 {{ x = {angle_for(__env, x)}, y = {angle_for(__env, y)} }}", testLeftOr2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], label: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Node.5 {{ label = {angle_for(__env, label)} }}", testNode

  @staticmethod
  def angle_query(*, label: Optional[str] = None) -> "GleanTestNode":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.TreeToTree.5 {angle_for(__env, arg)}", testTreeToTree

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestTree"] = None) -> "GleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Name.1 {angle_for(__env, arg)}", testName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestName":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, grandparent: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.SkipRevEdge.5 {{ child = {angle_for(__env, child)}, grandparent = {angle_for(__env, grandparent)} }}", testSkipRevEdge

  @staticmethod
  def angle_query(*, child: Optional["GleanTestNode"] = None, grandparent: Optional["GleanTestNode"] = None) -> "GleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithRev.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testStoredRevStringPairWithRev

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")

class GleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.MatchOneAlt.1 {{ x = {angle_for(__env, x)}, y = {angle_for(__env, y)} }}", testMatchOneAlt

  @staticmethod
  def angle_query(*, x: Optional[Tuple[()]] = None, y: Optional[int] = None) -> "GleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevEdge.5 {{ child = {angle_for(__env, child)}, parent = {angle_for(__env, parent)} }}", testRevEdge

  @staticmethod
  def angle_query(*, child: Optional["GleanTestNode"] = None, parent: Optional["GleanTestNode"] = None) -> "GleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairSum.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testStoredRevStringPairSum

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.EmptyStoredStringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testEmptyStoredStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Unbound.1 {{ x = {angle_for(__env, x)}, y = {angle_for(__env, y)} }}", testUnbound

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.1 {angle_for(__env, arg)}", testPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DualStringPair.1 {{ fst = {angle_for(__env, fst)}, snd = {angle_for(__env, snd)} }}", testDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional["GleanTestStringPair"] = None, snd: Optional["GleanTestStringPair"] = None) -> "GleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Unbound2.1 {{ x = {angle_for(__env, x)}, y = {angle_for(__env, y)} }}", testUnbound2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], box: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StringPairBox.1 {{ box = {angle_for(__env, box)} }}", testStringPairBox

  @staticmethod
  def angle_query(*, box: Optional["GleanTestStringPair"] = None) -> "GleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")

class GleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.ReflStringPair.1 {angle_for(__env, arg)}", testReflStringPair

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Foo.5 {angle_for(__env, arg)}", testFoo

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestFoo":
    raise Exception("this function can only be called from @angle_query")


