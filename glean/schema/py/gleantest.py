# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict
from thrift.py3 import Struct
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just


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
    return f"glean.test.Bar.5 { angle_for(__env, arg, None) or '_' }", testBar

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestBar":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, r: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairs.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, r, 'r')])) or '_' } }}", testRevStringPairs

  @staticmethod
  def angle_query(*, x: Optional[str] = None, r: Optional["GleanTestRevStringPair"] = None) -> "GleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testStoredRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Ref.5 { angle_for(__env, arg, None) or '_' }", testRef

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestPredicate"] = None) -> "GleanTestRef":
    raise Exception("this function can only be called from @angle_query")



class GleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.KeyValue.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kstring, 'kstring'), angle_for(__env, knat, 'knat')])) or '_' } }}", testKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")



class GleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.IsThree.1 { angle_for(__env, arg, None) or '_' }", testIsThree

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "GleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")



class GleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], edge: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.EdgeWrapper.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, edge, 'edge')])) or '_' } }}", testEdgeWrapper

  @staticmethod
  def angle_query(*, edge: Optional["GleanTestEdge"] = None) -> "GleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RefRef.5 { angle_for(__env, arg, None) or '_' }", testRefRef

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestRef"] = None) -> "GleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")



class GleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], a: ast.Expr, b: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.nothingTest.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, a, 'a'), angle_for(__env, b, 'b')])) or '_' } }}", testnothingTest

  @staticmethod
  def angle_query(*, a: Optional[Union[Just[str], Just[None]]] = None, b: Optional[int] = None) -> "GleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")



class GleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.FooToFoo.5 { angle_for(__env, arg, None) or '_' }", testFooToFoo

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestFoo"] = None) -> "GleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")



class GleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], var_: ast.Expr, lit: ast.Expr, prim: ast.Expr, ap: ast.Expr, lam: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Expr.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, var_, 'var_'), angle_for(__env, lit, 'lit'), angle_for(__env, prim, 'prim'), angle_for(__env, ap, 'ap'), angle_for(__env, lam, 'lam')])) or '_' } }}", testExpr

  @staticmethod
  def angle_query_var_(*, var_: "GleanTestName") -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_lit(*, lit: int) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_prim(*, prim: "GleanTestName") -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_ap(*, ap: Tuple[()]) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_lam(*, lam: Tuple[()]) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")




class GleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevRevStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testRevRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Edge.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')])) or '_' } }}", testEdge

  @staticmethod
  def angle_query(*, parent: Optional["GleanTestNode"] = None, child: Optional["GleanTestNode"] = None) -> "GleanTestEdge":
    raise Exception("this function can only be called from @angle_query")



class GleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.SameString.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", testSameString

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestSameString":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithA.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testStoredRevStringPairWithA

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")



class GleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", testLeftOr

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr, vnat: ast.Expr, vstring: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kstring, 'kstring'), angle_for(__env, knat, 'knat'), angle_for(__env, vnat, 'vnat'), angle_for(__env, vstring, 'vstring')])) or '_' } }}", testDerivedKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None, vnat: Optional[int] = None, vstring: Optional[str] = None) -> "GleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")



class GleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.ViaStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testViaStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Qux.5 { angle_for(__env, arg, None) or '_' }", testQux

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestQux":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredDualStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testStoredDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional["GleanTestStringPair"] = None, snd: Optional["GleanTestStringPair"] = None) -> "GleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.IsGlean.1 { angle_for(__env, arg, None) or '_' }", testIsGlean

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")



class GleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue2.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kstring, 'kstring'), angle_for(__env, knat, 'knat')])) or '_' } }}", testDerivedKeyValue2

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairRec.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testRevStringPairRec

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")



class GleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, left: ast.Expr, right: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Tree.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, node, 'node'), angle_for(__env, left, 'left'), angle_for(__env, right, 'right')])) or '_' } }}", testTree

  @staticmethod
  def angle_query(*, node: Optional["GleanTestNode"] = None, left: Optional[Union[Just["GleanTestTree"], Just[None]]] = None, right: Optional[Union[Just["GleanTestTree"], Just[None]]] = None) -> "GleanTestTree":
    raise Exception("this function can only be called from @angle_query")



class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.5 { angle_for(__env, arg, None) or '_' }", testPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")



class GleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr2.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", testLeftOr2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")



class GleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], label: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Node.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, label, 'label')])) or '_' } }}", testNode

  @staticmethod
  def angle_query(*, label: Optional[str] = None) -> "GleanTestNode":
    raise Exception("this function can only be called from @angle_query")



class GleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.TreeToTree.5 { angle_for(__env, arg, None) or '_' }", testTreeToTree

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestTree"] = None) -> "GleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Name.1 { angle_for(__env, arg, None) or '_' }", testName

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestName":
    raise Exception("this function can only be called from @angle_query")



class GleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, grandparent: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.SkipRevEdge.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, grandparent, 'grandparent')])) or '_' } }}", testSkipRevEdge

  @staticmethod
  def angle_query(*, child: Optional["GleanTestNode"] = None, grandparent: Optional["GleanTestNode"] = None) -> "GleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithRev.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testStoredRevStringPairWithRev

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")



class GleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.MatchOneAlt.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", testMatchOneAlt

  @staticmethod
  def angle_query(*, x: Optional[Tuple[()]] = None, y: Optional[int] = None) -> "GleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevEdge.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, parent, 'parent')])) or '_' } }}", testRevEdge

  @staticmethod
  def angle_query(*, child: Optional["GleanTestNode"] = None, parent: Optional["GleanTestNode"] = None) -> "GleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairSum.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testStoredRevStringPairSum

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")



class GleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.EmptyStoredStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testEmptyStoredStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Unbound.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", testUnbound

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")



class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.1 { angle_for(__env, arg, None) or '_' }", testPredicate

  @staticmethod
  def angle_query(*, arg: Optional[Tuple[()]] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")



class GleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DualStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", testDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional["GleanTestStringPair"] = None, snd: Optional["GleanTestStringPair"] = None) -> "GleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Unbound2.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", testUnbound2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], box: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StringPairBox.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, box, 'box')])) or '_' } }}", testStringPairBox

  @staticmethod
  def angle_query(*, box: Optional["GleanTestStringPair"] = None) -> "GleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")



class GleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.ReflStringPair.1 { angle_for(__env, arg, None) or '_' }", testReflStringPair

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Foo.5 { angle_for(__env, arg, None) or '_' }", testFoo

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestFoo":
    raise Exception("this function can only be called from @angle_query")




