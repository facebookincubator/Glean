# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Optional, Tuple, Union, List, Dict, TypeVar
from thrift.py3 import Struct
from enum import Enum
import ast
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate, angle_for, R, Just, InnerGleanSchemaPredicate
from glean.schema.py.codecxx import *
from glean.schema.py.pp1 import *
from glean.schema.py.sys import *


from glean.schema.glean_test.types import (
    Bar,
    RevStringPairs,
    StoredRevStringPair,
    Ref,
    KeyValue,
    IsThree,
    EdgeWrapper,
    RefRef,
    nothingTest,
    FooToFoo,
    Expr,
    RevRevStringPair,
    Edge,
    SameString,
    StoredRevStringPairWithA,
    LeftOr,
    RevStringPair,
    DerivedKeyValue,
    ViaStringPair,
    Qux,
    StoredDualStringPair,
    IsGlean,
    DerivedKeyValue2,
    RevStringPairRec,
    Tree,
    Predicate,
    LeftOr2,
    Node,
    TreeToTree,
    StringPair,
    Name,
    SkipRevEdge,
    StoredRevStringPairWithRev,
    MatchOneAlt,
    RevEdge,
    StoredRevStringPairSum,
    EmptyStoredStringPair,
    Unbound,
    Predicate,
    DualStringPair,
    Unbound2,
    StringPairBox,
    ReflStringPair,
    Foo,
    KitchenSink,
    Entity,
    KitchenSink,
    Enum,
    WrappedStringPair,
    EdgeSum,
    Rec,
    Sum,
)


class GleanTestBar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Bar.5 { angle_for(__env, arg, None) or '_' }", Bar

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestBar":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, r: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairs.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, r, 'r')])) or '_' } }}", RevStringPairs

  @staticmethod
  def angle_query(*, x: Optional[str] = None, r: Optional["GleanTestRevStringPair"] = None) -> "GleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", StoredRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Ref.5 { angle_for(__env, arg, None) or '_' }", Ref

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestPredicate"] = None) -> "GleanTestRef":
    raise Exception("this function can only be called from @angle_query")



class GleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.KeyValue.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kstring, 'kstring'), angle_for(__env, knat, 'knat')])) or '_' } }}", KeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")



class GleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.IsThree.1 { angle_for(__env, arg, None) or '_' }", IsThree

  @staticmethod
  def angle_query(*, arg: Optional[int] = None) -> "GleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")



class GleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], edge: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.EdgeWrapper.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, edge, 'edge')])) or '_' } }}", EdgeWrapper

  @staticmethod
  def angle_query(*, edge: Optional["GleanTestEdge"] = None) -> "GleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RefRef.5 { angle_for(__env, arg, None) or '_' }", RefRef

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestRef"] = None) -> "GleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")



class GleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], a: ast.Expr, b: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.nothingTest.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, a, 'a'), angle_for(__env, b, 'b')])) or '_' } }}", nothingTest

  @staticmethod
  def angle_query(*, a: Optional[Union[Just[str], Just[None]]] = None, b: Optional[int] = None) -> "GleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")



class GleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.FooToFoo.5 { angle_for(__env, arg, None) or '_' }", FooToFoo

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestFoo"] = None) -> "GleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")



class GleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], var_: ast.Expr, lit: ast.Expr, prim: ast.Expr, ap: ast.Expr, lam: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Expr.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, var_, 'var_'), angle_for(__env, lit, 'lit'), angle_for(__env, prim, 'prim'), angle_for(__env, ap, 'ap'), angle_for(__env, lam, 'lam')])) or '_' } }}", Expr

  @staticmethod
  def angle_query_var_(*, var_: Optional["GleanTestName"] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_lit(*, lit: Optional[int] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_prim(*, prim: Optional["GleanTestName"] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_ap(*, ap: Optional['GleanTestExpr_ap'] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_lam(*, lam: Optional['GleanTestExpr_lam'] = None) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")


class GleanTestExpr_ap(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fun: ast.Expr, arg: ast.Expr) -> Tuple[str, Struct]:
    return f" {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fun, 'fun'), angle_for(__env, arg, 'arg')])) or '_' } }}", GleanTestExpr_ap

  @staticmethod
  def angle_query(*, fun: Optional["GleanTestExpr"] = None, arg: Optional["GleanTestExpr"] = None) -> "GleanTestExpr_ap":
    raise Exception("this function can only be called from @angle_query")



class GleanTestExpr_lam(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], var_: ast.Expr, body: ast.Expr) -> Tuple[str, Struct]:
    return f" {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, var_, 'var_'), angle_for(__env, body, 'body')])) or '_' } }}", GleanTestExpr_lam

  @staticmethod
  def angle_query(*, var_: Optional["GleanTestName"] = None, body: Optional["GleanTestExpr"] = None) -> "GleanTestExpr_lam":
    raise Exception("this function can only be called from @angle_query")





class GleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevRevStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", RevRevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], parent: ast.Expr, child: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Edge.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, parent, 'parent'), angle_for(__env, child, 'child')])) or '_' } }}", Edge

  @staticmethod
  def angle_query(*, parent: Optional["GleanTestNode"] = None, child: Optional["GleanTestNode"] = None) -> "GleanTestEdge":
    raise Exception("this function can only be called from @angle_query")



class GleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.SameString.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", SameString

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestSameString":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithA.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", StoredRevStringPairWithA

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")



class GleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", LeftOr

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", RevStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr, vnat: ast.Expr, vstring: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kstring, 'kstring'), angle_for(__env, knat, 'knat'), angle_for(__env, vnat, 'vnat'), angle_for(__env, vstring, 'vstring')])) or '_' } }}", DerivedKeyValue

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None, vnat: Optional[int] = None, vstring: Optional[str] = None) -> "GleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")



class GleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.ViaStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", ViaStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Qux.5 { angle_for(__env, arg, None) or '_' }", Qux

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestQux":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredDualStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", StoredDualStringPair

  @staticmethod
  def angle_query(*, fst: Optional["GleanTestStringPair"] = None, snd: Optional["GleanTestStringPair"] = None) -> "GleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.IsGlean.1 { angle_for(__env, arg, None) or '_' }", IsGlean

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")



class GleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], kstring: ast.Expr, knat: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DerivedKeyValue2.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, kstring, 'kstring'), angle_for(__env, knat, 'knat')])) or '_' } }}", DerivedKeyValue2

  @staticmethod
  def angle_query(*, kstring: Optional[str] = None, knat: Optional[int] = None) -> "GleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevStringPairRec.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", RevStringPairRec

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")



class GleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], node: ast.Expr, left: ast.Expr, right: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Tree.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, node, 'node'), angle_for(__env, left, 'left'), angle_for(__env, right, 'right')])) or '_' } }}", Tree

  @staticmethod
  def angle_query(*, node: Optional["GleanTestNode"] = None, left: Optional[Union[Just["GleanTestTree"], Just[None]]] = None, right: Optional[Union[Just["GleanTestTree"], Just[None]]] = None) -> "GleanTestTree":
    raise Exception("this function can only be called from @angle_query")



class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.5 { angle_for(__env, arg, None) or '_' }", Predicate

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestKitchenSink"] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")



class GleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.LeftOr2.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", LeftOr2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[int] = None) -> "GleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")



class GleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], label: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Node.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, label, 'label')])) or '_' } }}", Node

  @staticmethod
  def angle_query(*, label: Optional[str] = None) -> "GleanTestNode":
    raise Exception("this function can only be called from @angle_query")



class GleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.TreeToTree.5 { angle_for(__env, arg, None) or '_' }", TreeToTree

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestTree"] = None) -> "GleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", StringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Name.1 { angle_for(__env, arg, None) or '_' }", Name

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestName":
    raise Exception("this function can only be called from @angle_query")



class GleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, grandparent: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.SkipRevEdge.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, grandparent, 'grandparent')])) or '_' } }}", SkipRevEdge

  @staticmethod
  def angle_query(*, child: Optional["GleanTestNode"] = None, grandparent: Optional["GleanTestNode"] = None) -> "GleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairWithRev.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", StoredRevStringPairWithRev

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")



class GleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.MatchOneAlt.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", MatchOneAlt

  @staticmethod
  def angle_query(*, x: Optional["GleanTestSum"] = None, y: Optional[int] = None) -> "GleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")



class GleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], child: ast.Expr, parent: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.RevEdge.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, child, 'child'), angle_for(__env, parent, 'parent')])) or '_' } }}", RevEdge

  @staticmethod
  def angle_query(*, child: Optional["GleanTestNode"] = None, parent: Optional["GleanTestNode"] = None) -> "GleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StoredRevStringPairSum.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", StoredRevStringPairSum

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")



class GleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.EmptyStoredStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", EmptyStoredStringPair

  @staticmethod
  def angle_query(*, fst: Optional[str] = None, snd: Optional[str] = None) -> "GleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Unbound.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", Unbound

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")



class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Predicate.1 { angle_for(__env, arg, None) or '_' }", Predicate

  @staticmethod
  def angle_query(*, arg: Optional["GleanTestKitchenSink_1"] = None) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")



class GleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.DualStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", DualStringPair

  @staticmethod
  def angle_query(*, fst: Optional["GleanTestStringPair"] = None, snd: Optional["GleanTestStringPair"] = None) -> "GleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], x: ast.Expr, y: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Unbound2.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, x, 'x'), angle_for(__env, y, 'y')])) or '_' } }}", Unbound2

  @staticmethod
  def angle_query(*, x: Optional[str] = None, y: Optional[str] = None) -> "GleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")



class GleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], box: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.StringPairBox.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, box, 'box')])) or '_' } }}", StringPairBox

  @staticmethod
  def angle_query(*, box: Optional["GleanTestStringPair"] = None) -> "GleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")



class GleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.ReflStringPair.1 { angle_for(__env, arg, None) or '_' }", ReflStringPair

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], arg: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Foo.5 { angle_for(__env, arg, None) or '_' }", Foo

  @staticmethod
  def angle_query(*, arg: Optional[str] = None) -> "GleanTestFoo":
    raise Exception("this function can only be called from @angle_query")





class GleanTestKitchenSink(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], byt: ast.Expr, nat: ast.Expr, bool_: ast.Expr, string_: ast.Expr, pred: ast.Expr, maybe_: ast.Expr, record_: ast.Expr, sum_: ast.Expr, enum_: ast.Expr, named_record_: ast.Expr, named_sum_: ast.Expr, named_enum_: ast.Expr, array_of_byte: ast.Expr, array_of_nat: ast.Expr, array_of_bool: ast.Expr, array_of_string: ast.Expr, array_of_pred: ast.Expr, array_of_named_record: ast.Expr, array_of_named_sum: ast.Expr, array_of_named_enum: ast.Expr, array2_of_byte: ast.Expr, array2_of_nat: ast.Expr, array2_of_bool: ast.Expr, array2_of_string: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.KitchenSink.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, byt, 'byt'), angle_for(__env, nat, 'nat'), angle_for(__env, bool_, 'bool_'), angle_for(__env, string_, 'string_'), angle_for(__env, pred, 'pred'), angle_for(__env, maybe_, 'maybe_'), angle_for(__env, record_, 'record_'), angle_for(__env, sum_, 'sum_'), angle_for(__env, enum_, 'enum_'), angle_for(__env, named_record_, 'named_record_'), angle_for(__env, named_sum_, 'named_sum_'), angle_for(__env, named_enum_, 'named_enum_'), angle_for(__env, array_of_byte, 'array_of_byte'), angle_for(__env, array_of_nat, 'array_of_nat'), angle_for(__env, array_of_bool, 'array_of_bool'), angle_for(__env, array_of_string, 'array_of_string'), angle_for(__env, array_of_pred, 'array_of_pred'), angle_for(__env, array_of_named_record, 'array_of_named_record'), angle_for(__env, array_of_named_sum, 'array_of_named_sum'), angle_for(__env, array_of_named_enum, 'array_of_named_enum'), angle_for(__env, array2_of_byte, 'array2_of_byte'), angle_for(__env, array2_of_nat, 'array2_of_nat'), angle_for(__env, array2_of_bool, 'array2_of_bool'), angle_for(__env, array2_of_string, 'array2_of_string')])) or '_' } }}", KitchenSink

  @staticmethod
  def angle_query(*, byt: Optional[bytes] = None, nat: Optional[int] = None, bool_: Optional[bool] = None, string_: Optional[str] = None, pred: Optional["SysBlob"] = None, maybe_: Optional[Union[Just['GleanTestKitchenSink_maybe_'], Just[None]]] = None, record_: Optional['GleanTestKitchenSink_record_'] = None, sum_: Optional['GleanTestKitchenSink_sum_'] = None, enum_: Optional['GleanTestKitchenSink_enum_'] = None, named_record_: Optional["GleanTestRec"] = None, named_sum_: Optional["GleanTestSum"] = None, named_enum_: Optional["GleanTestEnum"] = None, array_of_byte: Optional[bytes] = None, array_of_nat: Optional[List[int]] = None, array_of_bool: Optional[List[bool]] = None, array_of_string: Optional[List[str]] = None, array_of_pred: Optional[List["GleanTestPredicate"]] = None, array_of_named_record: Optional[List["GleanTestRec"]] = None, array_of_named_sum: Optional[List["GleanTestSum"]] = None, array_of_named_enum: Optional[List["GleanTestEnum"]] = None, array2_of_byte: Optional[List["GleanTestArrayByte"]] = None, array2_of_nat: Optional[List["GleanTestArrayNat"]] = None, array2_of_bool: Optional[List["GleanTestArrayBool"]] = None, array2_of_string: Optional[List["GleanTestArrayString"]] = None) -> "GleanTestKitchenSink":
    raise Exception("this function can only be called from @angle_query")

class GleanTestKitchenSink_record_(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], a: ast.Expr, b: ast.Expr) -> Tuple[str, Struct]:
    return f" {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, a, 'a'), angle_for(__env, b, 'b')])) or '_' } }}", GleanTestKitchenSink_record_

  @staticmethod
  def angle_query(*, a: Optional[bytes] = None, b: Optional[int] = None) -> "GleanTestKitchenSink_record_":
    raise Exception("this function can only be called from @angle_query")



class GleanTestKitchenSink_sum_(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], c: ast.Expr, d: ast.Expr) -> Tuple[str, Struct]:
    return f" {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, c, 'c'), angle_for(__env, d, 'd')])) or '_' } }}", GleanTestKitchenSink_sum_

  @staticmethod
  def angle_query_c(*, c: Optional["GleanTestPredicate"] = None) -> "GleanTestKitchenSink_sum_":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_d(*, d: Optional["SysBlob"] = None) -> "GleanTestKitchenSink_sum_":
    raise Exception("this function can only be called from @angle_query")






class GleanTestEntity(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], cxx: ast.Expr, pp: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Entity.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, cxx, 'cxx'), angle_for(__env, pp, 'pp')])) or '_' } }}", Entity

  @staticmethod
  def angle_query_cxx(*, cxx: Optional["CodeCxxEntity"] = None) -> "GleanTestEntity":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_pp(*, pp: Optional["Pp1Define"] = None) -> "GleanTestEntity":
    raise Exception("this function can only be called from @angle_query")




class GleanTestKitchenSink(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], byt: ast.Expr, nat: ast.Expr, array_of_byte: ast.Expr, array_of_nat: ast.Expr, record_: ast.Expr, sum_: ast.Expr, named_record_: ast.Expr, named_sum_: ast.Expr, named_enum_: ast.Expr, pred: ast.Expr, maybe_: ast.Expr, bool_: ast.Expr, string_: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.KitchenSink.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, byt, 'byt'), angle_for(__env, nat, 'nat'), angle_for(__env, array_of_byte, 'array_of_byte'), angle_for(__env, array_of_nat, 'array_of_nat'), angle_for(__env, record_, 'record_'), angle_for(__env, sum_, 'sum_'), angle_for(__env, named_record_, 'named_record_'), angle_for(__env, named_sum_, 'named_sum_'), angle_for(__env, named_enum_, 'named_enum_'), angle_for(__env, pred, 'pred'), angle_for(__env, maybe_, 'maybe_'), angle_for(__env, bool_, 'bool_'), angle_for(__env, string_, 'string_')])) or '_' } }}", KitchenSink

  @staticmethod
  def angle_query(*, byt: Optional[bytes] = None, nat: Optional[int] = None, array_of_byte: Optional[bytes] = None, array_of_nat: Optional[List[int]] = None, record_: Optional['GleanTestKitchenSink_record_'] = None, sum_: Optional['GleanTestKitchenSink_sum_'] = None, named_record_: Optional["GleanTestRec"] = None, named_sum_: Optional["GleanTestSum"] = None, named_enum_: Optional["GleanTestEnum"] = None, pred: Optional["SysBlob"] = None, maybe_: Optional[Union[Just['GleanTestKitchenSink_maybe_'], Just[None]]] = None, bool_: Optional[bool] = None, string_: Optional[str] = None) -> "GleanTestKitchenSink":
    raise Exception("this function can only be called from @angle_query")

class GleanTestKitchenSink_record_(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], a: ast.Expr, b: ast.Expr) -> Tuple[str, Struct]:
    return f" {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, a, 'a'), angle_for(__env, b, 'b')])) or '_' } }}", GleanTestKitchenSink_record_

  @staticmethod
  def angle_query(*, a: Optional[bytes] = None, b: Optional[int] = None) -> "GleanTestKitchenSink_record_":
    raise Exception("this function can only be called from @angle_query")



class GleanTestKitchenSink_sum_(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], c: ast.Expr, d: ast.Expr) -> Tuple[str, Struct]:
    return f" {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, c, 'c'), angle_for(__env, d, 'd')])) or '_' } }}", GleanTestKitchenSink_sum_

  @staticmethod
  def angle_query_c(*, c: Optional[bytes] = None) -> "GleanTestKitchenSink_sum_":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_d(*, d: Optional[int] = None) -> "GleanTestKitchenSink_sum_":
    raise Exception("this function can only be called from @angle_query")






class GleanTestEnum(Enum):
  red = 0
  green = 1
  blue = 2

class GleanTestWrappedStringPair(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], wrapped: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.WrappedStringPair.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, wrapped, 'wrapped')])) or '_' } }}", WrappedStringPair

  @staticmethod
  def angle_query(*, wrapped: Optional["GleanTestStringPair"] = None) -> "GleanTestWrappedStringPair":
    raise Exception("this function can only be called from @angle_query")



class GleanTestEdgeSum(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], fst: ast.Expr, snd: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.EdgeSum.5 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, fst, 'fst'), angle_for(__env, snd, 'snd')])) or '_' } }}", EdgeSum

  @staticmethod
  def angle_query_fst(*, fst: Optional["GleanTestEdgeWrapper"] = None) -> "GleanTestEdgeSum":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_snd(*, snd: Optional["GleanTestEdgeWrapper"] = None) -> "GleanTestEdgeSum":
    raise Exception("this function can only be called from @angle_query")




class GleanTestRec(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], alpha: ast.Expr, beta: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Rec.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, alpha, 'alpha'), angle_for(__env, beta, 'beta')])) or '_' } }}", Rec

  @staticmethod
  def angle_query(*, alpha: Optional["GleanTestEnum"] = None, beta: Optional["GleanTestSum"] = None) -> "GleanTestRec":
    raise Exception("this function can only be called from @angle_query")



class GleanTestSum(InnerGleanSchemaPredicate):
  @staticmethod
  def build_angle(__env: Dict[str, R], mon: ast.Expr, tue: ast.Expr, wed: ast.Expr) -> Tuple[str, Struct]:
    return f"glean.test.Sum.1 {{ { ', '.join(filter(lambda x: x != '', [angle_for(__env, mon, 'mon'), angle_for(__env, tue, 'tue'), angle_for(__env, wed, 'wed')])) or '_' } }}", Sum

  @staticmethod
  def angle_query_mon(*, mon: Optional[bytes] = None) -> "GleanTestSum":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_tue(*, tue: Optional[int] = None) -> "GleanTestSum":
    raise Exception("this function can only be called from @angle_query")

  @staticmethod
  def angle_query_wed(*, wed: Optional[bool] = None) -> "GleanTestSum":
    raise Exception("this function can only be called from @angle_query")






GleanTestArrayByte = bytes

GleanTestArrayNat = List[int]

GleanTestArrayString = List[str]

GleanTestArrayBool = List[bool]
