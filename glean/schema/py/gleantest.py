# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Tuple, Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GleanTestBar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Bar.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestBar":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.RevStringPairs.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.StoredRevStringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Ref.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.KeyValue.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.IsThree.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: int) -> "GleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.EdgeWrapper.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.RefRef.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.nothingTest.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.FooToFoo.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")

class GleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Expr.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.RevRevStringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Edge.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.SameString.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestSameString":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.StoredRevStringPairWithA.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.LeftOr.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.RevStringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.DerivedKeyValue.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.ViaStringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Qux.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestQux":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.StoredDualStringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.IsGlean.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.DerivedKeyValue2.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.RevStringPairRec.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Tree.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Predicate.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.LeftOr2.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Node.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestNode":
    raise Exception("this function can only be called from @angle_query")

class GleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.TreeToTree.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.StringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Name.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestName":
    raise Exception("this function can only be called from @angle_query")

class GleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.SkipRevEdge.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.StoredRevStringPairWithRev.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")

class GleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.MatchOneAlt.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")

class GleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.RevEdge.5 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.StoredRevStringPairSum.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")

class GleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.EmptyStoredStringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Unbound.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")

class GleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Predicate.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.DualStringPair.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Unbound2.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")

class GleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.StringPairBox.1 { { } }"

  @staticmethod
  def angle_query(*, name: Tuple[()]) -> "GleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")

class GleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.ReflStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")

class GleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[int, bool, str, Tuple[()]]) -> str:
    return f"glean.test.Foo.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GleanTestFoo":
    raise Exception("this function can only be called from @angle_query")


