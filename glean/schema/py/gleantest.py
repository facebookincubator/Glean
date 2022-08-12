# @generated
# To regenerate this file run fbcode//glean/schema/gen/sync
from typing import Union
import json
from glean.schema.py.glean_schema_predicate import GleanSchemaPredicate


class GSGleanTestBar(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Bar.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestBar":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestRevStringPairs(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.RevStringPairs.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestRevStringPairs":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestStoredRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.StoredRevStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestStoredRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Ref.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestRef":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.KeyValue.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestIsThree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.IsThree.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestIsThree":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestEdgeWrapper(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.EdgeWrapper.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestEdgeWrapper":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestRefRef(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.RefRef.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestRefRef":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestNothingTest(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.nothingTest.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestNothingTest":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestFooToFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.FooToFoo.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestFooToFoo":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestExpr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Expr.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestExpr":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestRevRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.RevRevStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestRevRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Edge.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestEdge":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestSameString(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.SameString.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestSameString":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestStoredRevStringPairWithA(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.StoredRevStringPairWithA.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestStoredRevStringPairWithA":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestLeftOr(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.LeftOr.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestLeftOr":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestRevStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.RevStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestRevStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestDerivedKeyValue(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.DerivedKeyValue.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestDerivedKeyValue":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestViaStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.ViaStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestViaStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestQux(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Qux.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestQux":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestStoredDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.StoredDualStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestStoredDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestIsGlean(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.IsGlean.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestIsGlean":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestDerivedKeyValue2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.DerivedKeyValue2.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestDerivedKeyValue2":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestRevStringPairRec(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.RevStringPairRec.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestRevStringPairRec":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Tree.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestTree":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Predicate.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestLeftOr2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.LeftOr2.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestLeftOr2":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestNode(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Node.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestNode":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestTreeToTree(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.TreeToTree.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestTreeToTree":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.StringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestName(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Name.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestName":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestSkipRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.SkipRevEdge.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestSkipRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestStoredRevStringPairWithRev(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.StoredRevStringPairWithRev.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestStoredRevStringPairWithRev":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestMatchOneAlt(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.MatchOneAlt.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestMatchOneAlt":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestRevEdge(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.RevEdge.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestRevEdge":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestStoredRevStringPairSum(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.StoredRevStringPairSum.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestStoredRevStringPairSum":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestEmptyStoredStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.EmptyStoredStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestEmptyStoredStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestUnbound(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Unbound.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestUnbound":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestPredicate(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Predicate.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestPredicate":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestDualStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.DualStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestDualStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestUnbound2(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Unbound2.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestUnbound2":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestStringPairBox(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.StringPairBox.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestStringPairBox":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestReflStringPair(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.ReflStringPair.1 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestReflStringPair":
    raise Exception("this function can only be called from @angle_query")

class GSGleanTestFoo(GleanSchemaPredicate):
  @staticmethod
  def build_angle(key: Union[str, int]) -> str:
    return f"glean.test.Foo.5 { json.dumps(key) }"

  @staticmethod
  def angle_query(*, name: str) -> "GSGleanTestFoo":
    raise Exception("this function can only be called from @angle_query")


