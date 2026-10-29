/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <set>
#include <stdexcept>

#include "glean/rts/ownership/acl.h"

namespace facebook::glean::rts {

namespace {

// Helper: create a promoted OR-set of UnitIds in the given Usets container.
Uset* makePromotedOrSet(Usets& usets, const std::set<UsetId>& members) {
  auto* uset = new Uset(SetU32::from(members), Or, 1);
  auto* added = usets.add(uset);
  usets.promote(added);
  return added;
}

// Helper: pair a visibility source unit with its matched ACL rules.
UnitACLAssignment makeAssignment(
    UnitId visibilityUnit,
    std::vector<std::vector<UnitId>> ruleGroups) {
  return UnitACLAssignment{visibilityUnit, std::move(ruleGroups)};
}

// Helper: recursively render a Uset expression tree as a human-readable string.
// Leaf IDs (below firstSetId) are printed as plain numbers.
// Promoted sets expand as "AND(...)" or "OR(...)".
std::string usetToString(const Usets& usets, UsetId id, UsetId firstSetId) {
  if (id < firstSetId) {
    return std::to_string(id);
  }
  auto* uset = usets.lookupById(id);
  if (!uset) {
    // Opaque reference (e.g., group UsetId not in this Usets container).
    return std::to_string(id);
  }
  std::string result = (uset->exp.op == And) ? "AND(" : "OR(";
  bool first = true;
  uset->exp.set.foreach([&](uint32_t member) {
    if (!first) {
      result += ", ";
    }
    first = false;
    result += usetToString(usets, member, firstSetId);
  });
  result += ")";
  return result;
}

} // namespace

TEST(ACLExpressionBuilderTest, RejectsPublicUnitsInExpressionRange) {
  Usets usets(/* firstId */ 100);
  EXPECT_THROW(ACLExpressionBuilder(usets, 100), std::runtime_error);
  EXPECT_THROW(ACLExpressionBuilder(usets, INVALID_USET), std::runtime_error);
  EXPECT_EQ(usets.size(), 0);
}

TEST(ACLExpressionBuilderTest, PublicAndInaccessibleOwnersAreNormalized) {
  Usets usets(/* firstId */ 100);
  ACLExpressionBuilder builder(usets, /* publicUnit */ 0);
  const auto restricted = builder.ownerForPath({{1}});

  EXPECT_EQ(
      builder.ownerForPathExpression(
          Or, {restricted, builder.ownerForPath({})}),
      builder.ownerForPath({}));
  EXPECT_EQ(
      builder.ownerForPathExpression(
          And, {builder.ownerForPath({}), restricted}),
      restricted);
  EXPECT_EQ(
      builder.ownerForPathExpression(And, {restricted, INVALID_USET}),
      INVALID_USET);
  EXPECT_EQ(builder.ownerForPathExpression(Or, {}), INVALID_USET);
  EXPECT_EQ(builder.ownerForPathExpression(And, {}), builder.ownerForPath({}));
}

TEST(ACLExpressionBuilderTest, NestedPathBuildsAConjunctionOfAlternatives) {
  Usets usets(/* firstId */ 100);
  ACLExpressionBuilder builder(usets, /* publicUnit */ 0);

  const auto owner = builder.ownerForPath({{1, 2}, {3}});
  const auto* conjunction = usets.lookupById(owner);
  ASSERT_NE(conjunction, nullptr);
  EXPECT_EQ(conjunction->exp.op, And);

  std::vector<UsetId> levels;
  conjunction->exp.set.foreach(
      [&](UsetId expression) { levels.push_back(expression); });
  ASSERT_EQ(levels.size(), 2);
  for (const auto level : levels) {
    const auto* alternatives = usets.lookupById(level);
    ASSERT_NE(alternatives, nullptr);
    EXPECT_EQ(alternatives->exp.op, Or);
  }
}

TEST(ACLExpressionBuilderTest, EquivalentPathsShareAnOwner) {
  Usets usets(/* firstId */ 100);
  ACLExpressionBuilder builder(usets, /* publicUnit */ 0);

  const auto first = builder.ownerForPath({{2, 1}, {3}});
  const auto second = builder.ownerForPath({{1, 2}, {3}});

  EXPECT_EQ(first, second);
  EXPECT_EQ(builder.ownerForPathExpression(Or, {first, first}), first);
}

TEST(ACLExpressionBuilderTest, PathWithoutMatchingLevelsIsPublic) {
  Usets usets(/* firstId */ 100);
  ACLExpressionBuilder builder(usets, /* publicUnit */ 0);

  const auto owner = builder.ownerForPath({});
  EXPECT_EQ(owner, builder.ownerForPath({{0}}));
  EXPECT_GE(owner, usets.getFirstId());
}

TEST(ACLExpressionBuilderTest, EmptyMatchingLevelIsInaccessible) {
  Usets usets(/* firstId */ 100);
  ACLExpressionBuilder builder(usets, /* publicUnit */ 0);

  EXPECT_EQ(builder.ownerForPath({{1}, {}}), INVALID_USET);
}

// Verify that facts owned by a promoted set with matching UnitIds are
// augmented with the expected AND(owner, aclCnf) structure.
TEST(AclTest, BasicAugmentation) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  // UnitIds 1 and 2 are leaf units.
  auto* ownerSet = makePromotedOrSet(usets, {1, 2});

  // facts_: one interval owned by ownerSet.
  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  augmentOwnershipWithACL(ownership, {makeAssignment(1, {{50}})});

  EXPECT_EQ(
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId),
      "AND(OR(1, 2), OR(50))");
}

// Verify that facts whose owner has no matching ACL units are left unchanged.
TEST(AclTest, NoMatchingACLLeavesOwnerUnchanged) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  auto* ownerSet = makePromotedOrSet(usets, {1, 2});

  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  // ACL assignment is for unit 99, which is NOT in the owner set.
  augmentOwnershipWithACL(ownership, {makeAssignment(99, {{50}})});

  EXPECT_EQ(ownership.facts_[0].second, ownerSet->id);
  EXPECT_EQ(
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId),
      "OR(1, 2)");
}

// Verify that empty assignments produce no changes.
TEST(AclTest, EmptyAssignmentsNoOp) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  auto* ownerSet = makePromotedOrSet(usets, {1});

  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  augmentOwnershipWithACL(ownership, {});

  EXPECT_EQ(ownership.facts_[0].second, ownerSet->id);
  EXPECT_EQ(
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId),
      "OR(1)");
}

// Stacked DB scenario: a fact's ownerUsetId is below the stacked DB's
// firstSetId because it refers to a promoted set from the base DB.
//
// In the stacked DB, firstSetId is higher than any base-DB set ID. So
// base-DB set IDs fall into the (ownerUsetId < firstSetId) branch,
// which treats them as leaf UnitId lookups in unitACLMap. Since
// unitACLMap only contains real leaf UnitIds (not base-DB set IDs),
// the lookup correctly misses and no ACL is applied — the base DB
// already handled ACL augmentation for those sets.
TEST(AclTest, StackedDB_BaseSetIdBelowFirstSetId) {
  // Simulate a stacked DB whose Usets start at ID 1000.
  // The base DB used set IDs in the range [10, 999].
  constexpr UsetId kStackedFirstSetId = 1000;
  Usets stackedUsets(kStackedFirstSetId);

  // Create a promoted set in the stacked layer (ID >= 1000).
  auto* stackedOwner = makePromotedOrSet(stackedUsets, {1, 2});
  ASSERT_GE(stackedOwner->id, kStackedFirstSetId);

  // A base-DB set ID that falls below the stacked firstSetId.
  // This is NOT a real leaf UnitId — it's a promoted set from the base.
  constexpr UsetId kBaseDbSetId = 500;
  ASSERT_LT(kBaseDbSetId, kStackedFirstSetId);

  // facts_: two intervals — one owned by a base-DB set (should be skipped
  // because unitACLMap won't match a set ID), one owned by a stacked set
  // (should be augmented).
  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(50), kBaseDbSetId},
      {Id::fromWord(200), stackedOwner->id},
  };

  ComputedOwnership ownership(std::move(stackedUsets), std::move(facts));

  augmentOwnershipWithACL(ownership, {makeAssignment(1, {{50}})});

  // Fact 0 (base-DB set owner): unchanged. The code enters the
  // (ownerUsetId < firstSetId) branch and does unitACLMap.find(500),
  // which correctly misses — the base DB already applied ACLs.
  EXPECT_EQ(ownership.facts_[0].second, kBaseDbSetId);

  // Fact 1 (stacked owner containing unit 1): should be augmented.
  EXPECT_EQ(
      usetToString(
          ownership.sets_, ownership.facts_[1].second, kStackedFirstSetId),
      "AND(OR(1, 2), OR(50))");
}

// Verify that INVALID_USET owners are skipped entirely.
TEST(AclTest, InvalidUsetSkipped) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), INVALID_USET}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  augmentOwnershipWithACL(ownership, {makeAssignment(1, {{50}})});

  EXPECT_EQ(ownership.facts_[0].second, INVALID_USET);
}

// Verify that multi-level ACL assignments produce the correct CNF structure:
// AND(owner, AND(OR(level1_groups), OR(level2_groups))).
TEST(AclTest, MultiLevelCNF) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  auto* ownerSet = makePromotedOrSet(usets, {1});

  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  // Two ACL levels: level 0 has groups {50, 51}, level 1 has group {60}.
  augmentOwnershipWithACL(ownership, {makeAssignment(1, {{50, 51}, {60}})});

  auto result =
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId);

  // The owner should be ANDed with the CNF of the two levels.
  EXPECT_THAT(result, ::testing::HasSubstr("AND("));
  EXPECT_THAT(result, ::testing::HasSubstr("OR(1)"));
  EXPECT_THAT(result, ::testing::HasSubstr("OR(50, 51)"));
  EXPECT_THAT(result, ::testing::HasSubstr("OR(60)"));
}

// Verify that when an owner set contains multiple units with different ACL
// assignments, all ACL CNFs are collected and ANDed with the owner.
TEST(AclTest, MultipleUnitsWithDifferentACLs) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  auto* ownerSet = makePromotedOrSet(usets, {1, 2});

  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  // Unit 1 has ACL group 50, unit 2 has ACL group 60.
  augmentOwnershipWithACL(
      ownership, {makeAssignment(1, {{50}}), makeAssignment(2, {{60}})});

  auto result =
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId);

  // Both ACL CNFs should appear in the augmented expression.
  EXPECT_THAT(result, ::testing::HasSubstr("AND("));
  EXPECT_THAT(result, ::testing::HasSubstr("OR(1, 2)"));
  EXPECT_THAT(result, ::testing::HasSubstr("OR(50)"));
  EXPECT_THAT(result, ::testing::HasSubstr("OR(60)"));
}

// Verify that the augment cache correctly reuses results when multiple facts
// share the same owner UsetId.
TEST(AclTest, CacheHitForSameOwner) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  auto* ownerSet = makePromotedOrSet(usets, {1});

  // Two facts with the same owner.
  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id},
      {Id::fromWord(200), ownerSet->id},
  };

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  augmentOwnershipWithACL(ownership, {makeAssignment(1, {{50}})});

  // Both facts should have the same augmented UsetId (cache hit).
  EXPECT_EQ(ownership.facts_[0].second, ownership.facts_[1].second);
  EXPECT_EQ(
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId),
      "AND(OR(1), OR(50))");
}

// Verify that a missing promoted set (ownerUsetId >= firstSetId but not in
// Usets) throws a std::runtime_error rather than silently bypassing ACL.
TEST(AclTest, MissingPromotedSetThrows) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  // Use a UsetId >= firstSetId that was never added to the Usets container.
  constexpr UsetId kBogusSetId = 999;

  std::vector<std::pair<Id, UsetId>> facts = {{Id::fromWord(100), kBogusSetId}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  EXPECT_THROW(
      augmentOwnershipWithACL(ownership, {makeAssignment(1, {{50}})}),
      std::runtime_error);
}

// Verify that assignments with empty levels are skipped without affecting
// ownership.
TEST(AclTest, AssignmentWithEmptyLevelsSkipped) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  auto* ownerSet = makePromotedOrSet(usets, {1});

  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  // Assignment for unit 1 with empty levels — should be skipped.
  augmentOwnershipWithACL(ownership, {makeAssignment(1, {})});

  EXPECT_EQ(ownership.facts_[0].second, ownerSet->id);
}

// Verify that empty inner level groups are skipped, and only non-empty levels
// contribute to the CNF.
TEST(AclTest, AssignmentWithEmptyInnerLevelSkipped) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  auto* ownerSet = makePromotedOrSet(usets, {1});

  std::vector<std::pair<Id, UsetId>> facts = {
      {Id::fromWord(100), ownerSet->id}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  // First inner level is empty (skipped), second has group 50.
  augmentOwnershipWithACL(ownership, {makeAssignment(1, {{}, {50}})});

  EXPECT_EQ(
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId),
      "AND(OR(1), OR(50))");
}

// Verify that a raw leaf UnitId (below firstSetId) with a matching ACL
// assignment is correctly augmented.
TEST(AclTest, RawLeafUnitIdWithACLMatch) {
  constexpr UsetId kFirstSetId = 10;
  Usets usets(kFirstSetId);

  // Fact owned by raw leaf UnitId 1 (below firstSetId).
  std::vector<std::pair<Id, UsetId>> facts = {{Id::fromWord(100), 1}};

  ComputedOwnership ownership(std::move(usets), std::move(facts));

  augmentOwnershipWithACL(ownership, {makeAssignment(1, {{50}})});

  // The raw leaf should be ANDed with the ACL CNF.
  EXPECT_EQ(
      usetToString(ownership.sets_, ownership.facts_[0].second, kFirstSetId),
      "AND(1, OR(50))");
}

} // namespace facebook::glean::rts
