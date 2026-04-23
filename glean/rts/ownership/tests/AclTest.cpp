/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>
#include <set>

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

// Helper: create a UnitACLAssignment concisely.
// levels is outer=AND, inner=OR of group UsetIds.
UnitACLAssignment makeAssignment(
    UnitId unitId,
    std::vector<std::vector<UsetId>> levels) {
  return UnitACLAssignment{unitId, std::move(levels)};
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

} // namespace facebook::glean::rts
