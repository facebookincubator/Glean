/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/ownership/acl.h"
#include "glean/rts/ownership/expression.h"
#include "glean/rts/timer.h"

#include <folly/container/F14Map.h>
#include <set>
#include <stdexcept>

namespace facebook {
namespace glean {
namespace rts {

ACLExpressionBuilder::ACLExpressionBuilder(Usets& usets, UnitId publicUnit)
    : expressionBuilder_(usets), publicUnit_(publicUnit) {
  if (publicUnit_ >= usets.getFirstId()) {
    throw std::runtime_error("public unit must precede ownership expressions");
  }
  publicOwner_ = expressionBuilder_.anyOf({publicUnit_});
}

UsetId ACLExpressionBuilder::ownerForPathExpression(
    SetOp op,
    const std::vector<UsetId>& owners) {
  return combine(op, owners);
}

UsetId ACLExpressionBuilder::ownerForPath(
    const std::vector<std::vector<UnitId>>& ruleGroups) {
  if (ruleGroups.empty()) {
    return publicOwner_;
  }

  std::vector<UsetId> ruleOwners;
  ruleOwners.reserve(ruleGroups.size());
  for (const auto& alternatives : ruleGroups) {
    ruleOwners.push_back(combine(Or, alternatives));
  }
  return combine(And, ruleOwners);
}

UsetId ACLExpressionBuilder::combine(
    SetOp op,
    const std::vector<UsetId>& owners) {
  std::vector<UsetId> normalized;
  normalized.reserve(owners.size());

  for (const auto owner : owners) {
    const auto isPublic = owner == publicUnit_ || owner == publicOwner_;
    if (op == Or) {
      if (isPublic) {
        return publicOwner_;
      }
      if (owner == INVALID_USET) {
        continue;
      }
    } else {
      if (owner == INVALID_USET) {
        return INVALID_USET;
      }
      if (isPublic) {
        continue;
      }
    }
    normalized.push_back(owner);
  }

  if (normalized.empty()) {
    return op == Or ? INVALID_USET : publicOwner_;
  }
  return op == Or ? expressionBuilder_.anyOf(normalized)
                  : expressionBuilder_.allOf(normalized);
}

namespace {

// Build UnitId → ACL CNF Uset mapping from assignments.
// Each assignment's levels are converted to OR-sets and combined into a
// CNF AND-set. All resulting Usets are promoted so they have IDs.
folly::F14FastMap<UnitId, UsetId> buildUnitACLMap(
    Usets& usets,
    const std::vector<UnitACLAssignment>& assignments) {
  OwnershipExpressionBuilder expressions(usets);
  folly::F14FastMap<UnitId, UsetId> unitACLMap;
  unitACLMap.reserve(assignments.size());

  for (const auto& assignment : assignments) {
    if (assignment.levels.empty()) {
      continue;
    }

    std::vector<UsetId> orSetIds;
    orSetIds.reserve(assignment.levels.size());
    for (const auto& levelGroups : assignment.levels) {
      if (levelGroups.empty()) {
        continue;
      }
      orSetIds.push_back(expressions.anyOf(levelGroups));
    }

    if (orSetIds.empty()) {
      continue;
    }

    unitACLMap[assignment.unitId] = expressions.allOf(orSetIds);
  }

  return unitACLMap;
}

} // namespace

void augmentOwnershipWithACL(
    ComputedOwnership& ownership,
    const std::vector<UnitACLAssignment>& assignments) {
  if (assignments.empty()) {
    return;
  }

  auto t = makeAutoTimer("augmentOwnershipWithACL");
  auto& usets = ownership.sets_;
  auto& facts = ownership.facts_;
  const auto firstSetId = usets.getFirstId();
  OwnershipExpressionBuilder expressions(usets);

  auto unitACLMap = buildUnitACLMap(usets, assignments);

  if (unitACLMap.empty()) {
    VLOG(1) << "augmentOwnershipWithACL: no unit ACL assignments, skipping";
    return;
  }

  VLOG(1) << "augmentOwnershipWithACL: " << unitACLMap.size()
          << " units with ACL assignments";

  // facts_ is a vector of (Id, UsetId) interval boundaries.
  size_t augmented = 0;

  // Cache: memoize owner UsetId → augmented UsetId to avoid duplicate work.
  folly::F14FastMap<UsetId, UsetId> augmentCache;

  for (auto& [factId, ownerUsetId] : facts) {
    if (ownerUsetId == INVALID_USET) {
      continue;
    }

    auto cacheIt = augmentCache.find(ownerUsetId);
    if (cacheIt != augmentCache.end()) {
      if (cacheIt->second != ownerUsetId) {
        ownerUsetId = cacheIt->second;
        ++augmented;
      }
      continue;
    }

    UsetId originalOwner = ownerUsetId;

    // Collect unique ACL CNF IDs for leaf UnitIds in this owner's set.
    std::set<UsetId> aclCnfIds;

    if (ownerUsetId < firstSetId) {
      // ownerUsetId < firstSetId means one of two things:
      //
      // 1. A raw leaf UnitId — rare for a non-stacked DB since
      //    computeOwnership promotes all owners to OR-sets (even
      //    single-unit owners become length-1 OR-sets), but handled
      //    defensively.
      //
      // 2. A promoted set from a base DB (stacked DB case) — the base
      //    DB's UsetIds are below this ComputedOwnership's firstSetId.
      //    This is safe to treat as a UnitId lookup because unitACLMap
      //    only contains real leaf UnitIds, so a base-DB set ID will
      //    simply not match, and the base DB already applied its own
      //    ACL augmentation.
      auto it = unitACLMap.find(ownerUsetId);
      if (it != unitACLMap.end()) {
        aclCnfIds.insert(it->second);
      } else {
        VLOG(1) << "augmentOwnershipWithACL: ownerUsetId " << ownerUsetId
                << " < firstSetId " << firstSetId
                << " has no ACL assignment — this fact will not have"
                   " ACL constraints applied and will remain accessible"
                   " based on original ownership alone (expected for"
                   " units without ACL config or base-DB sets in a"
                   " stacked DB where ACLs were already applied)";
      }
    } else {
      // Promoted set — resolve and examine leaf members.
      // After computeOwnership, sets are flat OR-sets of UnitIds
      // (no nested set references), so checking immediate members
      // is sufficient.
      auto* ownerUset = usets.lookupById(ownerUsetId);
      if (!ownerUset) {
        // ownerUsetId >= firstSetId means it should be a promoted set in
        // this ComputedOwnership's Usets container. Base-DB set IDs are
        // < firstSetId (stacked IDs are always > base IDs) and would
        // enter the if-branch above. A missing set here means we cannot
        // determine which UnitIds this fact belongs to, so we cannot
        // apply the correct ACL constraints — the fact would silently
        // bypass ACL enforcement. Throw rather than continue with
        // incorrect access control.
        throw std::runtime_error(
            "augmentOwnershipWithACL: UsetId " + std::to_string(ownerUsetId) +
            " >= firstSetId " + std::to_string(firstSetId) +
            " but not found in this ComputedOwnership's Usets."
            " Cannot determine ACL constraints for this fact;"
            " continuing would risk incorrect access control.");
      }
      ownerUset->exp.set.foreach([&](UsetId member) {
        if (member < firstSetId) {
          auto it = unitACLMap.find(member);
          if (it != unitACLMap.end()) {
            aclCnfIds.insert(it->second);
          }
        }
      });
    }

    if (aclCnfIds.empty()) {
      augmentCache[originalOwner] = originalOwner;
      continue;
    }

    // Create AND(existingOwner, aclCnf1, aclCnf2, ...)
    std::vector<UsetId> andMembers = {ownerUsetId};
    andMembers.insert(andMembers.end(), aclCnfIds.begin(), aclCnfIds.end());
    const auto augmentedOwner = expressions.allOf(andMembers);

    augmentCache[originalOwner] = augmentedOwner;
    ownerUsetId = augmentedOwner;
    ++augmented;
  }

  VLOG(1) << "augmentOwnershipWithACL: augmented " << augmented
          << " fact intervals out of " << facts.size();
  t.log("augmentOwnershipWithACL");
}

} // namespace rts
} // namespace glean
} // namespace facebook
