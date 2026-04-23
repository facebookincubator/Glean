/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/ownership/acl.h"
#include "glean/rts/timer.h"

#include <folly/CppAttributes.h>
#include <folly/container/F14Map.h>
#include <set>

namespace facebook {
namespace glean {
namespace rts {

namespace {

// Create an OR-set from a list of UsetIds and add it to the Usets container.
Uset* FOLLY_NULLABLE makeOrSet(Usets& usets, const std::vector<UsetId>& ids) {
  CHECK(!ids.empty());
  if (ids.size() == 1) {
    return usets.add(std::make_unique<Uset>(SetU32::from({ids[0]}), Or, 1));
  }
  std::set<UsetId> idSet(ids.begin(), ids.end());
  auto entry = std::make_unique<Uset>(SetU32::from(idSet), Or, 1);
  return usets.add(std::move(entry));
}

// Create an AND-set from a list of UsetIds and add it to the Usets container.
Uset* FOLLY_NULLABLE makeAndSet(Usets& usets, const std::vector<UsetId>& ids) {
  CHECK(!ids.empty());
  if (ids.size() == 1) {
    return usets.lookupById(ids[0]);
  }
  std::set<UsetId> idSet(ids.begin(), ids.end());
  auto entry = std::make_unique<Uset>(SetU32::from(idSet), And, 1);
  return usets.add(std::move(entry));
}

// Build UnitId → ACL CNF Uset mapping from assignments.
// Each assignment's levels are converted to OR-sets and combined into a
// CNF AND-set. All resulting Usets are promoted so they have IDs.
folly::F14FastMap<UnitId, Uset*> buildUnitACLMap(
    Usets& usets,
    const std::vector<UnitACLAssignment>& assignments) {
  folly::F14FastMap<UnitId, Uset*> unitACLMap;
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
      auto* orSet = CHECK_NOTNULL(makeOrSet(usets, levelGroups));
      usets.promote(orSet);
      orSetIds.push_back(orSet->id);
    }

    if (orSetIds.empty()) {
      continue;
    }

    Uset* cnf = makeAndSet(usets, orSetIds);
    unitACLMap[assignment.unitId] = cnf;
  }

  for (auto& [unitId, cnf] : unitACLMap) {
    usets.promote(cnf);
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
        aclCnfIds.insert(it->second->id);
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
            aclCnfIds.insert(it->second->id);
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
    auto* andSet = CHECK_NOTNULL(makeAndSet(usets, andMembers));
    usets.promote(andSet);

    augmentCache[originalOwner] = andSet->id;
    ownerUsetId = andSet->id;
    ++augmented;
  }

  VLOG(1) << "augmentOwnershipWithACL: augmented " << augmented
          << " fact intervals out of " << facts.size();
  t.log("augmentOwnershipWithACL");
}

} // namespace rts
} // namespace glean
} // namespace facebook
