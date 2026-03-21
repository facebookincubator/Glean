/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/ownership/acl.h"
#include "glean/rts/timer.h"

#include <folly/container/F14Map.h>
#include <set>

namespace facebook {
namespace glean {
namespace rts {

namespace {

// Create an OR-set from a list of UsetIds and add it to the Usets container.
Uset* makeOrSet(Usets& usets, const std::vector<UsetId>& ids) {
  CHECK(!ids.empty());
  std::set<uint32_t> idSet(ids.begin(), ids.end());
  auto entry = std::make_unique<Uset>(SetU32::from(idSet), Or, 1);
  return usets.add(std::move(entry));
}

// Create an AND-set from a list of Uset pointers.
// Each Uset represents one OR-level in the CNF.
Uset* makeAndSet(Usets& usets, const std::vector<Uset*>& orSets) {
  CHECK(!orSets.empty());
  if (orSets.size() == 1) {
    return orSets[0];
  }
  std::set<uint32_t> ids;
  for (auto* orSet : orSets) {
    usets.promote(orSet);
    ids.insert(orSet->id);
  }
  auto entry = std::make_unique<Uset>(SetU32::from(ids), And, 1);
  return usets.add(std::move(entry));
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

  // Step 1: Build UnitId → ACL CNF Uset mapping
  folly::F14FastMap<UnitId, Uset*> unitACLMap;
  unitACLMap.reserve(assignments.size());

  for (const auto& assignment : assignments) {
    if (assignment.levels.empty()) {
      continue;
    }

    std::vector<Uset*> orSets;
    orSets.reserve(assignment.levels.size());
    for (const auto& levelGroups : assignment.levels) {
      if (levelGroups.empty()) {
        continue;
      }
      auto* orSet = makeOrSet(usets, levelGroups);
      orSets.push_back(orSet);
    }

    if (orSets.empty()) {
      continue;
    }

    Uset* cnf = makeAndSet(usets, orSets);
    unitACLMap[assignment.unitId] = cnf;
  }

  if (unitACLMap.empty()) {
    VLOG(1) << "augmentOwnershipWithACL: no unit ACL assignments, skipping";
    return;
  }

  VLOG(1) << "augmentOwnershipWithACL: " << unitACLMap.size()
          << " units with ACL assignments";

  // Promote all ACL CNF Usets so they have IDs.
  for (auto& [unitId, cnf] : unitACLMap) {
    usets.promote(cnf);
  }

  // Step 2: Build UsetId → Uset* index for resolving promoted sets.
  //
  // After computeOwnership, ALL fact owners are promoted Usets with
  // IDs >= firstSetId — even single-unit facts get wrapped in an OR-set
  // and promoted. The old code checked (ownerUsetId < firstSetId) which
  // was NEVER true, so no facts were ever augmented. We must resolve
  // promoted sets to their leaf UnitIds to find ACL matches.
  folly::F14FastMap<UsetId, Uset*> idToUset;
  usets.foreach([&](Uset* entry) {
    if (entry->promoted()) {
      idToUset[entry->id] = entry;
    }
  });

  // Step 3: Walk facts_ and augment ownership.
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
    std::set<uint32_t> aclCnfIds;

    if (ownerUsetId < firstSetId) {
      // Direct UnitId (rare after computeOwnership but handle it)
      auto it = unitACLMap.find(ownerUsetId);
      if (it != unitACLMap.end()) {
        aclCnfIds.insert(it->second->id);
      }
    } else {
      // Promoted set — resolve and examine leaf members.
      // After computeOwnership, sets are flat OR-sets of UnitIds
      // (no nested set references), so checking immediate members
      // is sufficient.
      auto usetIt = idToUset.find(ownerUsetId);
      if (usetIt != idToUset.end()) {
        usetIt->second->exp.set.foreach([&](uint32_t member) {
          if (member < firstSetId) {
            auto it = unitACLMap.find(member);
            if (it != unitACLMap.end()) {
              aclCnfIds.insert(it->second->id);
            }
          }
        });
      }
    }

    if (aclCnfIds.empty()) {
      augmentCache[originalOwner] = originalOwner;
      continue;
    }

    // Create AND(existingOwner, aclCnf1, aclCnf2, ...)
    std::set<uint32_t> andMembers = {ownerUsetId};
    andMembers.insert(aclCnfIds.begin(), aclCnfIds.end());
    auto andEntry = std::make_unique<Uset>(SetU32::from(andMembers), And, 1);
    auto* andSet = usets.add(std::move(andEntry));
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
