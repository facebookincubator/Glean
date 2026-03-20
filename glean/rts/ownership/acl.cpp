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

UsetId makeACLCnf(
    ComputedOwnership& ownership,
    const std::vector<std::vector<UsetId>>& levels) {
  auto& usets = ownership.sets_;

  std::vector<Uset*> orSets;
  orSets.reserve(levels.size());
  for (const auto& levelGroups : levels) {
    if (levelGroups.empty()) {
      continue;
    }
    std::set<uint32_t> idSet(levelGroups.begin(), levelGroups.end());
    auto entry = std::make_unique<Uset>(SetU32::from(idSet), Or, 1);
    orSets.push_back(usets.add(std::move(entry)));
  }

  CHECK(!orSets.empty());
  Uset* cnf;
  if (orSets.size() == 1) {
    cnf = orSets[0];
  } else {
    std::set<uint32_t> ids;
    for (auto* orSet : orSets) {
      usets.promote(orSet);
      ids.insert(orSet->id);
    }
    auto entry = std::make_unique<Uset>(SetU32::from(ids), And, 1);
    cnf = usets.add(std::move(entry));
  }

  usets.promote(cnf);
  return cnf->id;
}

void augmentOwnershipWithACL(
    ComputedOwnership& ownership,
    const std::vector<std::pair<UnitId, UsetId>>& assignments) {
  if (assignments.empty()) {
    return;
  }

  auto t = makeAutoTimer("augmentOwnershipWithACL");
  auto& usets = ownership.sets_;
  auto& facts = ownership.facts_;
  const auto firstSetId = usets.getFirstId();

  // Build UnitId → CNF UsetId mapping.
  folly::F14FastMap<UnitId, UsetId> unitACLMap;
  unitACLMap.reserve(assignments.size());
  for (const auto& [unitId, cnfId] : assignments) {
    unitACLMap[unitId] = cnfId;
  }

  VLOG(1) << "augmentOwnershipWithACL: " << unitACLMap.size()
          << " units with ACL assignments";

  // Walk facts_ and augment ownership.
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
      auto it = unitACLMap.find(ownerUsetId);
      if (it != unitACLMap.end()) {
        aclCnfIds.insert(it->second);
      }
    } else {
      // Promoted set — resolve and examine leaf members.
      // After computeOwnership, sets are flat OR-sets of UnitIds
      // (no nested set references), so checking immediate members
      // is sufficient.
      auto* ownerUset = usets.lookupById(ownerUsetId);
      if (ownerUset) {
        ownerUset->exp.set.foreach([&](uint32_t member) {
          if (member < firstSetId) {
            auto it = unitACLMap.find(member);
            if (it != unitACLMap.end()) {
              aclCnfIds.insert(it->second);
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
