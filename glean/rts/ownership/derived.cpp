// Copyright (c) Facebook, Inc. and its affiliates.

#include <folly/container/F14Map.h>
#include <folly/experimental/AutoTimer.h>

#include "glean/rts/ownership/derived.h"

namespace facebook {
namespace glean {
namespace rts {

void DefineOwnership::derivedFrom(Id id, const std::set<UsetId>& deps) {
  if (deps.size() == 0) {
    LOG(ERROR) << "DefineOwnership::derivedFrom: empty deps";
    return;
  }

  SetU32 set = SetU32::from(deps);

  UsetId usetid;
  if (set.size() == 1) {
    usetid = *deps.begin();
  } else {
    auto uset = std::make_unique<Uset>(set, And, 0);
    size_t size = set.size();
    usetid = ownership_->lookupSet(uset.get());
    if (usetid == INVALID_USET) {
      auto q = uset.get();
      auto p = usets_.add(std::move(uset));
      if (p == q) {
        usets_.promote(p);
        usetid = p->id;
        newSets_.push_back(p);
        VLOG(2) << "new set: " << usetid << ", size = " << size;
      } else {
        usetid = p->id;
        VLOG(2) << "existing set in batch: " << usetid;
      }
    } else {
      VLOG(2) << "existing set in DB: " << usetid;
    }
  }
  ids_.push_back(id);
  owners_.push_back(usetid);
}

void DefineOwnership::subst(const Substitution& subst) {
  for (auto& id : ids_) {
    id = subst.subst(id);
  }
}

std::unique_ptr<ComputedOwnership> computeDerivedOwnership(
  Ownership& ownership,
  DerivedFactOwnershipIterator *iter) {
  folly::AutoTimer t("computeDerivedOwnership");
  LOG(INFO) << "computing derived ownership";

  // Here we are identifying facts that were derived multiple
  // different ways.  e.g. if a fact was derived twice with owners A
  // and B, then its final ownership set will be A || B. That is, the
  // fact will be visibile (derivable) if either A or B are visible.

  std::map<uint64_t,UsetId> factOwners;
  folly::F14FastMap<int64_t,std::set<UsetId>> factOwnerSets;

  while (const auto owners = iter->get()) {
    for (uint32_t i = 0; i < owners->ids.size(); i++) {
      auto id = owners->ids[i].toWord();
      auto owner = owners->owners[i];
      const auto [it, inserted] = factOwners.insert({id, owner});
      if (inserted) {
        const auto [it2, _] = factOwnerSets.insert({id, {it->second}});
        it2->second.insert(owner);
      }
    }
  }

  // Create all the new sets
  //   we are under the write lock here, so we can create canonical
  //   sets, no need to rebase them later.

  Usets usets(ownership.nextSetId());

  for (auto& pair : factOwnerSets) {
    if (pair.second.size() > 1) {
      SetU32 set = SetU32::from(pair.second);
      auto uset = std::make_unique<Uset>(std::move(set), Or, 0);
      auto usetid = ownership.lookupSet(uset.get());
      if (usetid == INVALID_USET) {
        auto p = usets.add(std::move(uset));
        usets.promote(p);
        usetid = p->id;
        VLOG(1) << "new set: " << usetid;
      } else {
        VLOG(1) << "existing set: " << usetid;
      }
      factOwners[pair.first] = usetid;
    }
  }

  auto sets = usets.toEliasFano();

  // convert factOwners into a vector of intervals
  std::vector<std::pair<Id,UsetId>> intervals;
  intervals.reserve(factOwners.size());
  UsetId current = INVALID_USET;
  for (auto& pair : factOwners) {
    auto usetid = pair.second;
    if (usetid != current) {
      intervals.push_back(std::make_pair(Id::fromWord(pair.first), usetid));
      current = usetid;
    }
  }

  // Now build a ComputedOwnership that we can return
  return std::make_unique<ComputedOwnership>(
      usets.getFirstId(),
      std::move(sets),
      std::move(intervals));

}

}
}
}
