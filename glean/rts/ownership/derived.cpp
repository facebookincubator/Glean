/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <folly/container/F14Map.h>
#include <numeric>

#include "glean/rts/lookup.h"
#include "glean/rts/ownership/derived.h"
#include "glean/rts/timer.h"

namespace facebook {
namespace glean {
namespace rts {

void DefineOwnership::derivedFrom(
    Pid pid,
    Id id,
    const std::set<UsetId>& deps) {
  if (deps.size() == 0) {
    return;
  }

  const auto [it, _] = defines_.insert({pid, PerPredicate()});
  PerPredicate& pred = it->second;

  SetU32 set = SetU32::from(deps);
  size_t size = set.size();

  UsetId usetid;
  if (size == 1) {
    usetid = *deps.begin();
  } else {
    auto uset = std::make_unique<Uset>(std::move(set), And, 0);
    // If this set already exists in the DB, it will be de-duplicated
    // when we write these sets in addDefineOwnership().
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
  }

  if (id >= first_id_) {
    pred.new_ids_.push_back(id);
    pred.new_owners_.push_back(usetid);
  } else {
    pred.ids_.push_back(id);
    pred.owners_.push_back(usetid);
  }
}

void DefineOwnership::sortByOwner(uint64_t facts, std::vector<int64_t>& order) {
  // We have a set of facts in the batch with Ids [first_id, first_id+1, ..]
  // We want to group these facts by owner, so that facts with the same
  // owner are adjacent.
  //
  // We're going to rearrange the facts and give them new IDs.
  //
  // 0. we start with
  //       ids =    [ 0, 1, 2, 3, 0 ]
  //       owners = [ C, B, A, B, A ]
  //  Note that we may have recorded multiple owners for some facts.
  //
  // 1. make a vector of owners, indexed by fact ID:
  //          [ X, B, A, B ]
  // for a fact that has multiple owners, we'll make up a temporary
  // new set Id. These are only for sorting purposes, they're not real
  // set Ids - the real ones are created later in
  // computeDerivedOwnership().
  //
  // e.g. we need a new set ID (let's use X) to represent the owner of
  // fact 0, because it has multiple owners assigned (C and A).

  std::vector<UsetId> owners(facts, INVALID_USET);

  auto tmpset = usets_.getNextId();
  for (auto& [_, pred] : defines_) {
    for (size_t i = 0; i < pred.new_ids_.size(); i++) {
      auto ix = pred.new_ids_[i] - first_id_;
      if (owners[ix] == INVALID_USET) {
        owners[ix] = pred.new_owners_[i];
      } else {
        // this fact has multiple owners, use a new temporary UsetId.
        owners[ix] = tmpset++;
      }
    }
  }

  // 2. order is some permutation of [0..facts-1]. sort by owner = [ 2, 1, 3, 0
  // ]
  //
  // This is the order in which we will serialize the facts, which
  // essentially renumbers them. Hence the facts will be renumbered
  //    2 -> 0
  //    1 -> 1
  //    3 -> 2
  //    0 -> 3

  sort(order.begin(), order.end(), [&](uint64_t a, uint64_t b) {
    return owners[a - first_id_.toWord()] < owners[b - first_id_.toWord()];
  });

  // 3. produce a mapping from old fact Ids to new fact Ids

  std::vector<Id> idmap(facts, Id::invalid());
  for (size_t i = 0; i < order.size(); i++) {
    idmap[order[i] - first_id_.toWord()] = Id::fromWord(i + first_id_.toWord());
  }

  // 4. substitute fact IDs in new_ids

  for (auto& [_, pred] : defines_) {
    for (size_t i = 0; i < pred.new_ids_.size(); i++) {
      pred.new_ids_[i] = idmap[pred.new_ids_[i] - first_id_];
    }
  }

  // At this point we must serialize the batch using
  // serializeReorder(order) so that the facts agree with the IDs used
  // in this DefineOwnership.
}

void DefineOwnership::subst(const Substitution& subst) {
  for (auto& [_, pred] : defines_) {
    for (auto& id : pred.ids_) {
      id = subst.subst(id);
    }
    for (auto& id : pred.new_ids_) {
      id = subst.subst(id);
    }
  }
}

std::unique_ptr<ComputedOwnership> computeDerivedOwnership(
    Ownership& ownership,
    Lookup* base, // Lookup for the base DB, if there is one
    DerivedFactOwnershipIterator* iter) {
  auto t = makeAutoTimer("computeDerivedOwnership");
  VLOG(1) << "computing derived ownership";

  // Here we are identifying facts that were derived multiple
  // different ways.  e.g. if a fact was derived twice with owners A
  // and B, then its final ownership set will be A || B. That is, the
  // fact will be visibile (derivable) if either A or B are visible.

  std::map<uint64_t, UsetId> factOwners;
  folly::F14FastMap<uint64_t, std::set<UsetId>> factOwnerSets;

  auto min_id = base ? base->firstFreeId() : Id::lowest();

  while (const auto owners = iter->get()) {
    for (uint32_t i = 0; i < owners->ids.size(); i++) {
      auto id = owners->ids[i].toWord();
      auto owner = owners->owners[i];
      bool is_base_fact = base && id < min_id.toWord();
      const auto [it, inserted] = factOwners.insert({id, owner});
      if (!inserted || is_base_fact) {
        // The second time we see a fact, create an entry in factOwnerSets
        const auto [it2, _] = factOwnerSets.insert({id, {it->second}});
        it2->second.insert(owner);
        if (inserted && is_base_fact) {
          // If this is a fact in the base DB, we need to OR the new owners with
          // the existing owner, creating a new set. This owner set will
          // then override the ownership of the fact in the base DB.
          auto baseOwner = base->getOwner(Id::fromWord(id));
          VLOG(2) << "baseOwner: " << baseOwner;
          if (baseOwner != INVALID_USET) {
            it2->second.insert(baseOwner);
          }
        }
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
        VLOG(2) << "new set: " << usetid;
      } else {
        VLOG(2) << "existing set: " << usetid;
      }
      factOwners[pair.first] = usetid;
    }
  }

  // convert factOwners into a vector of intervals. factOwners may be sparse,
  // because we may have derived facts that already existed in a base DB, so we
  // have to be careful to fill in gaps in the interval map with INVALID_USET
  std::vector<std::pair<Id, UsetId>> intervals;
  intervals.reserve(factOwners.size());
  UsetId current = INVALID_USET;
  Id prev = Id::lowest() - 1;
  for (auto& pair : factOwners) {
    auto id = Id::fromWord(pair.first);
    auto usetid = pair.second;
    if (id != prev + 1 || usetid != current) {
      if (id != prev + 1) {
        intervals.emplace_back(prev + 1, INVALID_USET);
      }
      intervals.emplace_back(id, usetid);
      current = usetid;
    }
    prev = id;
  }
  if (!intervals.empty()) {
    intervals.emplace_back(prev + 1, INVALID_USET);
  }

  VLOG(1) << "computing derived ownership: " << intervals.size()
          << " intervals";

  // Now build a ComputedOwnership that we can return
  return std::make_unique<ComputedOwnership>(
      std::move(usets), std::move(intervals));
}

void addDerived(
    Lookup* lookup,
    DefineOwnership* define,
    Pid pid,
    DerivedDependencyIterator* it) {
  while (auto v = it->get()) {
    auto& facts = v->first;
    auto& deps = v->second;
    std::set<UsetId> owners;
    for (auto dep : deps) {
      auto owner = lookup->getOwner(dep);
      if (owner == INVALID_USET) {
        VLOG(1) << "fact " << dep.toWord() << " has no owner";
      } else {
        owners.insert(owner);
      }
    }

    if (owners.size() > 0) {
      for (auto id : facts) {
        define->derivedFrom(pid, id, owners);
      }
    }
  }
}

} // namespace rts
} // namespace glean
} // namespace facebook
