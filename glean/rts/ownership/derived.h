/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <map>

#include "glean/rts/ownership.h"
#include "glean/rts/substitution.h"

namespace facebook {
namespace glean {
namespace rts {

// Ownership data for all derived predicates in a batch.
struct DefineOwnership {
  DefineOwnership(Ownership* ownership, Id first_id)
      : first_id_(first_id),
        ownership_(ownership),
        defines_(),
        usets_(ownership->nextSetId()) {}

  // record that a fact was derived from some other facts
  void derivedFrom(Pid pid, Id id, const std::set<UsetId>& deps);

  // Apply a substitution to the Ids
  void subst(const Substitution& subst);

  void sortByOwner(uint64_t facts, std::vector<int64_t>& order);

  struct PerPredicate {
    // new owners for exsiting facts (< first_id)
    std::vector<Id> ids_;
    std::vector<UsetId> owners_;

    // owners for new facts in this batch
    std::vector<Id> new_ids_;
    std::vector<UsetId> new_owners_;
  };

  using const_iterator = std::map<Pid, PerPredicate>::const_iterator;
  const_iterator begin() {
    return defines_.begin();
  }
  const_iterator end() {
    return defines_.end();
  }

  /// The sets that derivedFrom() promoted for this batch, numbered from the
  /// ownership's next set ID. addDefineOwnership() stores every one of them.
  const Usets& sets() const {
    return usets_;
  }

  // first fact Id in the current batch
  Id first_id_;
  Ownership* ownership_;
  std::map<Pid, PerPredicate> defines_;

 private:
  Usets usets_;
};

struct DerivedFactOwnership {
  folly::Range<const Id*> ids;
  folly::Range<const UsetId*> owners;
};

struct DerivedFactOwnershipIterator {
  virtual ~DerivedFactOwnershipIterator() = default;
  DerivedFactOwnershipIterator() = default;
  DerivedFactOwnershipIterator(const DerivedFactOwnershipIterator&) = delete;
  DerivedFactOwnershipIterator& operator=(const DerivedFactOwnershipIterator&) =
      delete;
  DerivedFactOwnershipIterator(DerivedFactOwnershipIterator&&) = delete;
  DerivedFactOwnershipIterator& operator=(DerivedFactOwnershipIterator&&) =
      delete;

  // Note: the result is only valid until the next call to get()
  virtual folly::Optional<DerivedFactOwnership> get() = 0;
};

///
// Compute ownership data for derived facts
//
std::unique_ptr<ComputedOwnership> computeDerivedOwnership(
    Ownership& ownership,
    Lookup* base,
    DerivedFactOwnershipIterator* iter);

struct DerivedDependencyIterator {
  virtual ~DerivedDependencyIterator() = default;
  DerivedDependencyIterator() = default;
  DerivedDependencyIterator(const DerivedDependencyIterator&) = delete;
  DerivedDependencyIterator& operator=(const DerivedDependencyIterator&) =
      delete;
  DerivedDependencyIterator(DerivedDependencyIterator&&) = delete;
  DerivedDependencyIterator& operator=(DerivedDependencyIterator&&) = delete;
  virtual folly::Optional<std::pair<std::vector<Id>, std::vector<Id>>>
  get() = 0;
};

///
// Add ownership data for externally derived facts
//
void addDerived(
    Lookup* lookup,
    DefineOwnership* define,
    Pid pid,
    DerivedDependencyIterator* it);

} // namespace rts
} // namespace glean
} // namespace facebook
