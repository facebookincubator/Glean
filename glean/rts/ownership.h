/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/id.h"
#include "glean/rts/ownership/uset.h"
#include "glean/rts/id.h"

#include <folly/Optional.h>
#include <folly/Range.h>

namespace facebook {
namespace glean {
namespace rts {

using UnitId = uint32_t;

using MutableOwnerSet = folly::compression::MutableEliasFanoCompressedList;
using OwnerSet = folly::compression::EliasFanoCompressedList;

struct OwnershipSetIterator {
  virtual ~OwnershipSetIterator() {}
  virtual std::pair<size_t,size_t> sizes() const = 0;
  virtual folly::Optional<std::pair<UsetId,SetExpr<const OwnerSet*>>> get() = 0;
};

///
// Interface for reading ownership data.
//
struct Ownership {
  virtual ~Ownership() {}

  // Return the ownership expression for a fact Id
  virtual UsetId getOwner(Id id) = 0;

  // Iterate through all the ownership expressions
  virtual std::unique_ptr<OwnershipSetIterator> getSetIterator() = 0;

  // Next unused set ID
  virtual UsetId nextSetId() = 0;

  // Look up a set, can return INVALID_USET if it doesn't exist
  virtual UsetId lookupSet(Uset*) = 0;
};

///
// Computed ownership data stored in memory
//
struct ComputedOwnership {
  ~ComputedOwnership() {
    for (auto &set: sets_) {
      set.set.free();
    }
  }

  ComputedOwnership(
        UsetId firstId,
        std::vector<SetExpr<MutableOwnerSet>>&& sets,
        std::vector<std::pair<Id,UsetId>>&& facts) :
      firstId_(firstId),
      sets_(std::move(sets)),
      facts_(std::move(facts)) {}

  UsetId firstId_;

  // Sets, indexed by UsetId starting at firstId_
  std::vector<SetExpr<MutableOwnerSet>> sets_;

  // Maps fact Ids to owner sets, represented as intervals
  std::vector<std::pair<Id,UsetId>> facts_;
};


/**
 * Raw ownership data (facts -> unit)
 */
struct OwnershipUnit {
  /** Id range */
  struct Ids {
    /** First id in range */
    Id start;

    /** Last id in range */
    Id finish;
  };

  /** The ownership unit that the facts belong to. */
  UnitId unit;

  /** Fact ids owner by the unit. */
  folly::Range<const Ids *> ids;
};

struct OwnershipUnitIterator {
  virtual ~OwnershipUnitIterator() {}

  // `OwnershipUnit`s are expected to be produced in strictly
  // ascending order of `unit`.
  virtual folly::Optional<OwnershipUnit> get() = 0;
};

class Inventory;
struct Lookup;

/**
 * Compute ownership data for non-derived facts
 */
std::unique_ptr<ComputedOwnership> computeOwnership(
  const Inventory& inventory,
  Lookup& lookup,
  OwnershipUnitIterator *iter);

}
}
}
