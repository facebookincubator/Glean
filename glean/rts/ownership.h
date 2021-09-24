// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/rts/id.h"
#include "glean/rts/ownership/uset.h"
#include "glean/rts/id.h"

#include <folly/Optional.h>
#include <folly/Range.h>

namespace facebook {
namespace glean {
namespace rts {

class Inventory;
struct Lookup;

using UnitId = uint32_t;

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

  // `OwnershipUnit`s are expected to be produced in strictly ascending order of
  // `unit`. Returning more that one `OwnershipUnit` for a `unit` isn't
  // currently supported but should be added.
  virtual folly::Optional<OwnershipUnit> get() = 0;
};
using MutableOwnerSet = folly::compression::MutableEliasFanoCompressedList;
using OwnerSet = folly::compression::EliasFanoCompressedList;

struct OwnershipSetIterator {
  virtual ~OwnershipSetIterator() {}
  virtual std::pair<size_t,size_t> sizes() const = 0;
  virtual folly::Optional<std::pair<UsetId,SetExpr<const OwnerSet*>>> get() = 0;
};

struct Ownership {
  virtual ~Ownership() {}

  virtual UsetId getOwner(Id id) = 0;

  virtual std::unique_ptr<OwnershipSetIterator> getSetIterator() = 0;
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
 * Compute ownership data for non-derived facts
 */
std::unique_ptr<ComputedOwnership> computeOwnership(
  const Inventory& inventory,
  Lookup& lookup,
  OwnershipUnitIterator *iter);

}
}
}
