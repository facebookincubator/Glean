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
  virtual size_t size() = 0;
  virtual folly::Optional<std::pair<UnitId,OwnerSet*>> get() = 0;
};

///
// A Slice is a bitmap, with one bit for each Uset, to indicate
// whether facts with that Uset should be visible or not.
//
// A Slice only makes sense in the context of a particular DB.
//
struct Slice {
  explicit Slice(std::vector<bool> set) : set_(std::move(set)) {}

  bool visible(UsetId uset) {
    return uset != INVALID_USET ? set_[uset] : false;
  }

 private:
  std::vector<bool> set_;
};

struct Ownership {
  virtual ~Ownership() {}

  virtual UsetId getUset(Id id) = 0;

  virtual std::unique_ptr<OwnershipSetIterator> getSetIterator() = 0;
};


///
// Computed ownership data stored in memory
//
struct ComputedOwnership {
  ~ComputedOwnership() {
    for (auto &set: sets_) {
      set.free();
    }
  }

  ComputedOwnership(
        UsetId firstId,
        std::vector<MutableOwnerSet>&& sets,
        std::vector<std::pair<Id,UsetId>>&& facts) :
      firstId_(firstId),
      sets_(std::move(sets)),
      facts_(std::move(facts)) {}

  UsetId firstId_;

  // Sets, indexed by UsetId starting at firstId_
  std::vector<MutableOwnerSet> sets_;

  // Maps fact Ids to owner sets, represented as intervals
  std::vector<std::pair<Id,UsetId>> facts_;
};


std::unique_ptr<Slice> slice(
    Ownership *ownership,
    const std::vector<UnitId>& units, // must be sorted
    bool exclude);

///
// A "slice" of a Lookup, restricted to returning only those facts
// visible in the given (Ownership, Slice).
//
struct Sliced : Lookup {
  ~Sliced() override {}

  Sliced(Lookup *base, Ownership *ownership, Slice *slice)
      : base_(base), slice_(slice), ownership_(ownership) {}

  Id idByKey(Pid type, folly::ByteRange key) override {
    if (auto id = base_->idByKey(type, key)) {
      if (slice_->visible(ownership_->getUset(id))) {
        return id;
      }
    }
    return Id::invalid();
  }

  Pid typeById(Id id) override {
    if (slice_->visible(ownership_->getUset(id))) {
      return base_->typeById(id);
    } else {
      return Pid::invalid();
    }
  }

  bool factById(
      Id id,
      std::function<void(Pid, Fact::Clause)> f) override {
    return base_->factById(id, std::move(f));
  }

  Id startingId() const override {
    return base_->startingId();
  }

  Id firstFreeId() const override {
    return base_->firstFreeId();
  }

  Interval count(Pid pid) const override {
    // TODO: this is wrong
    return base_->count(pid);
  }

  std::unique_ptr<FactIterator> enumerate(
      Id from,
      Id upto) override {
    return FactIterator::filter(
        base_->enumerate(from,upto),
        [&](Id id) {
          return slice_->visible(ownership_->getUset(id));
        });
  }

  std::unique_ptr<FactIterator> enumerateBack(
      Id from,
      Id downto) override {
    return FactIterator::filter(
        base_->enumerate(from,downto),
        [&](Id id) {
          return slice_->visible(ownership_->getUset(id));
        });
  }

  std::unique_ptr<FactIterator> seek(
      Pid type,
      folly::ByteRange start,
      size_t prefix_size) override {
    return FactIterator::filter(
        base_->seek(type, start, prefix_size),
        [&](Id id) {
          return slice_->visible(ownership_->getUset(id));
        });
  }

 private:
  Lookup *base_;
  Slice *slice_;
  Ownership *ownership_;
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
