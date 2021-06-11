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

///
// A Slice is a bitmap, with one bit for each Uset, to indicate
// whether facts with that Uset should be visible or not.
//
// A Slice only makes sense in the context of a particular DB.
//
struct Slice {
  explicit Slice(std::vector<bool> set) : set_(std::move(set)) {}

  bool visible(UsetId uset) { return set_[uset]; }

 private:
  std::vector<bool> set_;
};

struct Ownership {
  virtual ~Ownership() {}
  virtual UsetId getUset(Id id) = 0;
  virtual std::unique_ptr<Slice> slice(
      std::vector<UnitId> units,
      bool exclude) const = 0;
    // units must be sorted
};

std::unique_ptr<Ownership> computeOwnership(
    const Inventory& inventory,
    Lookup& lookup,
    OwnershipUnitIterator *iter);

}
}
}
