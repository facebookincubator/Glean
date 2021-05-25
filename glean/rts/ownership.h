#pragma once

#include "glean/rts/id.h"

#include <folly/Range.h>

namespace facebook {
namespace glean {
namespace rts {

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
  uint32_t unit;

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

}
}
}
