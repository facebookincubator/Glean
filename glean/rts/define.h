// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/rts/inventory.h"
#include "glean/rts/lookup.h"
#include "glean/rts/substitution.h"

namespace facebook {
namespace glean {
namespace rts {

/// A set of facts which new facts can be added to.
struct Define : Lookup {
  /// Add a fact to the set and return its id. This can be an existing id
  /// if the fact is already in the set, a new id if it's a new fact and
  /// Id::INVALID if the set contains a fact with the same type and key but a
  /// different value.
  ///
  /// The optional 'max_ref' parameter contains the max fact id referenced by
  /// the clause. This can be used for optimisations by some implementations.
  /// Not setting it or setting it to Id::invalid() is always safe.
  virtual Id define(
    Pid type,
    Fact::Clause clause,
    Id max_ref = Id::invalid()) = 0;
};

/// Define all new facts in a batch, returning the resulting substitution. The
/// facts are typechecked based on the inventory.
Substitution defineUntrustedBatch(
  Define& define,
  const Inventory& inventory,
  Id first,
  const Id * FOLLY_NULLABLE ids,   // nullptr if there are no named facts
  size_t count,
  folly::ByteRange batch);

}
}
}
