/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/binary.h"
#include "glean/rts/id.h"
#include "glean/rts/ownership/uset.h"

#include <folly/Optional.h>
#include <folly/Range.h>

#include <string>

namespace facebook {
namespace glean {
namespace rts {

using UnitId = uint32_t;

using MutableOwnerSet = folly::compression::MutableEliasFanoCompressedList;
using OwnerSet = folly::compression::EliasFanoCompressedList;

void serializeEliasFano(binary::Output& out, const OwnerSet& set);

/// Deserialize an OwnerSet from a binary::Input. Note that the
// OwnerSet points to the contents of the binary::Input, so that
// must remain alive as long as the OwnerSet is needed.
OwnerSet deserializeEliasFano(binary::Input& in);

/// Iterates over the ownership set expressions of a single DB, in increasing
/// UsetId order. Each set is a UsetId paired with a SetExpr (an Or/And over
/// the set's member elements). The sets occupy the contiguous UsetId range
/// [first, first + count) reported by sizes().
struct OwnershipSetIterator {
  OwnershipSetIterator() = default;
  virtual ~OwnershipSetIterator() = default;
  OwnershipSetIterator(const OwnershipSetIterator&) = delete;
  OwnershipSetIterator& operator=(const OwnershipSetIterator&) = delete;
  OwnershipSetIterator(OwnershipSetIterator&&) = delete;
  OwnershipSetIterator& operator=(OwnershipSetIterator&&) = delete;

  /// Returns {first, count}: `first` is the lowest UsetId produced by this
  /// iterator and `count` is the number of sets, so the sets cover the
  /// range [first, first + count).
  virtual std::pair<size_t, size_t> sizes() const = 0;

  /// Returns the next set as a {UsetId, SetExpr} pair and advances the
  /// iterator. Sets are produced in increasing UsetId order. Returns
  /// folly::none once every set has been consumed.
  ///
  /// The returned OwnerSet pointer refers to storage owned by the iterator
  /// and is only valid until the next call to get().
  virtual folly::Optional<std::pair<UsetId, SetExpr<const OwnerSet*>>>
  get() = 0;
};

struct OwnershipStats {
  uint64_t num_units;
  size_t units_size;

  uint64_t num_sets;
  size_t sets_size;

  uint64_t num_owner_entries;
  size_t owners_size;

  int64_t num_orphan_facts; // <0 means "don't know yet"
};

///
// Interface for reading ownership data.
//
struct Ownership {
  Ownership() = default;
  virtual ~Ownership() = default;
  Ownership(const Ownership&) = delete;
  Ownership& operator=(const Ownership&) = delete;
  Ownership(Ownership&&) = delete;
  Ownership& operator=(Ownership&&) = delete;

  // Iterate through all the ownership expressions
  virtual std::unique_ptr<OwnershipSetIterator> getSetIterator() = 0;

  // Next unused set ID
  virtual UsetId nextSetId() = 0;

  // Look up a set, can return INVALID_USET if it doesn't exist
  virtual UsetId lookupSet(Uset*) = 0;

  // Fetch the set corresponding to a UsetId. This is used for
  // introspection only.
  virtual folly::Optional<SetExpr<SetU32>> getUset(UsetId) = 0;

  // Look up the UnitId for a unit
  virtual folly::Optional<UnitId> getUnitId(folly::ByteRange unit) = 0;

  // Return stats about the ownership data
  virtual OwnershipStats getStats() = 0;
};

/**
 * Computed ownership data stored in memory.
 * Holds data about multiple predicates.
 * Contains the canonical 'UsetId's that will be added to the database.
 */
struct ComputedOwnership {
  ComputedOwnership(Usets&& sets, std::vector<std::pair<Id, UsetId>>&& facts)
      : sets_(std::move(sets)), facts_(std::move(facts)) {}

  // Sets, indexed by UsetId starting at firstId_
  Usets sets_;

  // Maps fact Ids to owner sets, represented as intervals
  std::vector<std::pair<Id, UsetId>> facts_;
};

/// A computed fact-to-accessibility-owner relation, ready for storage.
///
/// Accessibility owners describe which callers may read a fact. Visibility
/// owners record the source units that keep the fact current. Both use Boolean
/// expressions: OR allows any member, while AND requires every member. Public
/// access is a unit selected by every caller.
///
/// The unit names, expressions and fact-owner intervals share these invariants:
/// - units_[i] has ID firstUnitId_ + i. The vector is nonempty and starts with
///   "public"; subsequent entries name ACL groups or special access units.
/// - owners_.sets_ starts immediately after the unit IDs. Units and expressions
///   share one ID namespace.
/// - owners_.facts_ has strictly increasing fact-ID boundaries. Each owner
///   applies until the next boundary and is a unit ID, an expression ID, or
///   INVALID_USET for an unowned interval. A boundary at firstFreeId() can
///   close the final interval.
struct ComputedAccessibilityOwnership {
  ComputedAccessibilityOwnership(
      UnitId firstUnitId,
      std::vector<std::string> units,
      Usets sets,
      std::vector<std::pair<Id, UsetId>> facts)
      : firstUnitId_(firstUnitId),
        units_(std::move(units)),
        owners_(std::move(sets), std::move(facts)) {}

  UnitId firstUnitId_;
  std::vector<std::string> units_;
  ComputedOwnership owners_;
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
  folly::Range<const Ids*> ids;
};

struct OwnershipUnitIterator {
  OwnershipUnitIterator() = default;
  virtual ~OwnershipUnitIterator() = default;
  OwnershipUnitIterator(const OwnershipUnitIterator&) = delete;
  OwnershipUnitIterator& operator=(const OwnershipUnitIterator&) = delete;
  OwnershipUnitIterator(OwnershipUnitIterator&&) = delete;
  OwnershipUnitIterator& operator=(OwnershipUnitIterator&&) = delete;

  // `OwnershipUnit`s are expected to be produced in strictly
  // ascending order of `unit`.
  virtual folly::Optional<OwnershipUnit> get() = 0;
};

struct Inventory;
struct Lookup;

/**
 * Compute ownership data for non-derived facts
 */
std::unique_ptr<ComputedOwnership> computeOwnership(
    const Inventory& inventory,
    Lookup& lookup,
    Lookup* base_lookup,
    OwnershipUnitIterator* iter);

void setOwnershipCompactThreshold(size_t threshold);
size_t getOwnershipCompactThreshold();
void setOwnershipMergeCacheSize(size_t size);
size_t getOwnershipMergeCacheSize();

} // namespace rts
} // namespace glean
} // namespace facebook
