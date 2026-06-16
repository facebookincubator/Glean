/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <boost/dynamic_bitset.hpp>

#include <folly/Utility.h>

#include "glean/rts/fact.h"
#include "glean/rts/lookup.h"
#include "glean/rts/ownership.h"
#include "glean/rts/serialize.h"

namespace facebook {
namespace glean {
namespace rts {

/// Records, for each UsetId in the range [first(), end()), whether facts
/// with that Uset should be visible.
///
/// A Slice only makes sense in the context of a particular DB. It normally
/// covers only the UsetIds for promoted usets in that DB, not for units. So
/// units are not covered by slices.
struct Slice {
  /// Constructs a Slice covering UsetIds starting at `first`, with `set`
  /// giving the visibility of each successive UsetId.
  explicit Slice(UsetId first, boost::dynamic_bitset<uint64_t> set)
      : first_(first), set_(std::move(set)) {}

  /// Whether facts with the given Uset are visible. `uset` must be in
  /// [first(), end()).
  bool visible(UsetId uset) const {
    assert(
        uset != INVALID_USET && uset >= first_ && uset - first_ < set_.size());
    return set_[uset - first_];
  }

  bool inRange(UsetId usetid) const {
    return usetid >= first() && usetid < end();
  }

  UsetId first() const {
    return first_;
  }
  /// One past the highest UsetId covered by this Slice.
  UsetId end() const {
    return first_ + set_.size();
  }
  bool empty() const {
    return set_.empty();
  }

  void serialize(binary::Output& o) const;
  static std::unique_ptr<Slice> deserialize(binary::Input& i);

 private:
  /// UsetId of bit 0 of set_.
  const UsetId first_;
  /// One bit per UsetId in [first_, first_ + set_.size()).
  const boost::dynamic_bitset<uint64_t> set_;
};

/// A collection of Slices that behaves like a single Slice.
///
/// This is used when querying a stack of Lookups: each Lookup in the
/// stack contributes its own Slice, and Slices presents them as one
/// unified view covering all the UsetIds in the stack.
///
/// Visibility uses AND semantics: a UsetId is visible only if every
/// member Slice that covers it agrees that it is visible (see visible()).
///
/// This relies on each DB in the stack owning a disjoint UsetId range (ids
/// are allocated per-DB from its first_unit_id upwards, which for a stacked
/// DB is the base DB's nextUsetId; see addOwnership in glean/storage). Because
/// the ranges don't overlap, a given UsetId falls in exactly one DB's range,
/// so only that DB's slices (its ownership slice and, if present, its ACL
/// slice) cover it and are ANDed together; slices for other DBs are skipped.
/// Note this disjointness is purely about UsetId *ranges*: a DB still assigns
/// its own UnitIds, so the same unit (e.g. a file path) can have different
/// UnitIds in different DBs in the stack.
struct Slices {
  /// Constructs a unified view over the given Slices, covering the range
  /// that spans all of them.
  explicit Slices(std::vector<const Slice*> slices)
      : slices_(std::move(slices)) {
    UsetId first = 0, end = 0;
    for (auto slice : slices_) {
      if (first == 0 || slice->first() < first) {
        first = slice->first();
        end = std::max(end, slice->end());
      }
    }
    first_ = first;
    end_ = end;
  }

  /// Whether the given UsetId is visible. A UsetId is visible only if at
  /// least one member Slice covers it and every covering Slice agrees it
  /// is visible (AND semantics). For example, an ownership slice and an
  /// ACL slice for the same layer must both make the UsetId visible.
  bool visible(UsetId usetid) const {
    if (usetid == INVALID_USET) {
      return false;
    }
    bool found = false;
    for (auto slice : slices_) {
      if (slice->inRange(usetid)) {
        found = true;
        if (!slice->visible(usetid)) {
          return false;
        }
      }
    }
    return found;
  }

  /// The lowest UsetId covered by any member Slice.
  UsetId first() const {
    return first_;
  }
  /// One past the highest UsetId covered by any member Slice.
  UsetId end() const {
    return end_;
  }
  bool empty() const {
    return first_ == end_;
  }

  /// Whether usetid falls within the overall [first(), end()) range. Note
  /// this does not guarantee a member Slice actually covers usetid, since
  /// the range may contain gaps between member Slices.
  bool inRange(UsetId usetid) const {
    return usetid >= first_ && usetid < end_;
  }

 private:
  /// Bounding range [first_, end_) spanning all member Slices.
  UsetId first_;
  UsetId end_;
  /// The underlying Slices, not owned by this object.
  const std::vector<const Slice*> slices_;
};

///
// construct a Slice for the given Ownership, given a set of Units
// that should be included/excluded.
//
std::unique_ptr<Slice> slice(
    Ownership& ownership,
    const Slices& base,
    const std::vector<UnitId>& units, // must be sorted
    bool exclude);

///
// A "slice" of a Lookup, restricted to returning only those facts
// visible in the given Slice. When the Lookup is a stack, the
// Slice should cover all the UsetIds used in the stack, which
// means it will probably be a Slices containing a Slice for
// each Lookup in the stack.
//
template <typename Slice>
struct Sliced : Lookup, folly::NonCopyableNonMovable {
  ~Sliced() override = default;

  Sliced(Lookup* base, Slice slice) : base_(base), slice_(std::move(slice)) {}

  Id idByKey(Pid type, folly::ByteRange key) override {
    auto id = base_->idByKey(type, key);
    if (id && slice_.visible(base_->getOwner(id))) {
      return id;
    } else {
      return Id::invalid();
    }
  }

  Pid typeById(Id id) override {
    return base_->typeById(id);
  }

  bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override {
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

  std::unique_ptr<FactIterator> enumerate(Id from, Id upto) override {
    return FactIterator::filter(base_->enumerate(from, upto), [&](Id id) {
      return slice_.visible(base_->getOwner(id));
    });
  }

  std::unique_ptr<FactIterator> enumerateBack(Id from, Id downto) override {
    return FactIterator::filter(base_->enumerate(from, downto), [&](Id id) {
      return slice_.visible(base_->getOwner(id));
    });
  }

  std::unique_ptr<FactIterator> seek(
      Pid type,
      folly::ByteRange prefix,
      std::optional<Fact::Ref> restart) override {
    return FactIterator::filter(base_->seek(type, prefix, restart), [&](Id id) {
      return slice_.visible(base_->getOwner(id));
    });
  }

  std::unique_ptr<FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange prefix,
      Id from,
      Id to,
      std::optional<Fact::Ref> restart) override {
    return FactIterator::filter(
        base_->seekWithinSection(type, prefix, from, to, restart),
        [&](Id id) { return slice_.visible(base_->getOwner(id)); });
  }

  UsetId getOwner(Id id) override {
    // Like factById(), we can assume that if the caller has an Id then the Id
    // is visible, so we don't have to check
    return base_->getOwner(id);
  }

 private:
  Lookup* base_;
  Slice slice_;
};

} // namespace rts
} // namespace glean
} // namespace facebook
