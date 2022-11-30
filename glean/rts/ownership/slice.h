/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/fact.h"
#include "glean/rts/lookup.h"
#include "glean/rts/ownership.h"

namespace facebook {
namespace glean {
namespace rts {

///
// A Slice is a bitmap, with one bit for each Uset, to indicate
// whether facts with that Uset should be visible or not.
//
// A Slice only makes sense in the context of a particular DB.
//
struct Slice {
  explicit Slice(UsetId first, std::vector<bool> set) :
      first_(first), set_(std::move(set)) {}

  bool visible(UsetId uset) {
    if (uset == INVALID_USET) {
      return false;
    }
    assert(uset >= first_ && uset - first_ < set_.size());
    return set_[uset - first_];
  }

 private:
  UsetId first_;
  std::vector<bool> set_;
};

///
// construct a Slice for the given Ownership, given a set of Units
// that should be included/excluded.
//
std::unique_ptr<Slice> slice(
    Ownership& ownership,
    const std::vector<UnitId>& units, // must be sorted
    bool exclude);

///
// A "slice" of a Lookup, restricted to returning only those facts
// visible in the given (Ownership, Slice).
//
struct Sliced : Lookup {
  ~Sliced() override {}

  Sliced(Lookup *base, Slice *slice)
      : base_(base), slice_(slice) {}

  Id idByKey(Pid type, folly::ByteRange key) override {
    if (auto id = base_->idByKey(type, key)) {
      if (slice_->visible(base_->getOwner(id))) {
        return id;
      }
    }
    return Id::invalid();
  }

  Pid typeById(Id id) override {
    return base_->typeById(id);
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
          return slice_->visible(base_->getOwner(id));
        });
  }

  std::unique_ptr<FactIterator> enumerateBack(
      Id from,
      Id downto) override {
    return FactIterator::filter(
        base_->enumerate(from,downto),
        [&](Id id) {
          return slice_->visible(base_->getOwner(id));
        });
  }

  std::unique_ptr<FactIterator> seek(
      Pid type,
      folly::ByteRange start,
      size_t prefix_size) override {
    return FactIterator::filter(
        base_->seek(type, start, prefix_size),
        [&](Id id) {
          return slice_->visible(base_->getOwner(id));
        });
  }

  std::unique_ptr<FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange start,
      size_t prefix_size,
      Id from,
      Id to) override {
    return FactIterator::filter(
        base_->seekWithinSection(type, start, prefix_size, from, to),
        [&](Id id) {
          return slice_->visible(base_->getOwner(id));
        });
  }

  UsetId getOwner(Id id) override {
    // Like factById(), we can assume that if the caller has an Id then the Id
    // is visible, so we don't have to check
    return base_->getOwner(id);
  }

 private:
  Lookup *base_;
  Slice *slice_;
};

}
}
}
