/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/binary.h"
#include "glean/rts/factset.h"
#include "glean/rts/inventory.h"
#include "glean/rts/substitution.h"

namespace facebook {
namespace glean {
namespace rts {

template <typename Iface>
struct StackedBase : Iface {
  StackedBase(Lookup* b, Iface* s)
      : base(b), stacked(s), mid(stacked->startingId()) {}

  // Lookup implementation

  Id idByKey(Pid type, folly::ByteRange key) override {
    if (auto id = stacked->idByKey(type, key)) {
      return id;
    } else {
      auto bid = base->idByKey(type, key);
      return bid < mid ? bid : Id::invalid();
    }
  }

  Pid typeById(Id id) override {
    return id < mid ? base->typeById(id) : stacked->typeById(id);
  }

  bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override {
    return id < mid ? base->factById(id, std::move(f))
                    : stacked->factById(id, std::move(f));
  }

  Id startingId() const override {
    return base->startingId();
  }
  Id firstFreeId() const override {
    return stacked->firstFreeId();
  }

  Interval count(Pid pid) const override {
    return base->count(pid) + stacked->count(pid);
  }

  std::unique_ptr<FactIterator> enumerate(Id from, Id upto) override {
    if (from < mid) {
      if (upto && upto <= mid) {
        return base->enumerate(from, upto);
      } else {
        return FactIterator::append(
            base->enumerate(from, mid), stacked->enumerate(mid, upto));
      }
    } else {
      return stacked->enumerate(from, upto);
    }
  }

  std::unique_ptr<FactIterator> enumerateBack(Id from, Id downto) override {
    if (from && from <= mid) {
      return base->enumerateBack(from, downto);
    } else if (downto >= mid) {
      return stacked->enumerateBack(from, downto);
    } else {
      return FactIterator::append(
          stacked->enumerateBack(from, mid), base->enumerateBack(mid, downto));
    }
  }

  std::unique_ptr<FactIterator>
  seek(Pid type, folly::ByteRange start, size_t prefix_size) override {
    return FactIterator::merge(
        base->seekWithinSection(type, start, prefix_size, Id::invalid(), mid),
        stacked->seek(type, start, prefix_size),
        prefix_size);
  }

  std::unique_ptr<FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange start,
      size_t prefix_size,
      Id from,
      Id to) override {
    if (to <= mid) {
      return base->seekWithinSection(type, start, prefix_size, from, to);
    } else if (mid <= from) {
      return stacked->seekWithinSection(type, start, prefix_size, from, to);
    } else {
      return FactIterator::merge(
          base->seekWithinSection(type, start, prefix_size, from, mid),
          stacked->seekWithinSection(type, start, prefix_size, mid, to),
          prefix_size);
    }
  }

  UsetId getOwner(Id id) override {
    // The stacked DB may specify the owner for a fact in the base DB
    // due to propagation of ownership from facts in the stacked DB,
    // so we have to check the stacked DB first.
    auto set = stacked->getOwner(id);
    if (set != INVALID_USET) {
      return set;
    } else {
      return id < mid ? base->getOwner(id) : INVALID_USET;
    }
  }

 protected:
  Lookup* base;
  Iface* stacked;
  Id mid;
};

/**
 * A Stacked stacks a Lookup or Define on top of a base Lookup, allowing facts
 * to be looked up in both and new facts added to the stacked object if
 * supported. The stacked object's starting id determines the boundary between
 * the two - any facts in the base Lookup with an id >= that will be ignored.
 *
 *
 * This is an attempt to model the corresponding Haskell hierarchy in C++.
 */
template <typename Iface>
struct Stacked;

template <>
struct Stacked<Lookup> final : StackedBase<Lookup> {
  using StackedBase<Lookup>::StackedBase;
};

template <>
struct Stacked<Define> final : StackedBase<Define> {
  using StackedBase<Define>::StackedBase;

  // Define implementation

  Id define(Pid type, Fact::Clause clause, Id max_ref = Id::invalid())
      override {
    // TODO: We probably want to look up in stacked first but the current
    // interface doesn't support doing that without two lookups.

    // If the clause references a local fact then it can't possibly exist in the
    // global Lookup so don't bother looking it up there.
    if (max_ref < mid) {
      auto id = base->idByKey(type, clause.key());
      if (id && id < mid) {
        // NOTE: We assume that 'value' has been typechecked. If it is empty
        // then
        //       it must be the only inhabitant of its type (which must be () or
        //       similar). This means that there is no need to check against the
        //       value stored in the database as that must be empty, too.
        if (clause.value_size != 0) {
          [[maybe_unused]] bool found =
              base->factById(id, [&](auto, auto found) {
                if (clause.value() != found.value()) {
                  id = Id::invalid();
                }
              });
          assert(found);
        }
        return id;
      }
    }
    return stacked->define(type, clause, max_ref);
  }
};

} // namespace rts
} // namespace glean
} // namespace facebook
