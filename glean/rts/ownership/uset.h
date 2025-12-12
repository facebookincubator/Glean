/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ownership/setu32.h"

#include <folly/Hash.h>
#include <folly/container/F14Set.h>
#include <vector>

namespace facebook {
namespace glean {
namespace rts {

using UsetId = uint32_t;
constexpr UsetId INVALID_USET = 0xffffffff;
constexpr UsetId SPECIAL_USET = 0xfffffffe; // special value as a sentinel

enum SetOp { Or, And };

/**
 * A set expression
 *
 * Elements of the set are
 *    0 ... MAX_UNIT_ID   : units
 *    MAX_UNIT_ID+1 ...   : sets
 *
 * facts with this owner set are visible if and only if:
 *     Or => any of the members of the set are visible
 *     And => all the members of the set are visible
 */
template <typename Set>
struct SetExpr {
  SetOp op;
  Set set;

  bool operator==(const SetExpr<Set>& other) const& {
    return op == other.op && set == other.set;
  }
};

/**
 * A "unique" set stored by `Usets` below. This is a `SetU32` with a memoized
 * hash, a ref count and some administrative data used by the ownership
 * algorithms. It should probably be given a more sane interface.
 */
struct Uset {
  explicit Uset(SetU32 s, uint32_t r = 0) : exp({Or, std::move(s)}), refs(r) {}
  explicit Uset(SetU32 s, SetOp op = Or, uint32_t r = 0)
      : exp({op, std::move(s)}), refs(r) {}

  void rehash() {
    hash = exp.set.hash(exp.op); // don't forget to include op in the hash
  }

  void use(int32_t n) {
    refs += n;
  }

  void drop() {
    if (--refs == 0) {
      delete this;
    }
  }

  void* link() const {
    return ptr;
  }

  void link(void* p) {
    ptr = p;
  }

  /** The set */
  SetExpr<SetU32> exp;

  /** The set's hash which must be memoized explicitly via `rehash`. */
  size_t hash;

  /** Reference count */
  uint32_t refs;

  /**
   * Once a set is promoted to the DB (i.e. it is the ownership set of
   * at least one fact), we assign it a 32-bit ID. Before it is promoted,
   * id is INVALID_USET.
   */
  UsetId id = INVALID_USET;

  bool promoted() const {
    return id != INVALID_USET;
  }

  /**
   * Generic pointer used temporarily for a variety of things - it is much
   * faster than a F14FastMap<Uset *, T>.
   */
  void* ptr = nullptr;

  struct Eq {
    bool operator()(const Uset* x, const Uset* y) const {
      return x == y || x->exp == y->exp;
    }
  };

  struct Hash {
    using folly_is_avalanching = std::true_type;
    size_t operator()(const Uset* x) const {
      return x->hash;
    }
  };

  using MutableEliasFanoList = SetU32::MutableEliasFanoList;
  SetExpr<MutableEliasFanoList> toEliasFano(UsetId max) const;
};

/**
 * Container for `Usets` which guarantees to store each set exactly once.
 */
struct Usets {
  explicit Usets(uint32_t firstId) : firstId(firstId), nextId(firstId) {}

  Usets(Usets&& other) noexcept : firstId(other.firstId), nextId(other.nextId) {
    std::swap(usets, other.usets);
    stats = other.stats;
  }

  ~Usets() {
    // We can't delete the Usets before destroying the `usets` map because
    // the latter might access hashes in its destructor.
    std::vector<Uset*> refs;
    refs.reserve(usets.size());
    refs.insert(refs.end(), usets.begin(), usets.end());
    usets.clear();
    for (auto ref : refs) {
      delete ref;
    }
  }

  template <typename F>
  void foreach(F&& f) {
    for (auto entry : usets) {
      f(entry);
    }
  }

  Uset* add(Uset* entry) {
    entry->rehash();
    const auto [p, added] = usets.insert(entry);
    if (added) {
      entry->exp.set.shrink_to_fit();
      ++stats.adds;
      stats.bytes += entry->exp.set.bytes();
      return entry;
    } else {
      ++stats.dups;
      use(*p, entry->refs);
      return *p;
    }
  }

  Uset* add(std::unique_ptr<Uset> entry) {
    auto p = add(entry.get());
    if (p == entry.get()) {
      entry.release();
    }
    return p;
  }

  Uset* add(SetU32 set, uint32_t refs = 0) {
    return add(std::unique_ptr<Uset>(new Uset(std::move(set), refs)));
  }

  Uset* lookup(Uset* entry) const {
    entry->rehash();
    auto it = usets.find(entry);
    if (it != usets.end()) {
      return *it;
    } else {
      return nullptr;
    }
  }

  // only when both sets have the same op
  Uset* merge(Uset* left, Uset* right) {
    SetU32 set;
    assert(left->exp.op == right->exp.op);
    auto res = SetU32::merge(set, left->exp.set, right->exp.set);
    if (res == &set) {
      return add(std::move(set), 1);
    } else {
      auto& r = res == &left->exp.set ? left : right;
      use(r, 1);
      return r;
    }
  }

  // merge a SetU32 directly. Op is assumed to be Or.
  Uset* merge(SetU32 left, Uset* right) {
    SetU32 set;
    assert(right->exp.op == Or);
    auto res = SetU32::merge(set, left, right->exp.set);
    if (res == &set) {
      return add(std::move(set), 1);
    } else {
      if (res == &right->exp.set) {
        use(right, 1);
        return right;
      } else {
        return add(std::move(left), 1);
      }
    }
  }

  void use(Uset* set, uint32_t refs = 1) {
    set->refs += refs;
  }

  void drop(Uset* uset) {
    assert(uset->refs != 0);
    --uset->refs;
    if (uset->refs == 0) {
      assert(!uset->promoted());
      usets.erase(uset);
      stats.bytes -= uset->exp.set.bytes();
      delete uset;
    }
  }

  void promote(Uset* uset) {
    if (!uset->promoted()) {
      uset->id = nextId++;
      ++uset->refs;
      ++stats.promoted;
    }
  }

  size_t size() const {
    return usets.size();
  }

  struct Stats {
    size_t bytes = 0;
    size_t promoted = 0;

    size_t adds = 0;
    size_t dups = 0;
  };

  const Stats& statistics() const {
    return stats;
  }

  UsetId getFirstId() const {
    return firstId;
  }

  UsetId getNextId() const {
    return nextId;
  }

  // Merge another Usets into this one. The other Usets must be
  // disjoint and using sets numbered from nextId onwards.
  void append(Usets&& other) {
    CHECK_EQ(other.firstId, nextId);
    CHECK_EQ(other.nextId, nextId + other.usets.size());
    other.foreach([&](Uset* uset) {
      auto p = add(uset);
      CHECK(p == uset); // we should have added it
    });
    other.usets
        .clear(); // ownership of the underlying sets has been transferred
    nextId = other.nextId;
  }

  using MutableEliasFanoList = SetU32::MutableEliasFanoList;
  std::vector<SetExpr<MutableEliasFanoList>> toEliasFano(UsetId max);

 private:
  folly::F14FastSet<Uset*, Uset::Hash, Uset::Eq> usets;
  Stats stats;
  const UsetId firstId;
  UsetId nextId;
};

} // namespace rts
} // namespace glean
} // namespace facebook
