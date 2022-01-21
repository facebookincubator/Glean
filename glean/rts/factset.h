/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/define.h"
#include "glean/rts/densemap.h"
#include "glean/rts/fact.h"
#include "glean/rts/inventory.h"
#include "glean/rts/substitution.h"
#include "glean/rts/store.h"

#include <atomic>
#include <boost/intrusive/list.hpp>
#include <boost/iterator/transform_iterator.hpp>
#include <folly/container/F14Set.h>
#include <folly/Synchronized.h>

namespace facebook {
namespace glean {
namespace rts {

// We used to have maps Id -> Fact* and pair<Id,ByteRange> -> Fact* here but
// this used quite a bit of memory (almost exactly as much as the facts
// themselves). Sets with folly's heterogenous key comparisons should use a
// lot less memory per fact.

struct FactById {
  using value_type = Id;

  static value_type get(const Fact *fact) {
    return fact->id();
  }

  static value_type get(const Fact::unique_ptr& p) {
    return get(p.get());
  }

  static value_type get(const value_type& x) {
    return x;
  }
};

struct FactByKey {
  using value_type = std::pair<Pid, folly::ByteRange>;

  static value_type get(const Fact *fact) {
    return {fact->type(), fact->key()};
  }

  static value_type get(const Fact::unique_ptr& p) {
    return get(p.get());
  }

  static value_type get(const value_type& x) {
    return x;
  }
};

struct FactByKeyOnly {
  using value_type = folly::ByteRange;

  static value_type get(const Fact *fact) {
    return fact->key();
  }

  static value_type get(const Fact::unique_ptr& p) {
    return get(p.get());
  }

  static value_type get(const value_type& x) {
    return x;
  }
};

template<typename By> struct EqualBy {
  template<typename T, typename U>
  bool operator()(const T& x, const U& y) const {
    return By::get(x) == By::get(y);
  }
};

template<typename By> struct HashBy {
  template<typename T>
  uint64_t operator()(const T& x) const {
    return folly::Hash()(By::get(x));
  }
};

template<typename T, typename By> using FastSetBy =
  folly::F14FastSet<
    T,
    folly::transparent<HashBy<By>>,
    folly::transparent<EqualBy<By>>>;

/**
 * A set of facts which can be looked up by id or by key.
 *
 * Iteration through the set happens in the order in which facts have been
 * first inserted.
 *
 */
class FactSet final : public Define {
public:
  explicit FactSet(Id start)
    : starting_id(start)
    , fact_memory(0)
    {}
  FactSet(FactSet&&) = default;
  FactSet& operator=(FactSet&&) = default;
  FactSet(const FactSet&) = delete;
  FactSet& operator=(const FactSet&) = delete;

  size_t size() const noexcept {
    return facts.size();
  }

  bool empty() const noexcept {
    return facts.empty();
  }

  // TODO: make this into an iterator over facts
  struct deref {
    const Fact& operator()(const Fact::unique_ptr& p) const {
      return *p;
    }
  };

  using const_iterator =
    boost::transform_iterator<
      deref,
      std::vector<Fact::unique_ptr>::const_iterator>;

  const_iterator begin() const {
    return boost::make_transform_iterator(facts.begin(), deref());
  }

  const_iterator end() const {
    return boost::make_transform_iterator(facts.end(), deref());
  }

  /// Return iterator to the first fact with an id that's not less than the
  /// given id (or end() if no such fact exists).
  const_iterator lower_bound(Id id) const {
    return begin() +
      (id <= starting_id
        ? 0
        : std::min(distance(starting_id, id), facts.size()));
  }

  const_iterator upper_bound(Id id) const {
    return begin() +
      (id < starting_id
        ? 0
        : std::min(distance(starting_id, id)+1, facts.size()));
  }

  /// Return the number of bytes occupied by facts.
  size_t factMemory() const noexcept {
    return fact_memory;
  }

 // Lookup implementation

  Id idByKey(Pid type, folly::ByteRange key) override;

  Pid typeById(Id id) override;

  bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override;

  Id startingId() const override {
    return starting_id;
  }

  Id firstFreeId() const override {
    return starting_id + facts.size();
  }

  Interval count(Pid pid) const override;

  std::unique_ptr<FactIterator> enumerate(
    Id from = Id::invalid(),
    Id upto = Id::invalid()) override;
  std::unique_ptr<FactIterator> enumerateBack(
    Id from = Id::invalid(),
    Id downto = Id::invalid()) override;

  /// Prefix seeks. This function can be called from multiple threads but prefix
  /// seeks can *not* be interleaved with modifying the FactSet.
  ///
  /// WARNING: This is currently not intended for production use as it is very
  /// slow. The first call for each predicate will be especially slow as it will
  /// need to create an index.
  std::unique_ptr<FactIterator> seek(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size) override;

  // Define implementation

  Id define(Pid type, Fact::Clause, Id max_ref = Id::invalid()) override;

  thrift::Batch serialize() const;

  thrift::Batch serializeReorder(folly::Range<const uint64_t*> order) const;

  // Substitute all facts in the set and split it into a global and a local part
  // based on the substitution. Facts with Ids that are in the range of the
  // substitution go into the global part - they are moved from the set
  // to the global Store. Facts that are beyond that range (i.e., those with
  // id >= subst.finish()) are assigned new Ids which don't clash with the
  // domain of the substitution and are added to the local part which is
  // returned by the function.
  FactSet rebase(
    const Inventory& inventory,
    const Substitution& subst,
    Store& global) const;

  /// Append a set of facts. This operation is only well defined under the
  /// following conditions.
  ///
  ///   * other.startingId() == this->firstFreeId() unless one of the FactSets
  ///     is empty
  ///   * the fact sets are disjoint, i.e., there are no facts that exist in
  ///     both sets
  ///
  void append(FactSet other);

  /// Checks if appending a particular fact set would be well defined.
  bool appendable(const FactSet& other) const;

  bool sanityCheck() const;

private:
  Id starting_id;
  std::vector<Fact::unique_ptr> facts;
  DenseMap<Pid, FastSetBy<const Fact *, FactByKeyOnly>> keys;
  size_t fact_memory;

  /// Index for prefix seeks. It is lazily initialised and slow as we typically
  /// don't do seeks on FactSets.
  class Index final {
  public:
    Index() : impl(nullptr) {}
    ~Index();
    Index(Index&&) noexcept;
    Index& operator=(Index&&);
    Index(const Index&) = delete;
    Index& operator=(const Index&) = delete;

    void swap(Index&) noexcept;

    /// Type of index maps
    using map_t = std::map<folly::ByteRange, const Fact *>;

    /// Type of index maps with synchronised access
    using entry_t = folly::Synchronized<map_t>;

    /// Get a reference to the index map for the given predicate, creating an
    /// empty one if necessary
    entry_t& operator[](Pid pid);

  private:
    struct Impl;

    std::atomic<Impl *> impl;
  };

  Index index;
};

}
}
}
