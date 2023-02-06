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
#include "glean/rts/ondemand.h"
#include "glean/rts/stats.h"
#include "glean/rts/store.h"
#include "glean/rts/substitution.h"

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
private:
  class Facts final {
  public:
    explicit Facts(Id start) noexcept
      : starting_id(start) {}
    Facts(Facts&& other) noexcept
      : starting_id(other.starting_id)
      , facts(std::move(other.facts))
      , fact_memory(other.fact_memory) {}
    Facts& operator=(Facts&& other) noexcept {
      starting_id = other.starting_id;
      facts = std::move(other.facts);
      fact_memory = other.fact_memory;
      return *this;
    }

    bool empty() const {
      return facts.empty();
    }

    size_t size() const {
      return facts.size();
    }

    Id startingId() const {
      return starting_id;
    }

    struct deref {
      Fact::Ref operator()(const Fact::unique_ptr& p) const {
        return p->ref();
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

    Fact::Ref operator[](size_t i) const {
      assert (i < facts.size());
      return facts[i]->ref();
    }

    void clear() {
      facts.clear();
      fact_memory = 0;
    }

    using Token = Fact::unique_ptr;

    Token alloc(Id id, Pid type, Fact::Clause clause) {
      return Fact::create({id, type, clause});
    }

    void commit(Token token) {
      fact_memory += token->size();
      facts.push_back(std::move(token));
    }

    void append(Facts other) {
      facts.insert(
        facts.end(),
        std::make_move_iterator(other.facts.begin()),
        std::make_move_iterator(other.facts.end()));
      fact_memory += other.fact_memory;
    }

    /// Return the number of bytes occupied by facts.
    size_t factMemory() const noexcept {
      return fact_memory;
    }

    /// Return (approximately) the total number of bytes allocated by the
    /// FactSet. This currently does *not* include the memory allocated by seek
    /// indices.
    size_t allocatedMemory() const noexcept;

  private:
    Id starting_id;
    std::vector<Fact::unique_ptr> facts;
    size_t fact_memory = 0;
  };

public:
  explicit FactSet(Id start);
  FactSet(FactSet&&) noexcept;
  FactSet& operator=(FactSet&&);
  ~FactSet() noexcept;

  FactSet(const FactSet&) = delete;
  FactSet& operator=(const FactSet&) = delete;


  size_t size() const noexcept {
    return facts.size();
  }

  bool empty() const noexcept {
    return facts.empty();
  }

  using const_iterator = Facts::const_iterator;

  const_iterator begin() const {
    return facts.begin();
  }

  const_iterator end() const {
    return facts.end();
  }

  /// Return iterator to the first fact with an id that's not less than the
  /// given id (or end() if no such fact exists).
  const_iterator lower_bound(Id id) const {
    return begin() +
      (id <= facts.startingId()
        ? 0
        : std::min(distance(facts.startingId(), id), facts.size()));
  }

  const_iterator upper_bound(Id id) const {
    return begin() +
      (id < facts.startingId()
        ? 0
        : std::min(distance(facts.startingId(), id)+1, facts.size()));
  }

  /// Return the number of bytes occupied by facts.
  size_t factMemory() const noexcept {
    return facts.factMemory();
  }

  size_t allocatedMemory() const noexcept;

  PredicateStats predicateStats() const;

 // Lookup implementation

  Id idByKey(Pid type, folly::ByteRange key) override;

  Pid typeById(Id id) override;

  bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override;

  Id startingId() const override {
    return facts.startingId();
  }

  Id firstFreeId() const override {
    return facts.startingId() + facts.size();
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

  std::unique_ptr<FactIterator> seekWithinSection(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size,
    Id from,
    Id to) override;

  UsetId getOwner(Id) override { return INVALID_USET; }

  // Define implementation

  Id define(Pid type, Fact::Clause, Id max_ref = Id::invalid()) override;

  struct Serialized {
    Id first;
    size_t count;
    binary::Output facts;
  };
  Serialized serialize() const;

  Serialized serializeReorder(folly::Range<const uint64_t*> order) const;

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
  Facts facts;
  DenseMap<Pid, FastSetBy<const Fact *, FactByKeyOnly>> keys;

  /// Cached predicate stats. We create these on-demand rather than maintain
  /// them throughout because most FactSets don't need them.
  struct CachedPredicateStats;
  mutable OnDemand<CachedPredicateStats> predicate_stats;

  /// Index for prefix seeks. It is lazily initialised and slow as we typically
  /// don't do seeks on FactSets.
  struct Index;
  OnDemand<Index> index;
};

}
}
}
