/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/id.h"
#include "glean/rts/ownership/setu32.h"

#include <folly/Hash.h>
#include <folly/container/F14Map.h>
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
 * A ref-counted ownership set expression (`SetExpr<SetU32>`) stored
 * uniquely by `Usets` below. Includes a memoized hash and a `UsetId`
 * assigned on promotion (when the set becomes the ownership set of at
 * least one fact).
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

  /** Whether this set has been promoted (assigned a persistent UsetId by
   * Usets::promote()). */
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
 * Deduplicating store for `Uset` objects. Uses content-based hashing
 * and equality (via F14FastSet) to guarantee each distinct set is stored
 * exactly once. Manages `Uset` lifetimes via reference counting, and
 * assigns persistent `UsetId`s to sets that are promoted (i.e. needed
 * as the ownership set of at least one fact).
 */
struct Usets {
  explicit Usets(uint32_t firstId) : firstId(firstId), nextId(firstId) {}

  Usets(Usets&& other) noexcept : firstId(other.firstId), nextId(other.nextId) {
    std::swap(usets, other.usets);
    stats = other.stats;
  }
  Usets(const Usets&) = delete;
  Usets& operator=(const Usets&) = delete;
  Usets& operator=(Usets&&) = delete;

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

  /** Insert a Uset into the deduplicated store.  If an equal set already
   *  exists, the existing one is returned and its refs are increased by
   *  entry->refs; the caller is responsible for freeing the rejected
   *  duplicate.  Otherwise entry is adopted and returned as-is. */
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

  /** Owning variant of add(): if the set is a duplicate the
   *  unique_ptr is left intact (caller frees); otherwise ownership
   *  is released to the store. */
  Uset* add(std::unique_ptr<Uset> entry) {
    auto p = add(entry.get());
    if (p == entry.get()) {
      entry.release();
    }
    return p;
  }

  /** Convenience: wrap a bare SetU32 in a new Uset and add it. */
  Uset* add(SetU32 set, uint32_t refs = 0) {
    return add(std::unique_ptr<Uset>(new Uset(std::move(set), refs)));
  }

  /** Find an existing Uset whose content equals entry (by hash+equality),
   *  or return nullptr.  Does not modify ref counts. */
  Uset* lookup(Uset* entry) const {
    entry->rehash();
    auto it = usets.find(entry);
    if (it != usets.end()) {
      return *it;
    } else {
      return nullptr;
    }
  }

  /** Compute the union of two Usets that share the same SetOp.
   *  May return one of the inputs if it already subsumes the other.
   *  The returned Uset has refs bumped by 1 (either via add() for a
   *  new set, or use() if an existing input was reused). */
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

  /** Merge a bare SetU32 with a Uset (Op assumed Or).  Same return
   *  and ref-count semantics as the two-Uset merge above. */
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

  /** Increment a Uset's ref count (keeps it alive across drops). */
  void use(Uset* set, uint32_t refs = 1) {
    set->refs += refs;
  }

  /** Decrement a Uset's ref count.  When refs reaches 0 the Uset is
   *  removed from the store and deleted.  Must not be called on a
   *  promoted Uset whose refs would reach 0 (asserted). */
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

  /**
   * Promote a transient set to a persistent one by assigning it a unique
   * UsetId. No-op if already promoted.
   */
  void promote(Uset* uset) {
    if (!uset->promoted()) {
      uset->id = nextId++;
      // Bump the ref count so it outlives any transient users
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

  /** The first UsetId used for promoted sets. IDs below this are
   *  leaf UnitIds; IDs at or above it are promoted set IDs. */
  UsetId getFirstId() const {
    return firstId;
  }

  /** The next UsetId that will be assigned by promote(). */
  UsetId getNextId() const {
    return nextId;
  }

  /** Merge another Usets container into this one.  The other container
   *  must be disjoint and its firstId must equal this->nextId (i.e.
   *  its ID range immediately follows ours).  Ownership of all sets
   *  is transferred; `other` is left empty. */
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
  /** Serialize all promoted sets to Elias-Fano encoding.  `max` is an
   *  exclusive upper bound on every element in every set (used as the
   *  universe size for Elias-Fano).  Returns one entry per promoted
   *  set, in UsetId order. */
  std::vector<SetExpr<MutableEliasFanoList>> toEliasFano(UsetId max);

 private:
  folly::F14FastSet<Uset*, Uset::Hash, Uset::Eq> usets;
  Stats stats;
  const UsetId firstId;
  UsetId nextId;
};

// NOLINTNEXTLINE(facebook-avoid-non-const-global-variables)
extern size_t merge_cache_size;

/**
 * Deferred, balanced merge engine for ownership-set propagation.
 *
 * Instead of merging sets one-by-one as references are discovered
 * (O(n) sequential unions), sets are queued per fact ID and merged
 * all at once in a balanced binary-reduction tree when the fact is
 * finally processed. A bounded LRU cache (split into two
 * semi-spaces) memoizes pairwise merges to avoid redundant work.
 *
 * Memory is bounded: when the queued data exceeds 1 GB the caller
 * can trigger an early flush (see completeOwnership in ownership.cpp).
 */
struct UsetsMerge {
  struct CacheKey {
    Uset* a;
    Uset* b;

    CacheKey(Uset* x, Uset* y) {
      // Normalize as order doesn't matter. (A∪B = B∪A)
      a = (x < y) ? x : y;
      b = (x < y) ? y : x;
    }

    bool operator==(const CacheKey& o) const {
      return (a == o.a && b == o.b);
    }

    size_t hash() const {
      return folly::hash::hash_combine(a->hash, b->hash);
    }
  };

  struct CacheKeyHash {
    size_t operator()(const CacheKey& k) const {
      return k.hash();
    }
  };

  /** Bounded merge-result cache with approximate LRU.
   *
   *  Two semi-spaces (new, old) are used.  Inserts go into new;
   *  when new reaches max_size, old is evicted (dropping refs on
   *  all its keys and values) and new is rotated into old.  Both
   *  semi-spaces are consulted on lookup.
   *
   *  Invariant: every Uset* stored as a key or value in either
   *  semi-space has had usets.use() called on it, so it stays alive
   *  until the cache entry is evicted. */
  struct Cache {
    using Map = folly::F14FastMap<CacheKey, Uset*, CacheKeyHash>;

    struct Stats {
      size_t hits = 0;
      size_t misses = 0;

      double hitRate() const {
        size_t total = hits + misses;
        return total > 0 ? (hits * 100.0 / total) : 0.0;
      }
    };

   private:
    Map new_cache;
    Map old_cache;
    Usets& usets;
    size_t max_size;
    Stats stats;

    /** Drop refs on all entries in old_cache and clear it. */
    void evictOldCache() {
      for (auto& [key, value] : old_cache) {
        usets.drop(key.a);
        usets.drop(key.b);
        usets.drop(value);
      }
      old_cache.clear();
    }

   public:
    Cache(size_t max_size, Usets& usets) : usets(usets), max_size(max_size) {}

    Uset* find(const CacheKey& key) {
      auto it = new_cache.find(key);
      if (it != new_cache.end()) {
        stats.hits += 1;
        return it->second;
      }
      it = old_cache.find(key);
      if (it != old_cache.end()) {
        stats.hits += 1;
        return it->second;
      }
      stats.misses += 1;
      return nullptr;
    }

    void insert(const CacheKey& key, Uset* value) {
      if (new_cache.size() >= max_size) {
        evictOldCache();
        std::swap(old_cache, new_cache);
      }
      new_cache.insert({key, value});
      usets.use(key.a);
      usets.use(key.b);
      usets.use(value);
    }

    const Stats& statistics() const {
      return stats;
    }

    void clear() {
      evictOldCache();
      std::swap(old_cache, new_cache);
      evictOldCache();
    }
  };

 private:
  Usets& usets;
  Cache cache{merge_cache_size, usets};

  // Map from fact id to the vector of corresponding usets that have to be
  // merged. A vector can contain duplicates, but it still performs better than
  // set
  folly::F14FastMap<Id, std::vector<Uset*>> refs;
  size_t usets_count = 0;

  /** Balanced binary-reduction merge of a vector of Usets.
   *  Pairs are merged bottom-up until a single result remains.
   *  Each intermediate merge goes through the cache. */
  Uset* mergeUsets(std::vector<Uset*>& setsToMerge) {
    CHECK(!setsToMerge.empty());
    if (setsToMerge.size() == 1) {
      return setsToMerge[0];
    }

    while (setsToMerge.size() > 1) {
      size_t numPairs = setsToMerge.size() / 2;
      std::vector<Uset*> result(numPairs + (setsToMerge.size() % 2), nullptr);

      for (size_t i = 0; i < numPairs; ++i) {
        result[i] = merge(setsToMerge[i * 2], setsToMerge[i * 2 + 1]);
      }

      if (setsToMerge.size() % 2 == 1) {
        result[numPairs] = setsToMerge.back();
      }

      setsToMerge.swap(result);
    }

    return setsToMerge[0];
  }

  /** Merge two Usets, consulting the cache first.  On a miss the
   *  result is inserted into the cache (bumping refs on both keys
   *  and the value).  In either case, refs on setA and setB are
   *  dropped (the cache or the caller now owns them). */
  Uset* merge(Uset* setA, Uset* setB) {
    CacheKey key(setA, setB);
    if (auto* cached = cache.find(key)) {
      usets.use(cached);
      usets.drop(setA);
      usets.drop(setB);
      return cached;
    }
    Uset* merged = usets.merge(setA, setB);
    cache.insert(key, merged);
    usets.drop(setA);
    usets.drop(setB);
    return merged;
  }

 public:
  explicit UsetsMerge(Usets& usets_) : usets(usets_) {}

  /** Queue `uset` for later merging into factId's ownership set.
   *  Bumps uset's ref count to keep it alive while queued. */
  void addUset(Id factId, Uset* uset) {
    refs[factId].push_back(uset);
    usets.use(uset);
    usets_count += 1;
  }

  /** Merge all queued sets for factId (plus `uset` if non-null)
   *  into a single result via balanced reduction, then clear the
   *  queue for factId.  The returned Uset's only ref is held by
   *  the merge cache — the caller must use() or promote() it to
   *  keep it alive across cache evictions. */
  Uset* addUsetAndMerge(Id factId, Uset* uset) {
    if (uset) {
      addUset(factId, uset);
    }

    auto factUsets = refs[factId];
    usets_count -= factUsets.size();
    auto merged = mergeUsets(factUsets);
    refs.erase(factId);

    return merged;
  }

  /** Approximate memory footprint of the pending-merge queue. */
  size_t bytes() const {
    return refs.getAllocatedMemorySize() + usets_count * sizeof(Uset*);
  }

  const Cache::Stats& statistics() const {
    return cache.statistics();
  }

  /** Snapshot of all fact IDs that currently have queued merges. */
  auto factIds() const {
    std::vector<Id> factIds;
    factIds.reserve(refs.size());
    for (const auto& [key, _] : refs) {
      factIds.push_back(key);
    }
    return factIds;
  }

  /** Whether factId has any queued (not-yet-merged) sets. */
  bool contains(Id factId) {
    return refs.contains(factId);
  }

  /** Drop all cached merge results and discard all pending queues. */
  void clear() {
    cache.clear();
    refs.clear();
    usets_count = 0;
  }
};

} // namespace rts
} // namespace glean
} // namespace facebook
