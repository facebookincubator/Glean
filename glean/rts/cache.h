/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <vector>
#include <folly/SharedMutex.h>
#include <folly/Synchronized.h>
#include <folly/ThreadCachedInt.h>

#include "glean/rts/factset.h"
#include "glean/rts/lookup.h"

namespace facebook {
namespace glean {
namespace rts {

/// An LRU fact cache for speeding up point lookups (and only those) during
/// writes. It only loads as much data as has been requested by the client which
/// means that for any given fact, it can store either only its type (via
/// typeById), its type and key (via idByKey) or everything (via factById).
///
/// The cache itself doesn't implement Lookup. To actually look things up, it
/// must be pair with a base Lookup via 'anchor'. This allows the cache to not
/// be tied to the lifetime of one specific Lookup instance. It is the user's
/// responsibility that the Lookups it is paired with are morally the same
/// throughout its lifetime (such as the same database opened multiple times).
///
/// The cache can be used concurrently by multiple threads and is supposed to
/// scale at least a little bit for hits. The hash maps are guarded by
/// a read-write, write-priority lock and the LRU list by a mutex. Crucially,
/// we don't update the LRU list on every access. Rather, we record all
/// accesses in append-only, lossy buffers sharded by threads (cf. the Touched
/// structure below). These get drained (in a not-really-LRU order) when we
/// need to evict or when they become full. This (hopefully) minimises
/// contention for read-only accesses. This is quite similar to what, e.g.,
/// Java's Caffeine library does.
///
/// TODO: Switch to a variant of the Clock algorithm or something similar which
///       lets us avoid a doubly linked list (which costs 2 pointers per fact).
/// TODO: We probably want to remove facts which haven't been used for some time
///       even if the cache isn't full.
/// TODO: Say we cache the result of idByKey but then only ever do typeById
///       afterwards. This will keep the cache entry alive and we will never
///       get rid of the key even though it's not needed. We'll see how much
///       of a problem this is.
/// TODO: We want to share one capacity between all db caches.
class LookupCache {
public:

  // A Stats object can be shared between different caches which will accumulate
  // their statistics into it.
  //
  // xxx_hits = cache hits by lookup function xxx
  // xxx_misses = true misses (i.e., fact isn't cached but exists in the Lookup)
  // xxx_failures = fact doesn't exist in the Lookup
  // xxx_deletes = fact existed in the cache but had too little information
  //               (e.g., only the type but xxx also needed the key)
  //
  // NOTE: This must be kept in sync with the Counter type in
  // Glean.RTS.Foreign.LookupCache
  struct Stats {
    enum Stat : size_t {
      FIRST_COUNTER,

      idByKey_hits = FIRST_COUNTER,
      idByKey_misses,
      idByKey_failures,
      idByKey_deletes,
      typeById_hits,
      typeById_misses,
      typeById_failures,
      factById_hits,
      factById_misses,
      factById_failures,
      factById_deletes,

      FIRST_NON_COUNTER,

      factBytes = FIRST_NON_COUNTER,
      factCount,

      COUNT
    };

    folly::ThreadCachedInt<uint64_t> values[COUNT];

    /// Obtain an approximate snapshot of the values in 'Stats'.
    std::vector<uint64_t> read();

    /// Obtain an approximate snapshot of the values in 'Stats' and reset all
    /// counters to 0 (but not sums like total cache size).
    std::vector<uint64_t> readAndResetCounters();
  };

  struct Options {
    /// Max capacity
    size_t capacity;

    /// Number of shards for Touched buffer - should typically be number of
    /// threads.
    size_t shards = std::thread::hardware_concurrency();

    /// How many hits to record locally before draining them to global buffer.
    size_t touched_buffer_size = 64 * 1024;
  };

  LookupCache(const Options& opts, std::shared_ptr<Stats> s);

  LookupCache(const LookupCache&) = delete;
  LookupCache(LookupCache&&) = delete;
  LookupCache& operator=(const LookupCache&) = delete;
  LookupCache& operator=(LookupCache&&) = delete;

  // Clear statistics
  void clear();

  // LookupCache paired with a base Lookup for looking up facts
  struct Anchor : Lookup {
    enum class ReplacementPolicy {
      LRU, // evict least-recently-used entries
      FIFO // evict the oldest entries. Faster with many concurrent clients.
    };
    Anchor(
        Lookup* b,
        LookupCache* c,
        ReplacementPolicy replacementPolicy = ReplacementPolicy::LRU)
        : base(b), cache(c), replacementPolicy(replacementPolicy) {}

    Id idByKey(Pid type, folly::ByteRange key) override;
    Pid typeById(Id id) override;
    bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override;

    virtual Id startingId() const override { return base->startingId(); }
    virtual Id firstFreeId() const override { return base->firstFreeId(); }

    std::unique_ptr<FactIterator> enumerate(Id from, Id upto) override;
    std::unique_ptr<FactIterator> enumerateBack(Id from, Id downto) override;

    Interval count(Pid pid) const override { return base->count(pid); }

    std::unique_ptr<FactIterator>
      seek(Pid type, folly::ByteRange start, size_t prefix_size) override;

    std::unique_ptr<FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange start,
      size_t prefix_size,
      Id from,
      Id to) override;

    UsetId getOwner(Id id) override { return base->getOwner(id); }

    Lookup *base;
    LookupCache *cache;
    const ReplacementPolicy replacementPolicy;
  };

  friend struct Anchor;

  Anchor anchor(
      Lookup* base,
      Anchor::ReplacementPolicy replacementPolicy =
          Anchor::ReplacementPolicy::LRU) {
    return Anchor(base, this, replacementPolicy);
  }

  // Execute a bulk loading function which takes a 'Store&' argument. The cache
  // will be locked during the execution so any lookups will cause a deadlock.
  template<typename F> inline void withBulkStore(F&& f) {
    performUpdate([&](auto& windex, auto& wstorage) {
      Inserter inserter(*this, windex, wstorage);
      f(static_cast<Store&>(inserter));
    });
  }

  ~LookupCache();

private:
  Options options;

  struct Index {
    FastSetBy<const Fact *, FactById> ids; // id -> fact
    FastSetBy<const Fact *, FactByKey> keys; // (type,key) -> fact

    // Only delete (as in free) facts while holding this lock exclusively. By
    // construction, it can only be acquired when holding a lock for the Index.
    mutable folly::SharedMutex delete_lock;
  };
  using SyncIndex = folly::Synchronized<Index, folly::SharedMutex>;
  SyncIndex index; // index guarded by r/w lock

  class Storage {
  public:
    Storage() {}
    Storage(Storage&&) = default;
    Storage(const Storage&) = delete;
    Storage& operator=(Storage&&) = default;
    Storage& operator=(const Storage&) = delete;
    ~Storage();

    using iterator = Fact::intrusive_list::iterator;

    iterator begin() { return facts.begin(); }
    iterator end() { return facts.end(); }

    size_t factBytes() const { return bytes; }
    size_t factCount() const { return count; }

    const Fact *push_back(Fact::unique_ptr fact);
    Fact::unique_ptr release(const Fact *fact);
    void touch(const Fact *fact);

    class Range {
    public:
      const Fact * FOLLY_NULLABLE next() {
        if (pos != finish) {
          const auto fact = &*pos;
          bytes += fact->size();
          ++count;
          ++pos;
          return fact;
        } else {
          return nullptr;
        }
      }

      bool empty() const {
        return pos == finish;
      }

      size_t factBytes() const { return bytes; }
      size_t factCount() const { return count; }

    private:
      Range(iterator s, iterator e)
        : start(s)
        , pos(s)
        , finish(e)
        , bytes(0)
        , count(0)
        {}

      friend class Storage;

      iterator start;
      iterator pos;
      iterator finish;
      size_t bytes;
      size_t count;
    };

    Range start() {
      return Range(begin(), end());
    }

    void splice_to(
      Fact::intrusive_list& list,
      Fact::intrusive_list::iterator pos,
      Range range);

  private:
    Fact::intrusive_list facts;
    size_t bytes = 0;
    size_t count = 0;
  };
  using SyncStorage = folly::Synchronized<Storage, std::mutex>;
  SyncStorage storage; // fact storage guarded by mutex

  // NOTE: locking order is always Index then Storage, never the other way
  // round.

  // Container for recording cache hits. This is intentionally very lossy as it
  // can be written to by multiple threads without synchronisation.
  struct alignas(folly::hardware_destructive_interference_size) Touched {
    // Make things atomic for the unlikely case we're ever going to be running
    // on something other than x86_64.
    std::unique_ptr<std::atomic<const Fact *>[]> facts;
    std::atomic<size_t> next { 0 };
  };
  std::vector<Touched> touched; // cache hits sharded by thread

  std::shared_ptr<Stats> stats; // statistics

  // Execute a function which updates the cache and updates statistics.
  template<typename F> inline void performUpdate(F&& f) {
    // We actually rely on wrap-around for underflow for these two. Initialise
    // to make Infer happy.
    uint64_t bytes_diff = 0;
    uint64_t count_diff = 0;

    index.withWLock([&](auto& windex) {
      storage.withLock([&](auto& wstorage) {
        const auto initial_bytes = wstorage.factBytes();
        const auto initial_count = wstorage.factCount();
        f(windex, wstorage);
        bytes_diff = wstorage.factBytes() - initial_bytes;
        count_diff = wstorage.factCount() - initial_count;
      });
    });

    stats->values[Stats::factBytes] += bytes_diff;
    stats->values[Stats::factCount] += count_diff;
  }

  // Insert a new fact into the cache.
  void insert(Fact::unique_ptr);

  // Insert a new fact into the locked cache and move all evicted facts into
  // 'dead'.
  void insertOne(
    Index& index,
    Storage& storage,
    Fact::unique_ptr,
    Fact::intrusive_list& dead);

  // Delete a fact from the locked index but not from the storage
  static void deleteFromIndex(Index& index, const Fact *fact);

  // Record a hit on a particular fact. Note that the ownership of the read
  // lock is passed to touch which will release it.
  void touch(SyncIndex::RLockedPtr, const Fact *);

  // Evict facts from the cache until we've freed up at least target bytes and
  // move evicted facts into 'dead'.
  static void evict(
    Index& index,
    Storage& storage,
    size_t target,
    Fact::intrusive_list& dead);

  // Drain hits recorded in 'touched', updating the storage as necessary.
  static void drain(Storage& storage, Touched& touched);

  struct Inserter : Store {
    LookupCache& cache;
    Index& index;
    Storage& storage;

    Inserter(LookupCache& c, Index& i, Storage& s)
      : cache(c), index(i), storage(s) {}

    void insert(Fact::Ref fact) override;
  };
};

}
}
}
