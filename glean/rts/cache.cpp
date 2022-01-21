/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/cache.h"

#include <folly/concurrency/CacheLocality.h>

namespace facebook {
namespace glean {
namespace rts {

std::vector<uint64_t> LookupCache::Stats::read() {
  std::vector<uint64_t> buffer(COUNT);
  for (size_t i = 0; i < COUNT; ++i) {
    buffer[i] = values[i].readFull();
  }
  return buffer;
}

std::vector<uint64_t> LookupCache::Stats::readAndResetCounters() {
  std::vector<uint64_t> buffer(COUNT);
  for (size_t i = 0; i < FIRST_NON_COUNTER; ++i) {
    buffer[i] = values[i].readFullAndReset();
  }
  for (size_t i = FIRST_NON_COUNTER; i < COUNT; ++i) {
    buffer[i] = values[i].readFull();
  }
  return buffer;
}

LookupCache::Storage::~Storage() {
  facts.erase_and_dispose(facts.begin(), facts.end(), Fact::destroy);
}

const Fact *LookupCache::Storage::push_back(Fact::unique_ptr fact) {
  facts.push_back(*fact);
  bytes += fact->size();
  ++count;
  return fact.release();
}

Fact::unique_ptr LookupCache::Storage::release(const Fact *fact) {
  assert(bytes >= fact->size());
  assert(count > 0);
  bytes -= fact->size();
  --count;
  facts.erase(facts.iterator_to(*fact));
  return Fact::unique_ptr(const_cast<Fact *>(fact));
}

void LookupCache::Storage::splice_to(
    Fact::intrusive_list& list,
    Fact::intrusive_list::iterator pos,
    LookupCache::Storage::Range range) {
  assert(bytes >= range.bytes);
  assert(count >= range.count);
  list.splice(pos, facts, range.start, range.pos);
  bytes -= range.bytes;
  count -= range.count;
}

void LookupCache::Storage::touch(const Fact *fact) {
  facts.erase(facts.iterator_to(*fact));
  facts.push_back(const_cast<Fact&>(*fact));
}

namespace {

/// We use the tag field of Fact to indicate how much data the fact has.
enum Tag : unsigned int {
  TYPE = 0, // type only (not key or value)
  KEY = 1, // type and key only (not value)
  FULL = 2 // everything
};

}

LookupCache::LookupCache(
    const Options& opts,
    std::shared_ptr<LookupCache::Stats> s)
    : options(opts)
    , touched(opts.shards)
    , stats(std::move(s)) {
  for (auto& t : touched) {
    t.facts = std::make_unique<std::atomic<const Fact *>[]>(
      options.touched_buffer_size);
  }
}

void LookupCache::clear() {
  storage.withLock([&](const auto& rstorage) {
    // NOTE: we don't seem to be able to this in the destructor because
    // something in ThreadCachedInt triggers ASAN
    stats->values[Stats::factBytes] -= rstorage.factBytes();
    stats->values[Stats::factCount] -= rstorage.factCount();
    // TODO
    // *this = LookupCache(base, capacity, stats);
  });
}

Id LookupCache::Anchor::idByKey(Pid type, folly::ByteRange key) {
  const auto cached = cache->index.withRLockPtr([&](auto rindex) {
    const auto i = rindex->keys.find(FactByKey::value_type{type, key});
    if (i != rindex->keys.end()) {
      const auto fact = *i;
      const auto id = fact->id();
      cache->touch(std::move(rindex), fact);
      return id;
    } else {
      return Id::invalid();
    }
  });

  if (cached) {
    ++cache->stats->values[Stats::idByKey_hits];
    return cached;
  }

  if (const auto id = base->idByKey(type, key)) {
    ++cache->stats->values[Stats::idByKey_misses];
    cache->insert(Fact::create({id, type, Fact::Clause::fromKey(key)}, KEY));
    return id;
  } else {
    ++cache->stats->values[Stats::idByKey_failures];
    return Id::invalid();
  }
}

Pid LookupCache::Anchor::typeById(Id id) {
  const auto cached = cache->index.withRLockPtr([&](auto rindex) {
    const auto i = rindex->ids.find(id);
    if (i != rindex->ids.end()) {
      const auto fact = *i;
      const auto ty = fact->type();
      cache->touch(std::move(rindex), fact);
      return ty;
    } else {
      return Pid::invalid();
    }
  });

  if (cached) {
    ++cache->stats->values[Stats::typeById_hits];
    return cached;
  } else if(auto type = base->typeById(id)) {
    ++cache->stats->values[Stats::typeById_misses];
    cache->insert(Fact::create({id, type, {}}, TYPE));
    return type;
  } else {
    ++cache->stats->values[Stats::typeById_failures];
    return Pid::invalid();
  }
}

bool LookupCache::Anchor::factById(
    Id id,
    std::function<void(Pid, Fact::Clause)> f) {
  const auto cached = cache->index.withRLockPtr([&](auto rindex) {
    const auto i = rindex->ids.find(id);
    if (i != rindex->ids.end() && (*i)->tag() == FULL) {
      const auto fact = *i;
      // It is imporant to call f after we (i.e., touch) have released the read
      // lock (since f might use the cache). However, fact needs to exist until
      // after we've called f. Grabbing a read lock for delete_lock makes sure
      // no facts will be deleted until we're done.
      //
      // We might consider finer-grained locking if this becomes an issue.
      folly::SharedMutex::ReadHolder dont_delete(rindex->delete_lock);
      cache->touch(std::move(rindex), fact);
      f(fact->type(), fact->clause());
      return true;
    } else {
      return false;
    }
  });

  if (cached) {
    ++cache->stats->values[Stats::factById_hits];
    return true;
  }

  Fact::unique_ptr fact;
  base->factById(id, [&](auto type, auto clause) {
    fact = Fact::create({id, type, clause}, FULL);
  });
  if (fact) {
    ++cache->stats->values[Stats::factById_misses];
    f(fact->type(), fact->clause());
    // FIXME: probably do this before f
    cache->insert(std::move(fact));
    return true;
  } else {
    ++cache->stats->values[Stats::factById_failures];
    return false;
  }
}

std::unique_ptr<FactIterator> LookupCache::Anchor::enumerate(Id from, Id upto) {
  return base->enumerate(from, upto);
}

std::unique_ptr<FactIterator> LookupCache::Anchor::enumerateBack(
    Id from, Id downto) {
  return base->enumerateBack(from, downto);
}

std::unique_ptr<FactIterator>LookupCache::Anchor::seek(
    Pid type,
    folly::ByteRange start,
    size_t prefix_size) {
  return base->seek(type, start, prefix_size);
}

void LookupCache::insert(Fact::unique_ptr owned) {
  folly::SharedMutex::WriteHolder delete_write(nullptr);
  Fact::intrusive_list dead;
  performUpdate([&](Index& index, Storage& storage) {
    insertOne(index, storage, std::move(owned), dead);
    if (!dead.empty()) {
      delete_write = folly::SharedMutex::WriteHolder(index.delete_lock);
    }
  });
  // Perform the actual deletions after we've released all locks on the index
  // and storage.
  if (!dead.empty()) {
    dead.erase_and_dispose(dead.begin(), dead.end(), Fact::destroy);
  }
}

void LookupCache::insertOne(
    Index& index,
    Storage& storage,
    Fact::unique_ptr owned,
    Fact::intrusive_list& dead) {
  const auto size = owned->size();

  if (size > options.capacity) {
    return;
  }

  // check if we already have a fact with this id in the cache
  const Fact *existing = nullptr;
  {
    auto o = index.ids.find(owned->id());
    if (o != index.ids.end()) {
      existing = *o;
    }
  }

  if (existing && existing->tag() >= owned->tag()) {
    // the cached fact has same or more data than we are trying to insert
    return;
  }

  if (existing || storage.factBytes() + size > options.capacity) {
    // Do deferred LRU operations if we're going to evict or replace. We need
    // to do this even when replacing because the hit buffers might reference
    // the fact we're going to delete.
    //
    // NOTE: drain is "lossy" but in this case, we're running under a
    // write lock for the index so there will be no concurrent drainers
    // and we'll have had a memory barrier before - so drain isn't
    // actually lossy here.
    //
    // This doesn't preserve LRU order as we process shards one by one,
    // i.e., hits within one shard a processes in chronological order
    // but we lose ordering across shards. It possible to fix this at the cost
    // of some performance but probably not worth doing.
    for (auto& t : touched) {
      drain(storage, t);
    }

    if (existing) {
      if (owned->tag() == KEY) {
        ++stats->values[Stats::idByKey_deletes];
      } else {
        ++stats->values[Stats::factById_deletes];
      }
      deleteFromIndex(index, existing);
      // TODO: defer this, see comments in evict
      auto fact = storage.release(existing);
      dead.push_back(*fact);
      // dead assumed ownership of the fact
      (void)fact.release();
    }

    if (storage.factBytes() + size > options.capacity) {
      // shrink cache to ~90% of capacity
      const auto wanted =
        options.capacity * 0.9 > size ? options.capacity * 0.9 - size : 0;
      evict(index, storage, wanted, dead);
    }
  }

  const auto fact = storage.push_back(std::move(owned));

  // For 'insert' (but not 'BulkStorage') we could unlock the storage here. It
  // doesn't matter, though, since nothing will really use it without getting a
  // lock for index first.

  index.ids.insert(fact);
  if (fact->tag() != TYPE) {
    index.keys.insert(fact);
  }
}

void LookupCache::deleteFromIndex(Index& index, const Fact *fact) {
  index.ids.erase(fact);
  if (fact->tag() > TYPE) {
    index.keys.erase(fact);
  }
}

void LookupCache::evict(
    LookupCache::Index& index,
    LookupCache::Storage& storage,
    size_t target,
    Fact::intrusive_list& dead) {
  auto range = storage.start();
  while (storage.factBytes() - range.factBytes() > target && !range.empty()) {
    deleteFromIndex(index, range.next());
  }
  storage.splice_to(dead, dead.end(), range);
}

void LookupCache::touch(
    LookupCache::SyncIndex::RLockedPtr rindex,
    const Fact *fact) {
  if (options.shards > 0) {
    auto& t = touched[folly::AccessSpreader<>::cachedCurrent(options.shards)];

    // This is intentionally very lossy
    auto k = t.next.load(std::memory_order_acquire);
    if (k < options.touched_buffer_size) {
      // yes, we might overwrite a previous store
      t.facts[k].store(fact, std::memory_order_release);
      // yes, there might have been other stores here in the meantime
      t.next.store(k+1, std::memory_order_release);
    } else {
      rindex.unlock();
      if (auto wstorage = storage.tryLock()) {
        // Only drain our shard - as in 'insert', this loses LRU ordering across
        // shards.
        drain(*wstorage, t);
      }
    }
  } else {
    storage.withLock([&](auto& wstorage) {
      wstorage.touch(fact);
    });
  }
}

void LookupCache::drain(Storage& storage, Touched& t) {
  // There might be calls to touch (but not to drain!) running concurrently to
  // this. It's ok to lose those. The only invariant we rely on is that the
  // Touched buffer has been filled up to n.
  const auto n = t.next.load(std::memory_order_acquire);
  for (size_t i = 0; i < n; ++i) {
    storage.touch(t.facts[i].load(std::memory_order_acquire));
  }
  // yes, we might lose recorded hits here
  t.next.store(0, std::memory_order_release);
}

void LookupCache::Inserter::insert(Fact::Ref fact) {
  Fact::intrusive_list dead;
  cache.insertOne(
    index,
    storage,
    Fact::create(fact, FULL),
    dead);
  if (!dead.empty()) {
    folly::SharedMutex::WriteHolder delete_write(index.delete_lock);
    dead.erase_and_dispose(dead.begin(), dead.end(), Fact::destroy);
  }
}

}
}
}
