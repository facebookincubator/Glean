/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>
#include <thread>

#include "glean/rts/cache.h"

using namespace facebook::glean::rts;

struct ConstantLookup : public Lookup {
  Id idByKey(Pid, folly::ByteRange) override {
    return Id::lowest();
  }
  Pid typeById(Id) override {
    return Pid::lowest();
  }
  bool factById(Id, std::function<void(Pid, Fact::Clause)> f) override {
    static unsigned char clause[] = "Helloworld";
    f(Pid::lowest(), {clause, 5, 5});
    return true;
  }

  Id startingId() const override {
    return Id::lowest();
  }
  Id firstFreeId() const override {
    return Id::lowest() + 1;
  }
  std::unique_ptr<FactIterator> enumerate(Id, Id) override {
    throw std::runtime_error("ConstantLookup::enumerate not implemented");
  }
  std::unique_ptr<FactIterator> enumerateBack(Id, Id) override {
    throw std::runtime_error("ConstantLookup::enumerateBack not implemented");
  }

  Interval count(Pid) const override {
    return 1;
  }

  std::unique_ptr<FactIterator>
  seek(Pid, folly::ByteRange, std::optional<Fact::Ref>) override {
    throw std::runtime_error("ConstantLookup::seek not implemented");
  }

  std::unique_ptr<FactIterator> seekWithinSection(
      Pid,
      folly::ByteRange,
      Id,
      Id,
      std::optional<Fact::Ref>) override {
    throw std::runtime_error(
        "ConstantLookup::seekWithinSection not implemented");
  }

  UsetId getOwner(Id) override {
    throw std::runtime_error("ConstantLookup::getOwner not implemented");
  }
};

// A Lookup that tracks calls and can return nothing (simulating missing facts)
struct TrackingLookup : public Lookup {
  int idByKeyCalls = 0;
  int typeByIdCalls = 0;
  int factByIdCalls = 0;
  bool returnEmpty = false;

  Id idByKey(Pid, folly::ByteRange) override {
    ++idByKeyCalls;
    return returnEmpty ? Id::invalid() : Id::lowest();
  }
  Pid typeById(Id) override {
    ++typeByIdCalls;
    return returnEmpty ? Pid::invalid() : Pid::lowest();
  }
  bool factById(Id, std::function<void(Pid, Fact::Clause)> f) override {
    ++factByIdCalls;
    if (returnEmpty) {
      return false;
    }
    static unsigned char clause[] = "Helloworld";
    f(Pid::lowest(), {clause, 5, 5});
    return true;
  }

  Id startingId() const override {
    return Id::lowest();
  }
  Id firstFreeId() const override {
    return Id::lowest() + 100;
  }
  std::unique_ptr<FactIterator> enumerate(Id, Id) override {
    return std::make_unique<EmptyIterator>();
  }
  std::unique_ptr<FactIterator> enumerateBack(Id, Id) override {
    return std::make_unique<EmptyIterator>();
  }
  Interval count(Pid) const override {
    return 1;
  }
  std::unique_ptr<FactIterator>
  seek(Pid, folly::ByteRange, std::optional<Fact::Ref>) override {
    return std::make_unique<EmptyIterator>();
  }
  std::unique_ptr<FactIterator> seekWithinSection(
      Pid,
      folly::ByteRange,
      Id,
      Id,
      std::optional<Fact::Ref>) override {
    return std::make_unique<EmptyIterator>();
  }
  UsetId getOwner(Id) override {
    return INVALID_USET;
  }
};

struct CacheTest : testing::Test {
  template <typename Base, typename F>
  void setup(Base&& b, F&& init) {
    stats = std::make_shared<LookupCache::Stats>();
    base = std::make_unique<Base>(std::forward<Base>(b));
    LookupCache::Options opts;
    opts.capacity = 5 * 1024;
    opts.touched_buffer_size = 128;
    init(opts);
    cache = std::make_unique<LookupCache>(opts, stats);
    lookup = std::make_unique<LookupCache::Anchor>(cache->anchor(base.get()));
  }

  void typeById_miss(size_t miss, size_t shards);

  TrackingLookup* setupTracking(
      size_t capacity = 10 * 1024,
      size_t shards = 1,
      bool returnEmpty = false) {
    auto tracking = std::make_unique<TrackingLookup>();
    tracking->returnEmpty = returnEmpty;
    auto* trackPtr = tracking.get();
    stats = std::make_shared<LookupCache::Stats>();
    base = std::move(tracking);
    LookupCache::Options opts;
    opts.capacity = capacity;
    opts.shards = shards;
    opts.touched_buffer_size = 128;
    cache = std::make_unique<LookupCache>(opts, stats);
    lookup = std::make_unique<LookupCache::Anchor>(cache->anchor(trackPtr));
    return trackPtr;
  }

  std::shared_ptr<LookupCache::Stats> stats;
  std::unique_ptr<Lookup> base;
  std::unique_ptr<LookupCache> cache;
  std::unique_ptr<Lookup> lookup;
};

template <typename F>
void concurrently(F&& f) {
  std::vector<std::thread> threads;

  folly::SharedMutex mutex;
  mutex.lock();
  for (size_t t = 0; t < 8; ++t) {
    threads.push_back(std::thread([&mutex, t, &f] {
      std::ignore = std::shared_lock{mutex};
      f(t);
    }));
  }
  mutex.unlock();

  for (auto& t : threads) {
    t.join();
  }
}

void CacheTest::typeById_miss(size_t miss, size_t shards) {
  setup(ConstantLookup(), [&](auto& opts) { opts.shards = shards; });

  constexpr size_t N = 10000;
  concurrently([&](size_t t) {
    for (size_t i = 0; i < N; ++i) {
      auto o = (i % 10) >= miss ? 1 : 0;
      lookup->typeById(Id::lowest() + (t * N + i - o));
    }
  });
}

TEST_F(CacheTest, typeById_miss_100_8) {
  typeById_miss(10, 8);
}
TEST_F(CacheTest, typeById_miss_90_8) {
  typeById_miss(9, 8);
}
TEST_F(CacheTest, typeById_miss_50_8) {
  typeById_miss(5, 8);
}
TEST_F(CacheTest, typeById_miss_10_8) {
  typeById_miss(1, 8);
}

TEST_F(CacheTest, typeById_miss_100_2) {
  typeById_miss(10, 2);
}
TEST_F(CacheTest, typeById_miss_90_2) {
  typeById_miss(9, 2);
}
TEST_F(CacheTest, typeById_miss_50_2) {
  typeById_miss(5, 2);
}
TEST_F(CacheTest, typeById_miss_10_2) {
  typeById_miss(1, 2);
}

TEST_F(CacheTest, typeById_miss_100_0) {
  typeById_miss(10, 0);
}
TEST_F(CacheTest, typeById_miss_90_0) {
  typeById_miss(9, 0);
}
TEST_F(CacheTest, typeById_miss_50_0) {
  typeById_miss(5, 0);
}
TEST_F(CacheTest, typeById_miss_10_0) {
  typeById_miss(1, 0);
}

TEST_F(CacheTest, upgrade) {
  setup(ConstantLookup(), [](auto&) {});

  constexpr size_t N = 10000;
  concurrently([&](size_t t) {
    for (size_t i = 0; i < N; ++i) {
      const auto id = Id::lowest() + (t * N + i);
      lookup->typeById(id);
      lookup->idByKey(
          Pid::lowest(),
          {reinterpret_cast<const unsigned char*>(&id), sizeof(id)});
      lookup->factById(id, [](auto, auto) {});
    }
  });
}

TEST_F(CacheTest, dont_replace) {
  setup(ConstantLookup(), [](auto&) {});

  constexpr size_t N = 10000;
  concurrently([&](size_t t) {
    for (size_t i = 0; i < N; ++i) {
      const auto id = Id::lowest() + (t * N + i);
      lookup->factById(id, [](auto, auto) {});
      lookup->typeById(id);
    }
  });
}

TEST_F(CacheTest, nested) {
  setup(ConstantLookup(), [](auto&) {});

  constexpr size_t N = 10000;
  lookup->factById(Id::lowest(), [&](auto, auto) {
    for (size_t i = 0; i < N; ++i) {
      lookup->typeById(Id::lowest() + i);
    }
  });
}

TEST_F(CacheTest, TypeByIdMissRecordsStats) {
  auto* trackPtr = setupTracking();
  auto type = lookup->typeById(Id::lowest());
  EXPECT_EQ(type, Pid::lowest());
  EXPECT_EQ(trackPtr->typeByIdCalls, 1);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::typeById_misses], 1);
  EXPECT_EQ(s[LookupCache::Stats::typeById_hits], 0);
}

TEST_F(CacheTest, TypeByIdHitRecordsStats) {
  auto* trackPtr = setupTracking();
  lookup->typeById(Id::lowest());
  auto type = lookup->typeById(Id::lowest());
  EXPECT_EQ(type, Pid::lowest());
  EXPECT_EQ(trackPtr->typeByIdCalls, 1);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::typeById_misses], 1);
  EXPECT_EQ(s[LookupCache::Stats::typeById_hits], 1);
}

TEST_F(CacheTest, TypeByIdFailureRecordsStats) {
  setupTracking(10 * 1024, 1, true);
  auto type = lookup->typeById(Id::lowest());
  EXPECT_EQ(type, Pid::invalid());
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::typeById_failures], 1);
}

TEST_F(CacheTest, FactByIdMissAndHitRecordsStats) {
  auto* trackPtr = setupTracking();
  Pid capturedType = Pid::invalid();
  bool found = lookup->factById(
      Id::lowest(), [&](Pid t, Fact::Clause) { capturedType = t; });
  EXPECT_TRUE(found);
  EXPECT_EQ(capturedType, Pid::lowest());
  EXPECT_EQ(trackPtr->factByIdCalls, 1);
  capturedType = Pid::invalid();
  found = lookup->factById(
      Id::lowest(), [&](Pid t, Fact::Clause) { capturedType = t; });
  EXPECT_TRUE(found);
  EXPECT_EQ(capturedType, Pid::lowest());
  EXPECT_EQ(trackPtr->factByIdCalls, 1);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::factById_misses], 1);
  EXPECT_EQ(s[LookupCache::Stats::factById_hits], 1);
}

TEST_F(CacheTest, FactByIdFailureRecordsStats) {
  setupTracking(10 * 1024, 1, true);
  bool found = lookup->factById(Id::lowest(), [](Pid, Fact::Clause) {});
  EXPECT_FALSE(found);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::factById_failures], 1);
}

TEST_F(CacheTest, IdByKeyMissAndHitRecordsStats) {
  auto* trackPtr = setupTracking();
  unsigned char keyData[] = "testkey";
  folly::ByteRange key(keyData, 7);
  auto id = lookup->idByKey(Pid::lowest(), key);
  EXPECT_EQ(id, Id::lowest());
  EXPECT_EQ(trackPtr->idByKeyCalls, 1);
  id = lookup->idByKey(Pid::lowest(), key);
  EXPECT_EQ(id, Id::lowest());
  EXPECT_EQ(trackPtr->idByKeyCalls, 1);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::idByKey_misses], 1);
  EXPECT_EQ(s[LookupCache::Stats::idByKey_hits], 1);
}

TEST_F(CacheTest, IdByKeyFailureRecordsStats) {
  setupTracking(10 * 1024, 1, true);
  unsigned char keyData[] = "testkey";
  folly::ByteRange key(keyData, 7);
  auto id = lookup->idByKey(Pid::lowest(), key);
  EXPECT_EQ(id, Id::invalid());
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::idByKey_failures], 1);
}

TEST_F(CacheTest, ReadAndResetCountersResetsOnlyCounters) {
  setupTracking();
  lookup->typeById(Id::lowest());
  lookup->typeById(Id::lowest());
  auto s1 = stats->readAndResetCounters();
  EXPECT_EQ(s1[LookupCache::Stats::typeById_misses], 1);
  EXPECT_EQ(s1[LookupCache::Stats::typeById_hits], 1);
  EXPECT_GT(s1[LookupCache::Stats::factBytes], 0);
  EXPECT_EQ(s1[LookupCache::Stats::factCount], 1);
  auto s2 = stats->read();
  EXPECT_EQ(s2[LookupCache::Stats::typeById_misses], 0);
  EXPECT_EQ(s2[LookupCache::Stats::typeById_hits], 0);
  EXPECT_GT(s2[LookupCache::Stats::factBytes], 0);
  EXPECT_EQ(s2[LookupCache::Stats::factCount], 1);
}

TEST_F(CacheTest, EvictionWhenCapacityExceeded) {
  setupTracking(256);
  for (size_t i = 0; i < 20; ++i) {
    lookup->typeById(Id::lowest() + i);
  }
  auto s = stats->read();
  EXPECT_LE(s[LookupCache::Stats::factBytes], 256);
}

TEST_F(CacheTest, UpgradeFromTypeToFullReplacesEntry) {
  auto* trackPtr = setupTracking();
  lookup->typeById(Id::lowest());
  EXPECT_EQ(trackPtr->typeByIdCalls, 1);
  bool found = lookup->factById(Id::lowest(), [](Pid, Fact::Clause) {});
  EXPECT_TRUE(found);
  EXPECT_EQ(trackPtr->factByIdCalls, 1);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::factById_deletes], 1);
  found = lookup->factById(Id::lowest(), [](Pid, Fact::Clause) {});
  EXPECT_TRUE(found);
  EXPECT_EQ(trackPtr->factByIdCalls, 1);
}

TEST_F(CacheTest, FIFOReplacementPolicySkipsTouch) {
  auto* trackPtr = setupTracking();
  auto fifoAnchor =
      cache->anchor(trackPtr, LookupCache::Anchor::ReplacementPolicy::FIFO);
  fifoAnchor.typeById(Id::lowest());
  auto type = fifoAnchor.typeById(Id::lowest());
  EXPECT_EQ(type, Pid::lowest());
  EXPECT_EQ(trackPtr->typeByIdCalls, 1);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::typeById_hits], 1);
}

TEST_F(CacheTest, DelegatesEnumerateToBase) {
  setupTracking();
  auto iter = lookup->enumerate(Id::lowest(), Id::lowest() + 10);
  ASSERT_NE(iter, nullptr);
  EXPECT_FALSE(bool(iter->get()));
  auto iterBack = lookup->enumerateBack(Id::lowest() + 10, Id::lowest());
  ASSERT_NE(iterBack, nullptr);
  EXPECT_FALSE(bool(iterBack->get()));
  unsigned char prefix[] = "test";
  auto seekIter = lookup->seek(Pid::lowest(), folly::ByteRange(prefix, 4), {});
  ASSERT_NE(seekIter, nullptr);
  EXPECT_FALSE(bool(seekIter->get()));
}

TEST_F(CacheTest, BulkStoreInsertsMultipleFacts) {
  auto* trackPtr = setupTracking();
  unsigned char data[] = "keyvalue";
  cache->withBulkStore([&](Store& store) {
    for (size_t i = 0; i < 5; ++i) {
      Fact::Ref ref;
      ref.id = Id::lowest() + i;
      ref.type = Pid::lowest();
      ref.clause = Fact::Clause::from(folly::ByteRange(data, 8), 3);
      store.insert(ref);
    }
  });
  for (size_t i = 0; i < 5; ++i) {
    bool found = lookup->factById(Id::lowest() + i, [](Pid, Fact::Clause) {});
    EXPECT_TRUE(found);
  }
  EXPECT_EQ(trackPtr->factByIdCalls, 0);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::factById_hits], 5);
}

TEST_F(CacheTest, FactTooLargeForCacheIsNotInserted) {
  auto* trackPtr = setupTracking(1);
  lookup->typeById(Id::lowest());
  lookup->typeById(Id::lowest());
  EXPECT_EQ(trackPtr->typeByIdCalls, 2);
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::typeById_hits], 0);
  EXPECT_EQ(s[LookupCache::Stats::typeById_misses], 2);
}

TEST_F(CacheTest, ZeroShardsUsesDirectTouch) {
  setupTracking(10 * 1024, 0);
  lookup->typeById(Id::lowest());
  auto type = lookup->typeById(Id::lowest());
  EXPECT_EQ(type, Pid::lowest());
  auto s = stats->read();
  EXPECT_EQ(s[LookupCache::Stats::typeById_hits], 1);
  EXPECT_EQ(s[LookupCache::Stats::typeById_misses], 1);
}
