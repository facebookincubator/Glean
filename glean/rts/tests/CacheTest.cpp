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

  std::unique_ptr<FactIterator> seek(Pid, folly::ByteRange, size_t) override {
    throw std::runtime_error("ConstantLookup::seek not implemented");
  }

  std::unique_ptr<FactIterator>
  seekWithinSection(Pid, folly::ByteRange, size_t, Id, Id) override {
    throw std::runtime_error(
        "ConstantLookup::seekWithinSection not implemented");
  }

  UsetId getOwner(Id) override {
    throw std::runtime_error("ConstantLookup::getOwner not implemented");
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
