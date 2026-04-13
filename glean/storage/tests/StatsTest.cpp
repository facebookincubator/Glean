/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>

#include <thread>
#include <vector>

#include "glean/storage/stats.h"

namespace facebook::glean::db {

namespace {

// Helper to build a PredicateStats with given pid->MemoryStats entries
PredicateStats makeStats(
    const std::vector<std::pair<uint64_t, rts::MemoryStats>>& entries) {
  PredicateStats stats;
  for (auto& [pidVal, memStats] : entries) {
    auto pid = rts::Pid::fromWord(pidVal);
    stats[pid] = memStats;
  }
  return stats;
}

} // namespace

// ============================================================
// Construction and Initial State
// ============================================================

TEST(AtomicPredicateStatsTest, DefaultConstructedHasEmptyStats) {
  AtomicPredicateStats aps;

  auto stats = aps.get();
  EXPECT_TRUE(stats.empty());
  EXPECT_EQ(stats.size(), 0);

  const auto& unprotectedStats = aps.unprotected();
  EXPECT_TRUE(unprotectedStats.empty());
}

// ============================================================
// set() and get() round-trip
// ============================================================

TEST(AtomicPredicateStatsTest, SetAndGetRoundTrip) {
  AtomicPredicateStats aps;

  auto input = makeStats({{1024, rts::MemoryStats(10, 200)}});
  aps.set(input);

  auto result = aps.get();
  ASSERT_EQ(result.size(), 1);

  auto pid = rts::Pid::fromWord(1024);
  auto val = result.get(pid);
  ASSERT_TRUE(val.hasValue());
  EXPECT_EQ(val->count, 10);
  EXPECT_EQ(val->memory, 200);
}

TEST(AtomicPredicateStatsTest, SetReplacesExistingStats) {
  AtomicPredicateStats aps;

  aps.set(makeStats({{1024, rts::MemoryStats(5, 100)}}));
  aps.set(makeStats({{1025, rts::MemoryStats(20, 400)}}));

  auto result = aps.get();
  // The second set should have completely replaced the first
  auto pid1024 = rts::Pid::fromWord(1024);
  auto pid1025 = rts::Pid::fromWord(1025);

  EXPECT_FALSE(result.get(pid1024).hasValue());

  auto val = result.get(pid1025);
  ASSERT_TRUE(val.hasValue());
  EXPECT_EQ(val->count, 20);
  EXPECT_EQ(val->memory, 400);
}

TEST(AtomicPredicateStatsTest, SetWithMultiplePredicates) {
  AtomicPredicateStats aps;

  auto input = makeStats({
      {1024, rts::MemoryStats(10, 200)},
      {1025, rts::MemoryStats(30, 600)},
      {1030, rts::MemoryStats(50, 1000)},
  });
  aps.set(input);

  auto result = aps.get();
  EXPECT_EQ(result.size(), 3);

  auto val1025 = result.get(rts::Pid::fromWord(1025));
  ASSERT_TRUE(val1025.hasValue());
  EXPECT_EQ(val1025->count, 30);
  EXPECT_EQ(val1025->memory, 600);

  auto val1030 = result.get(rts::Pid::fromWord(1030));
  ASSERT_TRUE(val1030.hasValue());
  EXPECT_EQ(val1030->count, 50);
  EXPECT_EQ(val1030->memory, 1000);
}

// ============================================================
// unprotected() access
// ============================================================

TEST(AtomicPredicateStatsTest, UnprotectedReflectsSetData) {
  AtomicPredicateStats aps;

  aps.set(makeStats({{1024, rts::MemoryStats(7, 140)}}));

  const auto& stats = aps.unprotected();
  ASSERT_EQ(stats.size(), 1);

  auto val = stats.get(rts::Pid::fromWord(1024));
  ASSERT_TRUE(val.hasValue());
  EXPECT_EQ(val->count, 7);
  EXPECT_EQ(val->memory, 140);
}

// ============================================================
// count() method
// ============================================================

TEST(AtomicPredicateStatsTest, CountReturnsCorrectValueForExistingPid) {
  AtomicPredicateStats aps;

  aps.set(makeStats({{1024, rts::MemoryStats(42, 800)}}));

  EXPECT_EQ(aps.count(rts::Pid::fromWord(1024)), 42);
}

TEST(AtomicPredicateStatsTest, CountReturnsZeroForNonExistentPid) {
  AtomicPredicateStats aps;

  aps.set(makeStats({{1024, rts::MemoryStats(42, 800)}}));

  // Pid 9999 was never set
  EXPECT_EQ(aps.count(rts::Pid::fromWord(9999)), 0);
}

TEST(AtomicPredicateStatsTest, CountReturnsZeroOnEmptyStats) {
  AtomicPredicateStats aps;

  EXPECT_EQ(aps.count(rts::Pid::fromWord(1024)), 0);
}

// ============================================================
// Multiple sequential set() calls (exercises retire path)
// ============================================================

TEST(AtomicPredicateStatsTest, MultipleSequentialSetsRetireOldHolders) {
  AtomicPredicateStats aps;

  // Each set() should retire the previous holder
  for (uint64_t i = 0; i < 10; ++i) {
    aps.set(makeStats({{1024 + i, rts::MemoryStats(i, i * 100)}}));
  }

  // Only the last set should be visible
  auto result = aps.get();
  EXPECT_EQ(result.size(), 1);

  auto val = result.get(rts::Pid::fromWord(1033));
  ASSERT_TRUE(val.hasValue());
  EXPECT_EQ(val->count, 9);
  EXPECT_EQ(val->memory, 900);
}

// ============================================================
// Concurrent reads during write (thread safety)
// ============================================================

TEST(AtomicPredicateStatsTest, ConcurrentReadsWhileWriting) {
  AtomicPredicateStats aps;

  // Set initial data
  aps.set(makeStats({{1024, rts::MemoryStats(1, 100)}}));

  constexpr int kNumReaders = 4;
  constexpr int kNumIterations = 1000;
  std::atomic<bool> stop{false};
  std::vector<std::thread> readers;
  readers.reserve(kNumReaders);

  // Launch reader threads that continuously call get() and count()
  for (int i = 0; i < kNumReaders; ++i) {
    readers.emplace_back([&aps, &stop]() {
      while (!stop.load(std::memory_order_relaxed)) {
        auto stats = aps.get();
        // Stats should always contain exactly one predicate (pid 1024)
        EXPECT_EQ(stats.size(), 1);

        auto cnt = aps.count(rts::Pid::fromWord(1024));
        // Count should always be a value we wrote (1..kNumIterations)
        EXPECT_GE(cnt, 1);
        EXPECT_LE(cnt, static_cast<size_t>(kNumIterations));
      }
    });
  }

  // Writer thread: repeatedly set new stats
  for (int i = 0; i < kNumIterations; ++i) {
    aps.set(makeStats(
        {{1024, rts::MemoryStats(static_cast<size_t>(i + 1), (i + 1) * 100)}}));
  }

  stop.store(true, std::memory_order_relaxed);
  for (auto& t : readers) {
    t.join();
  }

  // After all writes, the final value should be the last one written
  EXPECT_EQ(aps.count(rts::Pid::fromWord(1024)), kNumIterations);
}

// ============================================================
// set() with empty stats
// ============================================================

TEST(AtomicPredicateStatsTest, SetEmptyStatsAfterNonEmpty) {
  AtomicPredicateStats aps;

  aps.set(makeStats({{1024, rts::MemoryStats(10, 200)}}));
  EXPECT_EQ(aps.count(rts::Pid::fromWord(1024)), 10);

  // Replace with empty stats
  aps.set(PredicateStats{});

  auto result = aps.get();
  EXPECT_TRUE(result.empty());
  EXPECT_EQ(aps.count(rts::Pid::fromWord(1024)), 0);
}

} // namespace facebook::glean::db
