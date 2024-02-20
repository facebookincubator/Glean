/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifdef OSS
#include <cpp/wrap.h> // @manual
#else
#include <common/hs/util/cpp/wrap.h>
#endif
#include "glean/interprocess/cpp/worklist.h"
#include "glean/interprocess/cpp/worklist_ffi.h"

#include <atomic>
#include <fstream>

#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>

#ifdef OSS
#include <glog/logging.h>
#else
#include "common/logging/logging.h"
#endif

// A stealing counter file has the following structure:
//
//   uint64    uint32   uint32
// +--------+--------+--------+-------+------------+------------+
// |   n    | start0 |  end0  | ..... | start(n-1) |  end(n-1)  |
// +--------+--------+--------+-------+------------+------------+
//
// That is, it starts with a 64 bit word which contains the number of workers
// and then for each worker there is a pair of (start,end) - the current index
// and the index at which to stop - which are both 32 bits.
//
// We manipulate each such pair as atomic 64-bit values which makes the logic
// quite simple. In particular, we can atomically increment the 64 bit value
// to get to the next element.
//
// With files, we don't need the size at the beginning, strictly speaking. We
// will, though, if we switch to shmem objects.

using namespace facebook::hs;
using namespace facebook::glean;

extern "C" {

struct glean_interprocess_worklist_t {
  boost::interprocess::file_mapping mapping;
  boost::interprocess::mapped_region region;
  std::atomic<uint64_t> *counters;
  size_t size;

  static worklist::Counter::Value unpack(uint64_t n) {
      return {static_cast<uint32_t>(n), static_cast<uint32_t>(n >> 32)};
  }

  static uint64_t pack(worklist::Counter::Value val) {
    return (uint64_t(val.end) << 32) | val.start;
  }

  explicit glean_interprocess_worklist_t(const char *path) {
    mapping = boost::interprocess::file_mapping(
      path, boost::interprocess::read_write);
    region = boost::interprocess::mapped_region(
      mapping,
      boost::interprocess::read_only,
      0,
      sizeof(uint64_t));
    size = *static_cast<const uint64_t *>(region.get_address());
    region = boost::interprocess::mapped_region(
      mapping,
      boost::interprocess::read_write,
      0,
      sizeof(uint64_t) * (size + 1));
    counters =
      static_cast<std::atomic<uint64_t> *>(region.get_address()) + 1;
    assert(std::atomic_is_lock_free(counters));
  }

  static void create(
      const char *path,
      const std::vector<worklist::Counter::Value>& values) {
    std::vector<uint64_t> words;
    words.reserve(values.size()+1);
    words.push_back(values.size());
    for (const auto& value : values) {
      words.push_back(pack(value));
    }
    std::ofstream stream(path, std::ios::out | std::ios::binary);
    stream.write(
      reinterpret_cast<const char *>(words.data()),
      words.size() * sizeof(uint64_t));
  }

  worklist::Counter::Value get(size_t worker) const noexcept {
    return unpack(counters[worker].load());
  }

  std::pair<worklist::Counter::Value, size_t> next(size_t worker) noexcept {
    auto victim = worker;
    auto value = unpack(counters[worker].fetch_add(1));
    if (value.empty()) {
      // NOTE: We assume that other workers will only steal from us, never give
      // us things to do.
      bool done = false;

      // The basic idea is to find the worker with the most work and steal half
      // of it. We do this in a loop until success because we want a lock-free
      // algorithm.
      while (!done) {
        worklist::Counter::Value other = value;

        // Find the worker with the most work.
        for (size_t i = (worker+1) % size; i != worker; i = (i+1) % size) {
          auto x = get(i);
          if (x.size() > other.size()) {
            other = x;
            victim = i;
          }
        }

        if (!other.empty()) {
          // If we found a worker which has some work left, steal half of it.
          uint32_t split = other.start + other.size() / 2;
          auto expected = pack(other);

          // To steal, just update their start/end pair provided it hasn't
          // changed. If it has, we'll do the whole thing again.
          if (counters[victim].compare_exchange_strong(
                expected, pack({other.start, split}))) {
            // We've stolen work, now set our start/end pair. We know it hasn't
            // changed because we have no more work left so nobody else is going
            // to update it.
            value = {split, other.end};
            counters[worker].store(pack({split+1, other.end}));
            done = true;
          }
        } else {
          // Nobody has any work left. We've already set the start/end outputs
          // when we were fetching our own counter.
          done = true;
        }
      }
    }
    return {value, victim};
  }
};

const char *glean_interprocess_worklist_create(
    const char *path,
    size_t count,
    const uint32_t *starts,
    const uint32_t *ends) {
  return ffi::wrap([=] {
    std::vector<worklist::Counter::Value> values;
    values.reserve(count);
    for (size_t i = 0; i < count; ++i) {
      values.push_back({starts[i], ends[i]});
    }
    glean_interprocess_worklist_t::create(path, values);
  });
}

const char *glean_interprocess_worklist_open(
    const char *path,
    glean_interprocess_worklist_t **worklist) {
  return ffi::wrap([=] {
    auto w = std::make_unique<glean_interprocess_worklist_t>(path);
    *worklist = w.release();
  });
}

void glean_interprocess_worklist_close(
    glean_interprocess_worklist_t *worklist) {
  ffi::free_(worklist);
}


void glean_interprocess_worklist_get(
    const glean_interprocess_worklist_t *worklist,
    size_t worker,
    uint32_t *start,
    uint32_t *end) {
  auto value = worklist->get(worker);
  *start = value.start;
  *end = value.end;
}

void glean_interprocess_worklist_next(
    glean_interprocess_worklist_t *worklist,
    size_t worker,
    uint32_t *start,
    uint32_t *end,
    size_t *victim) {
  auto r = worklist->next(worker);
  *start = r.first.start;
  *end = r.first.end;
  *victim = r.second;
}

}

namespace facebook::glean::worklist {

namespace {

struct SerialCounter : public Counter {
  uint32_t start;
  uint32_t end;

  explicit SerialCounter(uint32_t i, uint32_t e) : start(i), end(e) {}

  folly::Optional<Value> next() override {
    if (start < end) {
      auto i = start;
      ++start;
      return Value{i,end};
    } else {
      return folly::none;
    }
  }
};

struct StealingCounter : public Counter {
  glean_interprocess_worklist_t worklist;
  size_t worker;

  StealingCounter(
      const std::string& path, size_t worker_index, size_t worker_count)
      : worklist(path.c_str()) {
    CHECK_EQ(worker_count, worklist.size);
    worker = worker_index;
  }

  folly::Optional<Value> next() override {
    auto r = worklist.next(worker);
    if (r.second != worker) {
      LOG(INFO) << worker << ": stole "
        << r.first.start+1 << "-" << r.first.end << " from " << r.second;
    }
    if (!r.first.empty()) {
      return r.first;
    } else {
      return folly::none;
    }
  }
};

}

std::unique_ptr<Counter> serialCounter(size_t start, size_t end) {
  return std::make_unique<SerialCounter>(start,end);
}

std::unique_ptr<Counter> stealingCounter(
    const std::string& path,
    size_t worker_index,
    size_t worker_count) {
  return std::make_unique<StealingCounter>(path, worker_index, worker_count);
}

void stealingCounterSetup(
    const std::string& path,
    const std::vector<Counter::Value>& values) {
  glean_interprocess_worklist_t::create(path.c_str(), values);
}

}
