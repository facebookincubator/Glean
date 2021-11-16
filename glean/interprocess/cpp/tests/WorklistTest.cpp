/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/interprocess/cpp/worklist.h"

#include <thread>

#include <boost/thread/barrier.hpp>
#include <glog/logging.h>

#include <folly/experimental/TestUtil.h>
#include <gtest/gtest.h>

namespace std {

// needed for CHECK_EQ
template<typename T>
std::ostream& operator<<(std::ostream& os, const std::vector<T>& xs) {
  os << '[';
  bool first = true;
  for (const auto& x : xs) {
    if (!first) {
      os << ',';
    }
    os << x;
    first = false;
  }
  return os << ']';
}

}

namespace facebook {
namespace glean {
namespace worklist {

using namespace testing;

namespace {

struct StealingFile {
  folly::test::TemporaryFile file;

  explicit StealingFile(const std::vector<Counter::Value>& values) {
    file.close();
    stealingCounterSetup(file.path().string(), values);
  }

  const std::string& path() const {
    return file.path().string();
  }
};

}

std::ostream& operator<<(std::ostream& os, Counter::Value value) {
  return os << '{' << value.start << ',' << value.end << '}';
}

std::ostream& operator<<(std::ostream& os, folly::Optional<Counter::Value> x) {
  if (x) {
    return os << x.value();
  } else {
    return os << "{}";
  }
}

void checkAll(Counter *counter, Counter::Value value) {
  while (value.start != value.end) {
    CHECK_EQ(counter->next(), value);
    ++value.start;
  }
}

TEST(WorklistTest, Singleton) {
  const auto start = 13;
  const auto end = 33;
  StealingFile file({{start,end}});

  auto counter = stealingCounter(file.path(), 0, 1);
  checkAll(counter.get(), {start,end});
  auto r = counter->next();
  CHECK(!r);
}

TEST(WorklistTest, Steal1) {
  std::vector<Counter::Value> values{{0,3},{3,7},{7,7},{7,10}};
  std::vector<size_t> visits(values.back().end, 0);

  StealingFile file(values);
  auto counter = stealingCounter(file.path(), 1, values.size());

  while (auto r = counter->next()) {
    ++visits.at(r.value().start);
  }

  CHECK_EQ(visits, std::vector<size_t>(values.back().end, 1));
}

TEST(WorklistTest, Steal2) {
  std::vector<Counter::Value> values{{0,30},{30,120},{120,121},{121,345}};
  const size_t len = values.back().end;
  auto atomic_visits = std::make_unique<std::atomic<size_t>[]>(len);
  for (auto i = 0; i < len; ++i) {
    atomic_visits[i].store(0);
  }

  StealingFile file(values);
  std::vector<std::thread> threads;
  boost::barrier barrier(values.size());
  for (auto i = 0; i < values.size(); ++i) {
    threads.push_back(std::thread([&,i] {
      auto counter = stealingCounter(file.path(), i, values.size());
      barrier.count_down_and_wait();
      while (auto r = counter->next()) {
        CHECK_LT(r.value().start, len);
        ++atomic_visits[r.value().start];
      }
    }));
  }
  for (auto& t : threads) {
    t.join();
  }

  std::vector<size_t> visits;
  for (auto i = 0; i < len; ++i) {
    visits.push_back(atomic_visits[i].load());
  }
  CHECK_EQ(visits, std::vector<size_t>(values.back().end, 1));
}

}
}
}
