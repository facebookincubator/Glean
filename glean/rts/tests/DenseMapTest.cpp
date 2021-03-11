#include <gtest/gtest.h>
#include <rapidcheck.h>
#include <rapidcheck/gtest.h>

#include <folly/container/F14Map.h>

#include "glean/rts/densemap.h"
#include "glean/rts/id.h"
#include "glean/rts/tests/arbitrary.h"

using namespace facebook::glean::rts;

namespace {

struct dense_pid_int {
  std::vector<std::pair<Pid,int>> value;

  template<typename T>
  T to() const {
    T xs;
    for(auto [pid, n] : value) {
      xs[pid] = n;
    }
    return xs;
  }

  DenseMap<Pid, int> denseMap() const {
    return to<DenseMap<Pid, int>>();
  }

  folly::F14FastMap<Pid,int,folly::Hash> hashMap() const {
    return to<folly::F14FastMap<Pid,int,folly::Hash>>();
  }
};

std::ostream& operator<<(std::ostream& out, const dense_pid_int& xs) {
  out << '[';
  bool sep = false;
  for (auto x : xs.value) {
    if (sep) {
      out << ',';
    } else {
      sep = true;
    }
    out << '{' << x.first << ',' << x.second << '}';
  }
  return out << ']';
}

}

namespace rc {

template<>
struct Arbitrary<dense_pid_int> {
  static Gen<dense_pid_int> arbitrary() {
    return gen::map(
      gen::arbitrary<std::vector<uint64_t>>(),
      [](const std::vector<uint64_t>& xs) {
        std::vector<std::pair<Pid, int>> ids;
        ids.reserve(xs.size());
        int k = 1;
        for (auto x : xs) {
          ids.push_back({Pid::lowest() + (x % 4096), k});
          ++k;
        }
        return dense_pid_int{std::move(ids)};
      }
    );
  }
};

}

namespace facebook {
namespace glean {
namespace rts {

template <class T>
struct DenseMapTest : testing::Test {};

RC_GTEST_PROP(
    DenseMapTest,
    construction,
    (const dense_pid_int& ids)) {
  auto map = ids.denseMap();
  auto check = ids.hashMap();
  for (auto [pid, n] : map) {
    if (n != 0) {
      const auto i = check.find(pid);
      RC_ASSERT(i != check.end());
      RC_ASSERT(i->second == n);
    }
  }
  for (auto [pid, n] : check) {
    RC_ASSERT(map.lookup(pid) != nullptr);
  }
}

RC_GTEST_PROP(
    DenseMapTest,
    bounds,
    (const dense_pid_int& ids)) {
  const auto map = ids.denseMap();
  if (!ids.value.empty()) {
    Pid minimum = ids.value[0].first;
    Pid maximum = ids.value[0].first;
    for (auto [id, n] : ids.value) {
      minimum = std::min(minimum, id);
      maximum = std::max(maximum, id);
    }
    // RC_ASSERT(minimum == map.low());
    RC_ASSERT(minimum == map.begin()->first);
    // RC_ASSERT(maximum+1 == map.high());
    Pid last;
    for (auto [id, n] : map) {
      last = id;
    }
    RC_ASSERT(maximum == last);
  }
}

RC_GTEST_PROP(
    DenseMapTest,
    beginEnd,
    (const dense_pid_int& ids)) {
  const auto map = ids.denseMap();
  const auto check = ids.to<std::map<Pid,int>>();
  if (ids.value.empty()) {
    RC_ASSERT(map.begin() == map.end());
    RC_ASSERT(check.empty());
  } else {
    auto mi = map.begin();
    auto ci = check.begin();

    do {
      RC_ASSERT(mi->first == ci->first && mi->second == ci->second);
      ++mi;
      ++ci;
      while (mi != map.end() && mi->second == 0) {
        ++mi;
      }
    } while (mi != map.end());
    RC_ASSERT(ci == check.end());
  }
}

RC_GTEST_PROP(
    DenseMapTest,
    lookup,
    (const dense_pid_int& ids)) {
  const auto map = ids.denseMap();
  for (auto i = map.begin(); i != map.end(); ++i) {
    if (i->second) {
      auto p = map.lookup(i->first);
      RC_ASSERT(p != nullptr);
      RC_ASSERT(p == &(i->second));
    }
  }
}

RC_GTEST_PROP(
    DenseMapTest,
    get,
    (const dense_pid_int& ids)) {
  const auto map = ids.denseMap();
  for (auto [id, n] : map) {
    RC_ASSERT(map.get(id) == n);
  }
}

RC_GTEST_PROP(
    DenseMapUnionWithTest,
    merge,
    (const dense_pid_int& left, const dense_pid_int& right)){
  auto combine = [](int& m, int n) { m = folly::Hash()(m,n); };
  auto map = left.denseMap();
  map.merge(right.denseMap(), combine);
  auto check = left.hashMap();
  for (auto [id, n] : right.hashMap()) {
    auto i = check.find(id);
    if (i != check.end()) {
      combine(i->second, n);
    } else {
      check.insert({id, n});
    }
  }

  for (auto [pid, n] : map) {
    if (n != 0) {
      const auto i = check.find(pid);
      RC_ASSERT(i != check.end());
      RC_ASSERT(i->second == n);
    }
  }
  for (auto [pid, n] : check) {
    RC_ASSERT(map.lookup(pid) != nullptr);
  }
}

}
}
}
