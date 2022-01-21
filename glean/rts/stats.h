/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <cstdlib>
#include <limits>

namespace facebook {
namespace glean {
namespace rts {

/** A Interval is a size_t interval with a lower and an upper bound */
struct Interval {
  Interval(size_t n = 0) : lo(n), hi(n) {}

  constexpr bool exact() const { return lo == hi; }
  constexpr size_t low() const { return lo; }
  constexpr size_t high() const { return hi; }

  static Interval atLeast(size_t n) {
    return Interval(n, std::numeric_limits<size_t>::max());
  }

  static Interval atMost(size_t n) {
    return Interval(0, n);
  }

  Interval& operator+=(const Interval& other) {
    lo = sat_add(lo, other.lo);
    hi = sat_add(hi, other.hi);
    return *this;
  }

  Interval asHigh() const {
    return Interval(0, hi);
  }

  Interval asLow() const {
    return Interval(lo, std::numeric_limits<size_t>::max());
  }

private:
  Interval(size_t low, size_t high) : lo(low), hi(high) {}

  static size_t sat_add(size_t x, size_t y) {
    return std::numeric_limits<size_t>::max() - x < y
      ? std::numeric_limits<size_t>::max()
      : x+y;
  }

  size_t lo;
  size_t hi;
};

inline constexpr bool operator==(const Interval& x, const Interval& y) {
  return x.low() == y.low() && x.high() == y.high();
}

inline constexpr bool operator!=(const Interval& x, const Interval& y) {
  return !(x == y);
}

inline Interval operator+(const Interval& x, const Interval& y) {
  Interval r(x);
  r += y;
  return r;
}

struct MemoryStats {

  /// Number of items
  size_t count;

  /// Memory used
  size_t memory;

  MemoryStats() : count(0), memory(0) {}
  MemoryStats(size_t n, size_t mem) : count(n), memory(mem) {}

  static MemoryStats one(size_t mem) {
    return MemoryStats(1,mem);
  }

  MemoryStats& operator+=(const MemoryStats& other) {
    count += other.count;
    memory += other.memory;
    return *this;
  }

  MemoryStats operator+(const MemoryStats& other) const {
    MemoryStats s(*this);
    s += other;
    return s;
  }

  bool operator==(const MemoryStats& other) const {
    return count == other.count && memory == other.memory;
  }

  bool operator!=(const MemoryStats& other) const {
    return !(*this == other);
  }

};

}
}
}
