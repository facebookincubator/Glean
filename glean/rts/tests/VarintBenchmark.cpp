/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <common/init/Init.h>
#include <folly/Benchmark.h>

#include "glean/rts/binary.h"
#include "glean/rts/tests/uniform.h"

using namespace facebook::glean;

namespace {

const size_t NATS = 10000000;

binary::Output genVarints(size_t k, size_t n, uint64_t(f)(uint64_t)) {
  binary::Output out;
  out.packed(k);
  for (uint64_t i = 0; i < n; ++i) {
    out.packed(f(i));
  }
  return out;
}

binary::Output genVarints(size_t n, uint64_t(f)(uint64_t)) {
  return genVarints(n, n, f);
}

} // namespace

#define MK_BENCHMARK(name, fn)                     \
  BENCHMARK(name##_##fn) {                         \
    folly::BenchmarkSuspender braces;              \
    auto nats = genVarints(NATS, fn);              \
    braces.dismiss();                              \
    name##_benchmark(binary::Input(nats.bytes())); \
  }

#define BENCHMARK_VARINTS(name, generate)     \
  void name##_benchmark(binary::Input value); \
  MK_BENCHMARK(name, uniform64)               \
  MK_BENCHMARK(name, uniform28)               \
  MK_BENCHMARK(name, uniform14)               \
  MK_BENCHMARK(name, uniform7)                \
  void name##_benchmark(binary::Input value)

// Copy an array of varints without going through folly's encoders and decoders.
// This is basically a lower bound on speed for the SubstituteArrayNat
// benchmark.
BENCHMARK_VARINTS(MemcpyVarints, value) {
  binary::Output out;
  const size_t count = value.packed<size_t>();
  out.packed(count);
  auto p = value.data();
  const auto e = p + value.size();
  for (size_t i = 0; i < count; ++i) {
    uint8_t c;
    size_t k = 0;
    do {
      if (p == e) {
        rts::error("truncated input");
      }
      c = *p;
      ++p;
      ++k;
    } while ((c & 0x80) != 0);
    if (k > folly::kMaxVarintLength64) {
      rts::error("invalid Nat");
    }
  }
  out.bytes(value.data(), p - value.data());
  if (p != e) {
    rts::error("input too long");
  }
}

// Decode and reencode each varint
BENCHMARK_VARINTS(CopyVarints, value) {
  binary::Output out;
  const size_t count = value.packed<size_t>();
  for (size_t i = 0; i < count; ++i) {
    out.packed(value.packed<size_t>());
  }
}

// Sum all varints - measures decoding speed
BENCHMARK_VARINTS(SumVarints, value) {
  uint64_t sum = 0;
  const size_t count = value.packed<size_t>();
  for (size_t i = 0; i < count; ++i) {
    sum += value.packed<uint64_t>();
  }
  folly::doNotOptimizeAway(sum);
}

int main(int argc, char** argv) {
  facebook::initFacebook(&argc, &argv);
  folly::runBenchmarks();
  return 0;
}
