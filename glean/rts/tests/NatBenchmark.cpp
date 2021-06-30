// Copyright (c) Facebook, Inc. and its affiliates.

#include <iostream>

#include <common/init/Init.h>
#include <folly/Benchmark.h>

#include "glean/rts/binary.h"
#include "glean/rts/tests/uniform.h"

using namespace facebook::glean;

namespace {

const size_t NATS = 10000000;

binary::Output genNats(size_t k, size_t n, uint64_t(f)(uint64_t)) {
  binary::Output out;
  out.nat(k);
  for (uint64_t i = 0; i < n; ++i) {
    out.nat(f(i));
  }
  return out;
}

binary::Output genNats(size_t n, uint64_t(f)(uint64_t)) {
  return genNats(n, n, f);
}

} // namespace

#define MK_BENCHMARK(name, fn)                     \
  BENCHMARK(name##_##fn) {                         \
    folly::BenchmarkSuspender braces;              \
    auto nats = genNats(NATS, fn);                 \
    braces.dismiss();                              \
    name##_benchmark(binary::Input(nats.bytes())); \
  }

#define BENCHMARK_NATS(name, value)           \
  void name##_benchmark(binary::Input value); \
  MK_BENCHMARK(name, uniform64)               \
  MK_BENCHMARK(name, uniform28)               \
  MK_BENCHMARK(name, uniform14)               \
  MK_BENCHMARK(name, uniform7)                \
  void name##_benchmark(binary::Input value)

// Copy an array of nats without going through folly's encoders and decoders.
// This is basically a lower bound on speed for the SubstituteArrayNat
// benchmark.
BENCHMARK_NATS(MemcpyNatsUntrusted, value) {
  binary::Output out;
  const auto count = value.untrustedNat();
  const auto p = value.data();
  for (size_t i = 0; i < count; ++i) {
    value.skipUntrustedNat();
  }
  if (!value.empty()) {
    rts::error("input too long");
  }
  out.nat(count);
  out.put({p, value.data()});
}

BENCHMARK_NATS(MemcpyNatsTrusted, value) {
  binary::Output out;
  const auto count = value.trustedNat();
  const auto p = value.data();
  for (size_t i = 0; i < count; ++i) {
    value.skipTrustedNat();
  }
  out.nat(count);
  out.put({p, value.data()});
}

// Decode and reencode each varint
BENCHMARK_NATS(CopyNatsUntrusted, value) {
  binary::Output out;
  const auto count = value.untrustedNat();
  out.nat(count);
  for (size_t i = 0; i < count; ++i) {
    out.nat(value.untrustedNat());
  }
}

BENCHMARK_NATS(CopyNatsTrusted, value) {
  binary::Output out;
  const auto count = value.trustedNat();
  out.nat(count);
  for (size_t i = 0; i < count; ++i) {
    out.nat(value.trustedNat());
  }
}

// Sum all nats - measures decoding speed
//
// FIXME: These two benchmarks seem to be affected by whatever code runs before
// them.
BENCHMARK_NATS(SumNatsUntrusted, value) {
  uint64_t sum = 0;
  const auto count = value.untrustedNat();
  for (size_t i = 0; i < count; ++i) {
    sum += value.untrustedNat();
  }
  folly::doNotOptimizeAway(sum);
}

BENCHMARK_NATS(SumNatsTrusted, value) {
  uint64_t sum = 0;
  const auto count = value.trustedNat();
  for (size_t i = 0; i < count; ++i) {
    sum += value.trustedNat();
  }
  folly::doNotOptimizeAway(sum);
}

int main(int argc, char** argv) {
  facebook::initFacebook(&argc, &argv);
  folly::runBenchmarks();
  return 0;
}
