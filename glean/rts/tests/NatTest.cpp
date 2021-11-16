/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>
#include <rapidcheck.h>
#include <rapidcheck/gtest.h>

#include "glean/rts/binary.h"
#include "glean/rts/nat.h"

using namespace facebook::glean::rts;

template <class T>
struct NatTest : testing::Test {};

RC_GTEST_PROP(NatTest, roundtripUntrusted, (uint64_t val)) {
  facebook::glean::binary::Output out;
  out.nat(val);
  RC_ASSERT(out.bytes().size() != 0);
  facebook::glean::binary::Input inp(out.bytes());
  auto x = inp.untrustedNat();
  RC_ASSERT(x == val);
  RC_ASSERT(inp.bytes().empty());
}

RC_GTEST_PROP(NatTest, roundtripTrusted, (uint64_t val)) {
  facebook::glean::binary::Output out;
  out.nat(val);
  RC_ASSERT(out.bytes().size() != 0);
  facebook::glean::binary::Input inp(out.bytes());
  auto x = inp.trustedNat();
  RC_ASSERT(x == val);
  RC_ASSERT(inp.bytes().empty());
}

RC_GTEST_PROP(NatTest, skipUntrusted, (uint64_t val)) {
  facebook::glean::binary::Output out;
  out.nat(val);
  facebook::glean::binary::Input input(out.bytes());
  input.skipUntrustedNat();
  RC_ASSERT(input.empty());
}

RC_GTEST_PROP(NatTest, skipTrusted, (uint64_t val)) {
  facebook::glean::binary::Output out;
  out.nat(val);
  facebook::glean::binary::Input input(out.bytes());
  input.skipTrustedNat();
  RC_ASSERT(input.empty());
}

RC_GTEST_PROP(NatTest, skipUnderflow, (uint64_t val)) {
  facebook::glean::binary::Output out;
  out.nat(val);
  facebook::glean::binary::Input input(
      out.bytes().begin(), out.bytes().end() - 1);
  RC_ASSERT_THROWS(input.skipUntrustedNat());
}

RC_GTEST_PROP(NatTest, readUnderflow, (uint64_t val)) {
  facebook::glean::binary::Output out;
  out.nat(val);
  facebook::glean::binary::Input input(
      out.bytes().begin(), out.bytes().end() - 1);
  RC_ASSERT_THROWS(input.untrustedNat());
}

namespace {

const std::vector<std::array<unsigned char, 9>> invalids = {
    {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF},
    {0xFF, 0xFE, 0xFD, 0xFB, 0xF7, 0xEF, 0xDF, 0xBF, 0x80},
    {0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
};

} // namespace

TEST(NatTest, skipInvalid) {
  for (const auto& bytes : invalids) {
    facebook::glean::binary::Input input(bytes.begin(), bytes.end());
    ASSERT_ANY_THROW(input.skipUntrustedNat());
  }
}

TEST(NatTest, readInvalid) {
  for (const auto& bytes : invalids) {
    facebook::glean::binary::Input input(bytes.begin(), bytes.end());
    ASSERT_ANY_THROW(input.untrustedNat());
  }
}

namespace {

enum Ord { LT, EQ, GT };

template <typename T, typename U>
Ord compare(T&& x, U&& y) {
  if (x < y) {
    return LT;
  } else if (y < x) {
    return GT;
  } else {
    return EQ;
  }
}

} // namespace

// Check that packed numbers compare lexicographically
RC_GTEST_PROP(NatTest, memcmp, (uint64_t val1, uint64_t val2)) {
  facebook::glean::binary::Output out1;
  out1.nat(val1);
  facebook::glean::binary::Output out2;
  out2.nat(val2);
  auto k = compare(
      std::memcmp(out1.data(), out2.data(), std::min(out1.size(), out2.size())),
      0);
  if (k == EQ) {
    k = compare(out1.size(), out2.size());
  }
  RC_ASSERT(k == compare(val1, val2));
}
