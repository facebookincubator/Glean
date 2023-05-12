/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <vector>
#include <utility>

#include "glean/rts/prim.h"
#include "glean/rts/binary.h"

namespace facebook {
namespace glean {
namespace rts {

void relToAbsByteSpans(folly::ByteRange range, binary::Output& output) {
  auto input = binary::Input(range);

  // Read the length of the given array of relative byte spans and write it to the output
  const uint64_t length = input.packed<uint64_t>();
  output.packed(length);

  // Read the packed relative byte spans and write the absolute spans
  // The first relative byte span contains the start position
  uint64_t start = 0;
  for (auto i = 0; i < length; i++) {
    auto offset = input.packed<uint64_t>();
    start += offset;
    output.packed(start);
    output.packed(input.packed<uint64_t>());
  }
}

void unpackByteSpans(folly::ByteRange range, binary::Output& output) {
  std::vector<std::pair<uint64_t, uint64_t>> result;
  {
    auto input = binary::Input(range);
    // Read the packed relative byte spans and write the absolute spans
    // The first relative byte span contains the start position
    uint64_t start = 0;
    const uint64_t size = input.packed<uint64_t>();
    for (auto i = 0; i < size; ++i) {
      const uint64_t length = input.packed<uint64_t>();
      const uint64_t group_size = input.packed<uint64_t>();
      for (auto j = 0; j < group_size; ++j) {
        start += input.packed<uint64_t>();
        result.emplace_back(start, length);
      }
    }
  }
  output.packed<uint64_t>(result.size());
  for (const auto& [start, length] : result) {
    output.packed<uint64_t>(start);
    output.packed<uint64_t>(length);
  }
}
}
}
}
