/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/binary.h"

namespace facebook {
namespace glean {
namespace binary {

std::vector<unsigned char> lexicographicallyNext(folly::ByteRange range) {
  const auto end = std::make_reverse_iterator(range.begin());
  auto pos = std::make_reverse_iterator(range.end());
  while (pos != end && *pos == 0xFF) {
    ++pos;
  }
  std::vector<unsigned char> bytes(range.begin(), pos.base());
  if (!bytes.empty()) {
    ++bytes.back();
  }
  return bytes;
}

void Output::realloc(size_t n) {
  // Just grow by 2, but start with at least 64 - we typically have very small
  // or very large buffers.
  const auto wanted =
    std::max(state.cap + std::max(state.cap, n), size_t(64));
  const auto new_cap = folly::goodMallocSize(wanted);
  state.data = static_cast<unsigned char *>(std::realloc(state.data, new_cap));
  state.cap = new_cap;
}

folly::fbstring Output::moveToFbString() {
  // fbstring requires the data to be NUL-terminated. The terminator isn't
  // included in the size.
  *alloc(1) = 0;
  State s;
  std::swap(state,s);
  return folly::fbstring(
   reinterpret_cast<char *>(s.data),
   s.len,
   s.cap,
   folly::AcquireMallocatedString());
}

}
}
}
