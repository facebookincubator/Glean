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
  // Just grow by 2x - the first dynamic allocation will be at least 2*SMALL_CAP
  const size_t k = capacity() + std::max(capacity(), n);
  const auto wanted = folly::goodMallocSize(k);
  if (isMalloced()) {
    large.data =
        static_cast<unsigned char*>(folly::checkedRealloc(large.data, wanted));
  } else {
    const auto p = static_cast<unsigned char*>(folly::checkedMalloc(wanted));
    std::memcpy(p, data(), size());
    large.data = p;
    large.size = (size() << TAG_BITS) | LARGE_BIT | MALLOC_BIT;
  }
  large.cap = wanted;
}

hs::ffi::malloced_array<uint8_t> Output::moveBytes() {
  folly::SysBufferUniquePtr p;
  const auto len = size();
  if (isMalloced()) {
    p = folly::SysBufferUniquePtr(large.data, {});
  } else {
    p = folly::allocate_sys_buffer(len);
    std::memcpy(p.get(), data(), len);
  }
  markEmpty();
  return hs::ffi::malloced_array<uint8_t>(std::move(p), len);
}

folly::fbstring Output::moveToFbString() {
  if (isMalloced()) {
    // fbstring requires the data to be NUL-terminated. The terminator isn't
    // included in the size.
    *alloc(1) = 0;
    const auto d = mutableData();
    const auto s = size();
    const auto c = large.cap;
    markEmpty();
    return folly::fbstring(
        reinterpret_cast<char*>(d), s, c, folly::AcquireMallocatedString());
  } else {
    return folly::fbstring(data(), data() + size());
  }
}

} // namespace binary
} // namespace glean
} // namespace facebook
