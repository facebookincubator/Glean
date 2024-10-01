/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/binary.h"
#include "glean/rts/error.h"

#include <rocksdb/db.h>

namespace facebook {
namespace glean {
namespace rocks {
namespace impl {

[[noreturn]] inline void error(const rocksdb::Status& s) {
  rts::error("rocksdb: " + s.ToString());
}

inline void check(const rocksdb::Status& status) {
  if (!status.ok()) {
    error(status);
  }
}

inline folly::ByteRange byteRange(const rocksdb::Slice& slice) {
  return folly::ByteRange(
      reinterpret_cast<const unsigned char*>(slice.data()), slice.size());
}

inline rocksdb::Slice slice(const folly::ByteRange& range) {
  return rocksdb::Slice(
      reinterpret_cast<const char*>(range.data()), range.size());
}

inline rocksdb::Slice slice(binary::Output& output) {
  return slice(output.bytes());
}

template <typename T>
inline rocksdb::Slice toSlice(const T& x) {
  return rocksdb::Slice(reinterpret_cast<const char*>(&x), sizeof(x));
}

template <typename T>
inline T fromSlice(const rocksdb::Slice& slice) {
  assert(slice.size() == sizeof(T));
  T x;
  std::memcpy(&x, slice.data(), slice.size());
  return x;
}

inline binary::Input input(const rocksdb::Slice& slice) {
  return binary::Input(byteRange(slice));
}

} // namespace impl
} // namespace rocks
} // namespace glean
} // namespace facebook
