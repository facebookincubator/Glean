/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/FBString.h>
#include <folly/Memory.h>
#include <folly/Range.h>

namespace facebook {
namespace glean {
namespace ffi {

/// A wrapper for a malloc'ed block of memory. Ownership can be release via
/// 'release' and 'release_to'; the memory must then be released manually via
/// 'free'. This makes it suitable for passing to Haskell.
template<typename T>
struct malloced_array {
  static_assert(std::is_trivially_destructible<T>::value,
    "malloced_array requires a trivially destructible type");

  malloced_array() noexcept : n(0) {}
  explicit malloced_array(size_t k)
    : ptr(folly::allocate_sys_buffer(k * sizeof(T))), n(k) {}
  malloced_array(folly::SysBufferUniquePtr&& p, size_t k) noexcept
    : ptr(std::move(p)), n(k) {}
  malloced_array(malloced_array&&) noexcept = default;
  malloced_array(const malloced_array&) = delete;

  malloced_array& operator=(malloced_array&& other) noexcept = default;
  malloced_array& operator=(const malloced_array&) = delete;

  template<typename U>
  void release_to(U **data, size_t *size) noexcept {
    *size = n;
    *data = release();
  }

  T *release() noexcept {
    n = 0;
    return static_cast<T *>(ptr.release());
  }

  T *get() const noexcept { return static_cast<T *>(ptr.get()); }
  size_t size() const noexcept { return n; }

  T& operator[](size_t i) const noexcept { return get()[i]; }

  void prune(size_t k) {
    assert(k <= n);
    n = k;
  }

private:
  folly::SysBufferUniquePtr ptr;
  size_t n;
};

template<typename T>
malloced_array<T> malloc_array(size_t n) {
  return malloced_array<T>(n);
}

template<typename T>
malloced_array<T> clone_array(const T *data, size_t size) {
  malloced_array<T> arr(size);
  if (size != 0) {
    std::memcpy(arr.get(), data, size * sizeof(T));
  }
  return arr;
}

inline malloced_array<uint8_t> clone_bytes(const void *data, size_t size) {
  return clone_array<uint8_t>(static_cast<const uint8_t *>(data), size);
}

inline malloced_array<uint8_t> clone_bytes(folly::ByteRange bytes) {
  return clone_bytes(bytes.data(), bytes.size());
}

inline malloced_array<uint8_t> clone_bytes(const folly::fbstring& bytes) {
  return clone_bytes(bytes.data(), bytes.size());
}

inline malloced_array<uint8_t> clone_bytes(const std::string& bytes) {
  return clone_bytes(bytes.data(), bytes.size());
}

inline malloced_array<char> clone_string(const std::string& s) {
  auto p = s.c_str();
  return clone_array(p, strlen(p)+1);
}

}
}
}
