/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <vector>

namespace facebook {
namespace glean {
namespace rts {

template<typename T>
struct Pool {
  static_assert(std::is_trivially_destructible_v<T>, "invalid T");
  std::vector<char *> pages;
  size_t page_size;
  size_t filled;

  Pool() noexcept {
    page_size = folly::goodMallocSize(1000000)/sizeof(T);
    filled = page_size;
  }

  ~Pool() noexcept {
    clear();
  }

  Pool(const Pool&) = delete;
  Pool(Pool&&) noexcept = default;

  Pool& operator=(const Pool&) = delete;
  Pool& operator=(Pool&&) noexcept = default;

  void swap(Pool& other) {
    pages.swap(other.pages);
    std::swap(page_size, other.page_size);
    std::swap(filled, other.filled);
  }

  void clear() {
    for (auto p : pages) {
      free(p);
    }
    pages.clear();
    filled = page_size;
  }

  void *next() {
    if (filled == page_size) {
      pages.push_back(static_cast<char *>(malloc(sizeof(T) * page_size)));
      filled = 0;
    }
    const auto p = pages.back() + filled * sizeof(T);
    ++filled;
    return p;
  }

  template<typename... Args>
  T *alloc(Args&&... args) {
    return new (next()) T(std::forward<Args>(args)...);
  }

  size_t count() const {
    return pages.size() * page_size + filled - page_size;
  }
};

}
}
}
