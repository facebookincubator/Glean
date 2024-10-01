/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/Format.h>
#include <folly/Hash.h>
#include <folly/container/HeterogeneousAccess.h>

namespace facebook {
namespace glean {
namespace rts {

const uint64_t FIRST_FREE_ID = 1024;
const uint64_t INVALID_ID = 0;

template <typename Tag>
struct WordId {
  // We might use uint32_t for pids in the future
  using word_type = uint64_t;

  // The FFI interface uses signed integers, because we need
  // to send data over Thrift which doesn't have unsigned types
  // and it's easier to convert at the FFI boundary.
  using thrift_word_type = int64_t;

  WordId() : val(0) {}

  WordId(const WordId&) = default;
  WordId& operator=(const WordId&) = default;

  static constexpr WordId invalid() {
    return WordId(INVALID_ID);
  }

  static constexpr WordId lowest() {
    return WordId(FIRST_FREE_ID);
  }

  explicit operator bool() const {
    return val != INVALID_ID;
  }

  bool operator==(const WordId& other) const {
    return val == other.val;
  }

  bool operator!=(const WordId& other) const {
    return val != other.val;
  }

  bool operator<=(const WordId& other) const {
    return val <= other.val;
  }

  bool operator<(const WordId& other) const {
    return val < other.val;
  }

  bool operator>=(const WordId& other) const {
    return val >= other.val;
  }

  bool operator>(const WordId& other) const {
    return val > other.val;
  }

  WordId operator+(word_type i) const {
    return WordId(val + i);
  }

  WordId& operator+=(word_type i) {
    val += i;
    return *this;
  }

  WordId& operator++() {
    ++val;
    return *this;
  }

  WordId operator++(int) {
    return WordId(val++);
  }

  WordId operator-(word_type i) const {
    assert(val > i);
    return WordId(val - i);
  }

  WordId& operator-=(word_type i) {
    assert(val > i);
    val -= i;
    return *this;
  }

  word_type operator-(WordId other) const {
    assert(val >= other.val);
    return val - other.val;
  }

  WordId& operator--() {
    assert(val > 1);
    --val;
    return *this;
  }

  WordId operator--(int) {
    assert(val > 1);
    return WordId(val--);
  }

  constexpr word_type toWord() const {
    return val;
  }

  static constexpr WordId fromWord(word_type x) {
    return WordId(x);
  }

  constexpr thrift_word_type toThrift() const {
    return static_cast<thrift_word_type>(val);
  }

  static constexpr WordId fromThrift(thrift_word_type id) {
    return WordId(id);
  }

 private:
  explicit constexpr WordId(word_type x) : val(x) {}

  word_type val = static_cast<word_type>(0);
};

template <typename Tag>
inline typename WordId<Tag>::word_type distance(
    WordId<Tag> from,
    WordId<Tag> to) {
  assert(from <= to);
  return to.toWord() - from.toWord();
}

struct Pid_tag;
struct Fid_tag;

using Pid = WordId<Pid_tag>;
using Id = WordId<Fid_tag>;

} // namespace rts
} // namespace glean
} // namespace facebook

namespace folly {

template <typename T>
struct FormatValue<facebook::glean::rts::WordId<T>> {
  facebook::glean::rts::WordId<T> id;

  explicit FormatValue(facebook::glean::rts::WordId<T> i) : id(i) {}

  template <class Callback>
  void format(FormatArg& arg, Callback& cb) const {
    return FormatValue<typename facebook::glean::rts::WordId<T>::word_type>(
               id.toWord())
        .format(arg, cb);
  }
};

template <typename T>
struct hasher<facebook::glean::rts::WordId<T>> {
  using word_hasher =
      hasher<typename facebook::glean::rts::WordId<T>::word_type>;
  using folly_is_avalanching = typename word_hasher::folly_is_avalanching;

  size_t operator()(facebook::glean::rts::WordId<T> id) const noexcept {
    return Hash()(id.toWord());
  }
};

template <typename T>
struct HeterogeneousAccessHash<facebook::glean::rts::WordId<T>>
    : hasher<facebook::glean::rts::WordId<T>> {
  using is_transparent = void;
};

} // namespace folly
