/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * Stubbed API for AVX instruction dependencies
 */

#pragma once

#include <glog/logging.h>

#include <stdint.h>

#include <folly/Range.h>
#include <folly/compression/Instructions.h>

/*
 * Stub of EliasFanoCoding.h API used by Glean
 */
namespace folly {
namespace compression {

template <class Pointer>
struct EliasFanoCompressedListBase {
  EliasFanoCompressedListBase() = default;

  template <class OtherPointer>
  EliasFanoCompressedListBase(
      const EliasFanoCompressedListBase<OtherPointer>& other)
      : size(other.size),
        numLowerBits(other.numLowerBits),
        upperSizeBytes(other.upperSizeBytes),
        data(other.data),
        skipPointers(reinterpret_cast<Pointer>(other.skipPointers)),
        forwardPointers(reinterpret_cast<Pointer>(other.forwardPointers)),
        lower(reinterpret_cast<Pointer>(other.lower)),
        upper(reinterpret_cast<Pointer>(other.upper)) {}

  void free(void) {
    return;
  }
  size_t size = 0;
  uint8_t numLowerBits = 0;
  size_t upperSizeBytes = 0;
  Range<Pointer> data;
  Pointer skipPointers = nullptr;
  Pointer forwardPointers = nullptr;
  Pointer lower = nullptr;
  Pointer upper = nullptr;
};

using EliasFanoCompressedList = EliasFanoCompressedListBase<const uint8_t*>;
using MutableEliasFanoCompressedList = EliasFanoCompressedListBase<uint8_t*>;

template <
    class Value,
    class SkipValue = uint64_t,
    size_t kSkipQuantum = 0, // 0 = disabled
    size_t kForwardQuantum = 0, // 0 = disabled
    bool kUpperFirst = false>
struct EliasFanoEncoder {
  using CompressedList = EliasFanoCompressedList;
  using MutableCompressedList = MutableEliasFanoCompressedList;

  using ValueType = Value;
  using SkipValueType = SkipValue;

  EliasFanoEncoder(size_t size_, ValueType upperBound) : size(size_) {}

  void add(ValueType value) {
    return;
  }

  const MutableCompressedList& finish() {
    return result_;
  }

  size_t size = 0;
  MutableCompressedList result_;
};

template <
    class Encoder,
    class Instructions = instructions::Default,
    bool kUnchecked = false,
    class SizeT = typename Encoder::SkipValueType>
class EliasFanoReader {
 public:
  using EncoderType = Encoder;
  using ValueType = typename Encoder::ValueType;
  using SizeType = SizeT;

  EliasFanoReader(const typename Encoder::CompressedList& list)
      : numLowerBits_(list.numLowerBits) {}

  bool previous() {
    return false;
  }
  bool next() {
    return false;
  }

  ValueType value() const {
    return value_;
  }

  template <bool kCanBeAtValue = true>
  bool skipTo(ValueType value) {
    return true;
  }

  bool valid() const {
    return true;
  }

 private:
  ValueType value_;
  const uint8_t numLowerBits_;
};

} // namespace compression
} // namespace folly
