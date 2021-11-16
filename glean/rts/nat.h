/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/Bits.h>
#include <folly/CppAttributes.h>
#include <folly/Likely.h>
#include <folly/Range.h>

namespace facebook {
namespace glean {
namespace rts {

// Packed natural numbers
//
// This module provides a packed representation of unsigned 64-bit numbers
// with the following properties (in order of importance).
//
// - Packed numbers compare lexicographically (i.e., memcmp can be used to
//   to compare two numbers).
// - As a collorary, every 64-bit number has exacly one packed representation
//   (some other compact encodings don't necessarily guarantee this).
// - They are very compact (max. 9 bytes for a 64-bit number compared to max
//   10 for varints).
// - They are quite fast to encode/decode.
//
// This encoding is a variant of prefix varints, albeit not one I've seen
// before. The first byte of an encoded number indicates how many bytes the
// number takes up and contains some data.
//
//   First byte    Bytes     Bits    Range
//   0nnnnnnn        1         7     0 - 0x7F
//   10nnnnnn        2        14     0x80 - 0x407F
//   110nnnnn        3        21     0x4080 - 0x20407F
//   1110nnnn        4        28     0x204080 - 0x1020407F
//   11110nnn        5        35     0x10204080 - 0x081020407F
//   111110nn        6        42     0x0810204080 - 0x04081020407F
//   1111110n        7        49     0x040810204080 - 0x0204081020407F
//   11111110        8        56     0x02040810204080 - 0x010204081020407F
//   11111111        9        64     0x010204081020407F - 0xFFFFFFFFFFFFFFFF
//
// Numbers are stored in big endian format. Here is how 0x123456 is stored:
//
//  11010001 11110011 11010110
//  +--+----------------------+
//    |           |
//    |           +- 21 significant bits storing 0x11F3D6, i.e.,
//    |              0x123456 - 0x4080 (which is the start of the range)
//    |
//    +- prefix indicates 3 byte representation
//
// Subtracting the start of the range from the number rather than just storing
// the bit pattern as is has two advantages. Firstly, this ensures that every
// number has exactly one packed representation without us having to do any
// checks. There is one exception: for 9 bytes encodings, the bit patterns
// above 0xFEFDFBF7EFDFBF7F are invalid - but this is an easy check which will
// will almost never be done because the vast majority of numbers are encoded
// in fewer than 9 bytes. Secondly, this is slightly more compact.

/// Compute the total number of bytes used to encode a number from the first
/// byte of the encoding.
inline size_t natSize(unsigned char b0) {
  static constexpr unsigned char natSizes[] = {
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      1, 1, 1, 1, 1, 1, 1, 1, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      2, 2, 2, 2, 2, 2, 2, 2, //
      3, 3, 3, 3, 3, 3, 3, 3, //
      3, 3, 3, 3, 3, 3, 3, 3, //
      3, 3, 3, 3, 3, 3, 3, 3, //
      3, 3, 3, 3, 3, 3, 3, 3, //
      4, 4, 4, 4, 4, 4, 4, 4, //
      4, 4, 4, 4, 4, 4, 4, 4, //
      5, 5, 5, 5, 5, 5, 5, 5, //
      6, 6, 6, 6, 7, 7, 8, 9, //
  };

  // NOTE: For smaller numbers, this benchmarks as significantly faster than
  // just
  //   return natSizes[b0]
  // I'm actually not entirely sure why.
  if (b0 < 0x80) {
    return 1;
  } else if (b0 < 0xC0) {
    return 2;
  } else if (b0 < 0xE0) {
    return 3;
  } else if (b0 < 0xF0) {
    return 4;
  } else if (b0 < 0xF8) {
    return 5;
  } else {
    return natSizes[b0];
  }
}

/// Skip over an encoded nat but check it for validity. Return a pointer to the
/// first byte after the nat or NULL if the encoding is invalid or the buffer
/// is too short.
const unsigned char* FOLLY_NULLABLE skipUntrustedNat(
    const unsigned char* p, // pointer to first byte
    const unsigned char* e // pointer to end of buffer
);

/// Skip over an encoded nat without any checks. Return a pointer to the first
/// byte after the nat.
inline const unsigned char* skipTrustedNat(const unsigned char* p) {
  return p + natSize(*p);
}

/// Check and decode an encoded nat and return the decoded number and a pointer
/// to the first byte after the nat. The pointer will be NULL is the number is
/// invalid of the buffer is too short.
std::pair<uint64_t, const unsigned char * FOLLY_NULLABLE> loadUntrustedNat(
    const unsigned char* p, // pointer to first byte
    const unsigned char* e // pointer to end of buffer
);

/// Decode an encoded nat without any checks. Return the decoded number and a
/// pointer to the first byte after the nat.
std::pair<uint64_t, const unsigned char*> loadTrustedNat(const unsigned char*);

/// Max number of bytes that an encoded nat can take up.
inline constexpr size_t MAX_NAT_SIZE = 9;

/// Encode and store a number in the buffer and return the number of bytes
/// taken up by the encoding. This assumes that the buffer has enough space.
size_t storeNat(unsigned char* out, uint64_t val);

/// A stack-allocated encoded nat
struct EncodedNat {
  explicit EncodedNat(uint64_t val) {
    encoded_size = storeNat(buf, val);
  }
  folly::ByteRange byteRange() {
    return folly::ByteRange(buf, buf + encoded_size);
  }
 private:
  uint8_t buf[rts::MAX_NAT_SIZE];
  size_t encoded_size;
};

} // namespace rts
} // namespace glean
} // namespace facebook
