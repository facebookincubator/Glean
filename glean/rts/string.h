/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <utility>
#include <folly/Range.h>

// UTF-8 string representation
//
// We use a slightly mangled representation for UTF-8 strings. A string is
// terminated by the two bytes 0x00 0x00. Any embedded \NUL characters are
// converted to the sequence 0x00 0x01. All other character are left untouched.
// This encoding has the following properties.
//
//   1. Prefix search and compression works properly.
//   2. memcmp sorts strings as expected (esp. for prefixes).
//   3. Embedded \NUL is supported.
//   4. No mangling happens in the common case (no embedded \NUL).

namespace facebook {
namespace glean {

namespace binary {
struct Output;
}

namespace rts {

/// Validate an untrusted mangled string and return its size, including the
/// terminator.
size_t validateUntrustedString(folly::ByteRange);

/// Demangle an untrusted mangled string into an Output and return its mangled
/// size, including the terminator.
size_t demangleUntrustedString(folly::ByteRange, binary::Output&);

/// Return the mangled and demangled sizes of a trusted string.
std::pair<size_t, size_t> skipTrustedString(folly::ByteRange) noexcept;

/// Demangle a trusted mangled string into a buffer and return its mangled size.
/// The buffer must have enough space.
size_t demangleTrustedString(folly::ByteRange, uint8_t *) noexcept;

/// Mangle a UTF-8 string into the Output.
void mangleString(folly::ByteRange, binary::Output&);

/// Tolower a mangled string
void toLowerTrustedString(folly::ByteRange, binary::Output&);

void reverseTrustedString(unsigned char *p, size_t size);

}
}
}
