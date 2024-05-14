/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/binary.h"
#include "glean/rts/error.h"
#include "glean/rts/string.h"
#include <cassert>
#include <cstring>
#include <glog/logging.h>
#include <unicode/utf8.h>
#include <unicode/uchar.h>

namespace facebook {
namespace glean {
namespace rts {

namespace {

/// Iterate over chunks of a mangled string, delimited by NULs, and call
/// Chunk for each chunk except the last and Last for the last one, passing
/// a pointer one past the end of the chunk (including delimiters). Examples:
///
/// mangled string         calls
///
/// abc\0\0                last(q)
/// p      q
///
/// abc\0\1def\0\1gh\0\0   chunk(p), chunk(q), chunk(r), last(s)
/// p      q      r     s
///
template<typename Chunk>
FOLLY_ALWAYS_INLINE
size_t untrustedChunks(folly::ByteRange range, Chunk&& chunk) {
  const unsigned char * const p = range.data();
  const size_t size = range.size();

  assert(p != nullptr);

  int i;
  for (i = 0; i < size && p[i] > 0 && p[i] < 0x80; ++i) {}
  if (i+1 < size && p[i] == 0 && p[i+1] == 0) {
    chunk(p, i);
    return i+2;
  }

  int k = 0;
  while (true) {
    UChar c;
    // NOTE: U8_NEXT returns c<0 on overlong (invalid) points so this doesn't
    // transcode (and we don't have to worry about, say, overlong \NUL).
    U8_NEXT(p, i, size, c);
    if (c == 0) {
      if (i < size) {
        switch (p[i]) {
          case 0:
            chunk(p+k, i-k-1);
            return i+1;

          case 1:
            chunk(p+k, i-k);
            ++i;
            k = i;
            break;

          default:
            rts::error("invalid NUL in mangled string");
        }
      } else {
        rts::error("truncated terminator in mangled string");
      }
    } else if (c < 0) {
      rts::error("invalid UTF-8 string");
    }
  }
}

}

size_t validateUntrustedString(folly::ByteRange range) {
  return untrustedChunks(range, [](auto, auto) {});
}

size_t demangleUntrustedString(folly::ByteRange range, binary::Output& output) {
  return untrustedChunks(
    range,
    [&](auto p, auto n) { output.bytes(p, n); }
  );
}

namespace {

template<typename Chunk>
FOLLY_ALWAYS_INLINE
size_t trustedChunks(folly::ByteRange range, Chunk&& chunk) noexcept {
  const auto end = range.end();
  auto p = range.begin();
  while (true) {
    auto q = static_cast<const unsigned char *>(std::memchr(p, 0, end-p));
    CHECK(q && q+1 < end);
    if (q[1] == 0) {
      chunk(p, q-p);
      return q - range.begin() + 2;
    } else {
      chunk(p, q-p+1);
      p = q+2;
    }
  }
}

}

std::pair<size_t, size_t> skipTrustedString(folly::ByteRange range) noexcept {
  size_t nuls = 0;
  auto size = trustedChunks(
    range,
    [&](auto,auto) { ++nuls; }
  );
  assert (nuls > 0);
  assert (size >= nuls*2);
  return std::make_pair(size, size - nuls - 1);
}

size_t demangleTrustedString(folly::ByteRange range, uint8_t *buffer) noexcept {
  auto out = buffer;
  trustedChunks(
    range,
    [&](auto p, auto n) {
      std::memcpy(out, p, n);
      out += n;
    }
  );
  return out-buffer;
}

void mangleString(folly::ByteRange range, binary::Output& output) {
  if (!range.empty()) {
    auto p = range.begin();
    while (auto q = static_cast<const unsigned char *>(
            std::memchr(p, 0, range.end()-p))) {
      ++q;
      output.put({p,q});
      output.fixed<uint8_t>(1);
      p = q;
    }
    output.put({p, range.end()});
  }
  const unsigned char terminator[2] = {0,0};
  output.bytes(terminator, 2);
}

void toLowerTrustedString(folly::ByteRange range, binary::Output& output) {
  output.expect(range.size());
  for (int32_t i = 0; i < range.size(); ) {
    unsigned char b = static_cast<unsigned char>(range[i]);
    if (b < 0x80) {
      // This will also ignore \0 and \1 in the mangled string
      if (b >= 'A' && b <= 'Z') {
        b = b - 'A' + 'a';
      }
      output.fixed(b);
      i++;
    } else {
      UChar32 c;
      U8_NEXT_UNSAFE(range.data(), i, c);
      c = u_tolower(c);
      uint8_t buf[4];
      auto j = 0;
      U8_APPEND_UNSAFE(buf, j, c);
      output.bytes(buf, j);
    }
  }
}

}
}
}
