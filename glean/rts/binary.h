/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/FBString.h>
#include <folly/Range.h>
#include <folly/Varint.h>

#include "glean/ffi/memory.h"
#include "glean/rts/id.h"
#include "glean/rts/error.h"
#include "glean/rts/nat.h"
#include "glean/rts/string.h"

namespace facebook {
namespace glean {
namespace binary {

inline folly::ByteRange byteRange(const std::vector<unsigned char>& s) {
  return folly::ByteRange(s.data(), s.size());
}

inline folly::ByteRange byteRange(const std::string& s) {
  return folly::ByteRange(
      reinterpret_cast<const unsigned char*>(s.data()), s.size());
}

inline folly::ByteRange byteRange(const folly::fbstring& s) {
  return folly::ByteRange(
      reinterpret_cast<const unsigned char*>(s.data()), s.size());
}

inline folly::ByteRange stringRange(const char* s) {
  return folly::ByteRange(reinterpret_cast<const unsigned char*>(s), strlen(s));
}

inline std::string mkString(folly::ByteRange r) {
  return std::string(reinterpret_cast<const char*>(r.data()), r.size());
}

inline folly::fbstring mkFbstring(folly::ByteRange r) {
  return folly::fbstring(reinterpret_cast<const char*>(r.data()), r.size());
}

/// Return the smallest sequence of bytes that is lexicographically larger than
/// and sequence with the prefix 'range' or the empty sequence if no such
/// sequence exists. Example:
///
/// lexicographicallyNext({0x25, 0x42, 0xFF, 0xFF}) == {0x25, 0x43}
/// lexicographicallyNext({0xFF}) = {}
std::vector<unsigned char> lexicographicallyNext(folly::ByteRange range);

namespace detail {

template<typename T, typename = void> struct word_traits;

template<typename T>
struct word_traits<T, typename std::enable_if_t<
          std::is_integral<std::remove_const_t<T>>::value ||
          std::is_enum<std::remove_const_t<T>>::value>> {
  using word_type = T;
  static T fromWord(word_type x) { return x; }
  static word_type toWord(T x) { return x; }
};

template<typename T>
struct word_traits<rts::WordId<T>> {
  using word_type = typename rts::WordId<T>::word_type;
  static rts::WordId<T> fromWord(word_type x) {
    return rts::WordId<T>::fromWord(x);
  }
  static word_type toWord(rts::WordId<T> x) {
    return x.toWord();
  }
};

}

struct Output;

/**
 * A binary buffer which can be read from.
 *
 * NOTE: It does not own the memory!
 *
 */
struct Input {
  folly::ByteRange buf;

  Input() {}
  explicit Input(const folly::ByteRange& b) : buf(b) {}
  explicit Input(const std::string* s)
      : buf(reinterpret_cast<const unsigned char*>(s->data()), s->size()) {}
  explicit Input(const folly::fbstring* s)
      : buf(reinterpret_cast<const unsigned char*>(s->data()), s->size()) {}
  Input(const void* p, size_t n)
      : buf(static_cast<const unsigned char*>(p), n) {}
  Input(const void* start, const void* finish)
      : buf(static_cast<const unsigned char*>(start),
            static_cast<const unsigned char*>(finish)) {}

  void wantError(size_t n) const {
    rts::error("truncated input: expected {} bytes, got {}", n, buf.size());
  }

  /// Ensure that there are at least n bytes left
  inline void want(size_t n) const {
    if (buf.size() < n) {
      wantError(n);
    }
  }

  bool empty() const {
    return buf.empty();
  }

  /// Read a fixed width number.
  template <typename T>
  T fixed() {
    using word_type = typename detail::word_traits<T>::word_type;
    const auto n = sizeof(word_type);
    want(n);
    const void* p = buf.data();
    buf.uncheckedAdvance(n);
    return detail::word_traits<T>::fromWord(folly::loadUnaligned<word_type>(p));
  }

  /// Read a packed unsigned number
  template <typename T>
  inline T packed() {
    auto temp = buf;
    if (auto r = folly::tryDecodeVarint(temp)) {
      buf = temp;
      return detail::word_traits<T>::fromWord(
        static_cast<typename detail::word_traits<T>::word_type>(r.value()));
    } else {
      rts::error("invalid packed value");
    }
  }

  /// Validate and read an encoded nat
  inline uint64_t untrustedNat() {
    auto r = rts::loadUntrustedNat(buf.begin(), buf.end());
    if (r.second != nullptr) {
      buf = {r.second, buf.end()};
      return r.first;
    } else {
      rts::error("invalid nat");
    }
  }

  /// Read an encoded nat without any checks
  inline uint64_t trustedNat() {
    auto r = rts::loadTrustedNat(buf.begin());
    assert(r.second <= buf.end());
    buf = {r.second, buf.end()};
    return r.first;
  }

  /// Validate and skip over an encoded nat
  inline void skipUntrustedNat() {
    auto p = rts::skipUntrustedNat(buf.begin(), buf.end());
    if (p != nullptr) {
      buf = {p, buf.end()};
    } else {
      rts::error("invalid nat");
    }
  }

  /// Skip over an encoded nat without any checks
  inline void skipTrustedNat() {
    auto p = rts::skipTrustedNat(buf.begin());
    buf = {p, buf.end()};
  }

  uint8_t byte() {
    want(1);
    auto c = *buf.data();
    buf.uncheckedAdvance(1);
    return c;
  }

  /// Read n bytes
  folly::ByteRange bytes(size_t n) {
    want(n);
    auto p = buf.data();
    buf.uncheckedAdvance(n);
    return folly::ByteRange(p, n);
  }

  /// Read the rest of the input
  folly::ByteRange bytes() {
    return bytes(buf.size());
  }

  const unsigned char* data() const {
    return buf.data();
  }

  const unsigned char* end() const {
    return buf.end();
  }

  size_t size() const {
    return buf.size();
  }

  /// Validate and skip over a mangled UTF-8 string.
  void skipUntrustedString() {
    buf.uncheckedAdvance(rts::validateUntrustedString(buf));
  }

  /// Validate and skip over a mangled string, writing its demangled
  /// representation into the Output
  void demangleUntrustedString(Output& output) {
    buf.uncheckedAdvance(rts::demangleUntrustedString(buf, output));
  }

  /// Skip over a trusted mangled string and return its *demangled* size.
  size_t skipTrustedString() {
    auto r = rts::skipTrustedString(buf);
    buf.uncheckedAdvance(r.first);
    return r.second;
  }

  template <typename T>
  T generic_string(size_t n) {
    folly::ByteRange r = bytes(n);
    return T(reinterpret_cast<const char*>(r.data()), r.size());
  }

  folly::fbstring fbstring(size_t n) {
    return generic_string<folly::fbstring>(n);
  }

  folly::fbstring fbstring() {
    return fbstring(buf.size());
  }

  std::string string(size_t n) {
    return generic_string<std::string>(n);
  }

  std::string string() {
    return string(buf.size());
  }

  bool shift(folly::ByteRange pat) {
    auto n = pat.size();
    if (buf.size() >= n && !std::memcmp(buf.data(), pat.data(), n)) {
      buf.uncheckedAdvance(n);
      return true;
    } else {
      return false;
    }
  }
}; // namespace binary

/**
 *
 * A binary buffer which can be written to.
 *
 */
struct Output {

  Output() {}
  Output(Output&&) = default;
  Output& operator=(Output&&) = default;
  Output(const Output&) = delete;
  void operator=(const Output&) = delete;

  size_t size() const {
    return buf.size();
  }

  const unsigned char* data() const {
    return buf.data();
  }

  // Write a packed unsigned number
  template <typename T>
  void packed(T x) {
    auto p = buf.buffer(folly::kMaxVarintLength64);
    auto n = folly::encodeVarint(
      static_cast<uint64_t>(detail::word_traits<T>::toWord(x)), p);
    buf.use(n);
  }

  /// Write an encoded nat
  inline void nat(uint64_t x) {
    auto p = buf.buffer(rts::MAX_NAT_SIZE);
    auto n = rts::storeNat(p, x);
    buf.use(n);
  }

  // Write a fixed width number
  template <typename T>
  void fixed(T x) {
    const auto w = detail::word_traits<T>::toWord(x);
    bytes(&w, sizeof(w));
  }

  void put(folly::ByteRange bytes) {
    this->bytes(bytes.begin(), bytes.size());
  }

  void bytes(const void* data, size_t size) {
    if (size > 0) {
      auto b = buf.grab(size);
      std::memcpy(b, data, size);
    }
  }

  void expect(size_t n) {
    (void)buf.buffer(n);
  }

  /// Store the mangled representation of a UTF-8 string. The validity of the
  /// string isn't checked.
  void mangleString(folly::ByteRange r) {
    rts::mangleString(r, *this);
  }

  folly::ByteRange bytes() & {
    return buf.to<folly::ByteRange>();
  }

  folly::fbstring fbstring() const {
    return buf.to<folly::fbstring>();
  }

  std::string string() const {
    return buf.to<std::string>();
  }

  ffi::malloced_array<uint8_t> moveBytes() {
    return buf.moveBytes();
  }

  folly::fbstring moveToFbString() {
    return buf.moveToFbString();
  }

 private:
  /// Simple implementation of a growable buffer. We need to be able to get
  /// ownership of the underlying memory which only folly::fbstring and
  /// folly::IOBuf seem to provide but both seem to be significantly slower
  /// (the latter more so than the former).
  class Buf {
   public:
    Buf() {
      len = 0;
    }
    Buf(Buf&&) = default;
    Buf& operator=(Buf&&) = default;
    Buf(const Buf&) = delete;
    Buf& operator=(const Buf&) = delete;

    size_t size() const {
      return len;
    }

    size_t capacity() const {
      return buf.size();
    }

    const unsigned char* data() const {
      return buf.get();
    }

    /// Return a point to enough space for n bytes. This reserves memory but
    /// doesn't increase the size - this can be done via 'use' afterwards.
    unsigned char* buffer(size_t n) {
      if (n > capacity() - size()) {
        realloc(n);
      }
      return buf.get() + size();
    }

    /// Increase the size of the buffer. This doesn't reserve memory so the
    /// new size must be <= capacity.
    void use(size_t n) {
      assert(capacity() - size() >= n);
      len += n;
    }

    /// Increase the buffer size by n and return a pointer to the new memory.
    unsigned char* grab(size_t n) {
      auto p = buffer(n);
      use(n);
      return p;
    }

    ffi::malloced_array<uint8_t> moveBytes() {
      buf.prune(len);
      return std::move(buf);
    }

    /// Transfer ownership of the underlying memory to a folly::fbstring.
    folly::fbstring moveToFbString();

    /// Create a container
    template<typename C> C to() const {
      return C(buf.get(), buf.get() + size());
    }

   private:
    void realloc(size_t n);

    ffi::malloced_array<uint8_t> buf;
    size_t len;
  };

  Buf buf;
};

} // namespace binary
} // namespace glean
} // namespace facebook
