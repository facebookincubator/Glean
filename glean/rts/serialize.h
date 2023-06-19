/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <variant>
#include "glean/rts/binary.h"
#include "glean/rts/id.h"
#include <glog/logging.h>

namespace facebook {
namespace glean {
namespace rts {

namespace serialize {

inline void put(binary::Output& o, uint64_t i) { o.nat(i); }
inline void put(binary::Output& o, int32_t i) { o.nat(i); }
inline void put(binary::Output& o, bool b) { o.fixed(uint8_t{b}); }
inline void put(binary::Output& o, Id p) { put(o, p.toWord()); }
inline void put(binary::Output& o, Pid p) { put(o, p.toWord()); }
inline void put(binary::Output& o, const std::string &s) {
  put(o, s.size());
  o.put(binary::byteRange(s));
}
inline void put(binary::Output& o, const binary::Output &out) {
    put(o, out.size());
    o.put(out.bytes());
}

template <typename T>
void put(binary::Output& o, const T* p) { put(o, *p); }

// We can specify type-specific serialization by defining a static method
//   T::put(binary::Output&, T&)
// and then serializing a std::vector<T> or std::optional<T> will work.
template <typename T>
void put(binary::Output& o, const T& p) {
    T::put(o,p);
}

template <typename T>
void put(binary::Output& o, const folly::Range<T*>& r) {
  put(o, r.size());
  for (auto& i : r) {
    put(o, i);
  }
}

template <typename T>
void put(binary::Output& o, const std::vector<T>& vec) {
  put(o, folly::Range<const T*>(vec.data(), vec.size()));
}

struct AsBytes {};

// put(out, vec, AsBytes{})
//   serialize a vector of integrals as a binary blob
template <typename T, typename = std::enable_if<std::is_integral_v<T>>>
void put(binary::Output& o, const std::vector<T>& vec, AsBytes) {
  put(o, vec.size());
  o.bytes(reinterpret_cast<const uint8_t*>(vec.data()), vec.size() * sizeof(T));
}

template <typename T>
void put(binary::Output& o, const std::optional<T>& opt) {
  if (opt.has_value()) {
    o.fixed(uint8_t{1});
    put(o, *opt);
  } else {
    o.fixed(uint8_t{0});
  }
}

template <typename T>
void put(binary::Output& o, const std::shared_ptr<T>& s) {
  if (s) {
    o.fixed(uint8_t{1});
    put(o, *s);
  } else {
    o.fixed(uint8_t{0});
  }
}


inline void get(binary::Input& i, uint64_t &r) { r = i.untrustedNat(); }
inline void get(binary::Input& i, int32_t &r) { r = i.untrustedNat(); }
inline void get(binary::Input& i, bool &r) { r = i.byte(); }
inline void get(binary::Input& i, Id &p) { uint64_t w; get(i, w); p = Id::fromWord(w); }
inline void get(binary::Input& i, Pid &p) { uint64_t w; get(i, w); p = Pid::fromWord(w); }
inline void get(binary::Input& i, std::string &s) {
  size_t n;
  get(i, n);
  auto r = i.bytes(n);
  s = binary::mkString(r);
}
inline void get(binary::Input& i, binary::Output& out) {
  size_t n;
  get(i, n);
  out.put(i.bytes(n));
}

template <typename T>
void get(binary::Input& i, T& p) {
  T::get(i,p);
}

template <typename T>
void get(binary::Input& i, std::vector<T>& vec) {
  size_t count;
  get(i, count);
  vec.reserve(count);
  vec.resize(0);
  for (size_t n = 0; n < count; n++) {
    T elt;
    get(i, elt);
    vec.push_back(std::move(elt));
  }
}

template <typename T, typename = std::enable_if<std::is_integral_v<T>>>
void get(binary::Input& i, std::vector<T>& vec, AsBytes) {
  size_t count;
  get(i, count);
  vec.resize(count);
  folly::ByteRange bytes = i.bytes(count * sizeof(T));
  std::copy(bytes.begin(), bytes.end(), reinterpret_cast<uint8_t*>(vec.data()));
}

template<typename T>
void get(binary::Input& i, std::optional<T> &opt) {
  auto x = i.byte();
  if (x) {
    T y;
    get(i, y);
    opt = std::move(y);
  } else {
    opt = {};
  }
}

template <typename T>
void get(binary::Input& i, std::shared_ptr<T>& p) {
  auto x = i.byte();
  if (x) {
    p = std::make_shared<T>();
    get(i, *p);
  } else {
    p = nullptr;
  }
}

/// A Thrift compact serializer that only supports a very
// small subset of the types.
namespace thriftcompact {

using Nat = int64_t;
using Binary = folly::ByteRange;
using String = std::string;
using List = std::vector<Nat>;  // TODO: generalise
using Map = std::map<String, List>;  // TODO: generalise

using Field = std::variant<Nat, Binary, Map>;
using Object = std::vector<std::pair<uint32_t, Field>>;

enum Type : uint32_t {
  NatTy = 6,
  BinaryTy = 8,
  StringTy = 8,
  ListTy = 9,
  MapTy = 11,
};

template<typename T> static Type typeOf();
template<> Type typeOf<Nat>() { return NatTy; }
template<> Type typeOf<Binary>() { return BinaryTy; }
template<> Type typeOf<String>() { return StringTy; }
template<> Type typeOf<std::vector<Nat>>() { return ListTy; }

inline void put(binary::Output& out, Nat x) {
  out.packed(folly::encodeZigZag(x));
}

inline void put(binary::Output& out, Binary b) {
  out.packed(b.size());
  out.bytes(b.data(), b.size());
}

inline void put(binary::Output& out, const String& s) {
  out.packed(s.size());
  out.put(binary::byteRange(s));
}

template<typename T>
void put(binary::Output& out, const std::vector<T>& v) {
  if (v.size() < 15) {
    out.fixed(uint8_t((v.size() << 4) | typeOf<T>()));
  } else {
    out.fixed(uint8_t(0xF0 | typeOf<T>()));
    out.packed(v.size());
  }
  for (const T& i : v) {
    put(out, i);
  }
}

template <typename K, typename V>
void put(binary::Output& out, const std::map<K, V>& m) {
  if (m.size() == 0) {
    out.fixed(uint8_t(0));
  } else {
    out.packed(m.size());
    out.fixed(uint8_t((typeOf<K>() << 4) | typeOf<V>()));
    for (const auto& pair : m) {
      put(out, pair.first);
      put(out, pair.second);
    }
  }
}

inline void put(binary::Output& out, const Object& obj) {
  uint32_t prev = 0;
  auto field = [&](Type ty, uint32_t num) {
    auto delta = num - prev;
    if (delta < 16) {
      out.fixed(uint8_t(ty + (delta << 4)));
    } else {
      out.fixed(uint8_t(ty));
      out.packed(num);
    }
    prev = num;
  };
  for (const auto& [num, val] : obj) {
    if (std::holds_alternative<Nat>(val)) {
      field(NatTy, num);
      put(out, std::get<Nat>(val));
    } else if (std::holds_alternative<Binary>(val)) {
      field(BinaryTy, num);
      put(out, std::get<Binary>(val));
    } else if (std::holds_alternative<Map>(val)) {
      field(MapTy, num);
      put(out, std::get<Map>(val));
    }
  }
  out.fixed(uint8_t(0)); // object terminator
}

} // namespace thriftcompact

} // namespace serialize
} // namespace rts
} // namespace glean
} // namespace facebook
