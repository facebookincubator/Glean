/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifdef OSS
#include <cpp/memory.h> // @manual
#include <cpp/wrap.h> // @manual
#else
#include <common/hs/util/cpp/memory.h>
#include <common/hs/util/cpp/wrap.h>
#endif
#include "glean/rts/json.h"

#include <folly/json/dynamic.h>
#include <folly/json/json.h>
#include <folly/lang/ToAscii.h>

using namespace facebook::hs;

namespace {

const folly::dynamic* todyn(glean_json_value_t* value) {
  return reinterpret_cast<folly::dynamic*>(value);
}

glean_json_value_t* fromdyn(const folly::dynamic* dyn) {
  return reinterpret_cast<glean_json_value_t*>(
      const_cast<folly::dynamic*>(dyn));
}

// JSON escaping

constexpr char K = 0; // keep character as is
constexpr char Z = 1; // NUL - hex-escape but treat specially in mangled repr
constexpr char X = 2; // hex-escape

using EscapeTable = std::array<char, 256>;

// JSON escape table: hescape all control characters, quotes and backslash
constexpr EscapeTable regularEscapes = {
    X,   X,   X,   X, X,    X,   X, X, // 0
    'b', 't', 'n', X, 'f',  'r', X, X, // 8
    X,   X,   X,   X, X,    X,   X, X, // 16
    X,   X,   X,   X, X,    X,   X, X, // 24
    K,   K,   '"', K, K,    K,   K, K, // 32
    K,   K,   K,   K, K,    K,   K, K, // 40
    K,   K,   K,   K, K,    K,   K, K, // 48
    K,   K,   K,   K, K,    K,   K, K, // 56
    K,   K,   K,   K, K,    K,   K, K, // 64
    K,   K,   K,   K, K,    K,   K, K, // 72
    K,   K,   K,   K, K,    K,   K, K, // 80
    K,   K,   K,   K, '\\', K,   K, K, // 88
    K,   K,   K,   K, K,    K,   K, K, // 96
    K,   K,   K,   K, K,    K,   K, K, // 104
    K,   K,   K,   K, K,    K,   K, K, // 112
    K,   K,   K,   K, K,    K,   K, K, // 120
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K,
};

// Same as regularEscapes except that NUL is treated specially
constexpr EscapeTable mangledEscapes = {
    Z,   X,   X,   X, X,    X,   X, X, // 0
    'b', 't', 'n', X, 'f',  'r', X, X, // 8
    X,   X,   X,   X, X,    X,   X, X, // 16
    X,   X,   X,   X, X,    X,   X, X, // 24
    K,   K,   '"', K, K,    K,   K, K, // 32
    K,   K,   K,   K, K,    K,   K, K, // 40
    K,   K,   K,   K, K,    K,   K, K, // 48
    K,   K,   K,   K, K,    K,   K, K, // 56
    K,   K,   K,   K, K,    K,   K, K, // 64
    K,   K,   K,   K, K,    K,   K, K, // 72
    K,   K,   K,   K, K,    K,   K, K, // 80
    K,   K,   K,   K, '\\', K,   K, K, // 88
    K,   K,   K,   K, K,    K,   K, K, // 96
    K,   K,   K,   K, K,    K,   K, K, // 104
    K,   K,   K,   K, K,    K,   K, K, // 112
    K,   K,   K,   K, K,    K,   K, K, // 120
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K, K, K, K, K,
    K,   K,   K,   K, K,    K,   K, K, K, K, K, K, K, K, K, K, K, K,
};

size_t escaped_size(
    const EscapeTable& escapes,
    const uint8_t* text,
    size_t text_size) noexcept {
  size_t size = 0;
  for (size_t i = 0; i < text_size; ++i) {
    switch (escapes[text[i]]) {
      case K:
        ++size;
        break;

      case Z:
        // \0 is represented as \0 \1 in the mangled representation
        assert(i < text_size);
        ++i;
        [[fallthrough]];

      case X:
        size += 6;
        break;

      default:
        size += 2;
        break;
    }
  }
  return size;
}

char hex(uint8_t c) {
  return c + (c < 10 ? '0' : 'a' - 10);
}

void escape(
    const EscapeTable& escapes,
    const uint8_t* text,
    size_t text_size,
    char* out,
    size_t escaped_size) noexcept {
  assert(escaped_size >= text_size);
  if (escaped_size == text_size) {
    if (text_size != 0) {
      memcpy(out, text, text_size);
    }
  } else {
    size_t o = 0;
    for (size_t i = 0; i < text_size; ++i) {
      const auto c = text[i];
      switch (escapes[c]) {
        case K:
          out[o++] = c;
          break;

        case Z:
          // \0 is represented as \0 \1 in the mangled representation
          assert(i < text_size);
          ++i;
          [[fallthrough]];

        case X:
          out[o++] = '\\';
          out[o++] = 'u';
          out[o++] = '0';
          out[o++] = '0';
          out[o++] = hex(c >> 4);
          out[o++] = hex(c & 0x0F);
          break;

        default:
          out[o++] = '\\';
          out[o++] = escapes[c];
          break;
      }
      assert(o <= escaped_size);
    }
    assert(o == escaped_size);
  }
}

} // namespace

extern "C" {

struct glean_json_document_t {
  folly::dynamic root;
};

const char* glean_json_parse(
    const char* text,
    size_t size,
    size_t rec_limit,
    glean_json_document_t** document) {
  return ffi::wrap([=] {
    folly::json::serialization_opts opts;
    opts.recursion_limit = rec_limit;
    auto dyn = folly::parseJson({text, size}, opts);
    *document = new glean_json_document_t{std::move(dyn)};
  });
}

void glean_json_document_free(glean_json_document_t* document) {
  ffi::free_(document);
}

glean_json_value_t* glean_json_document_root(glean_json_document_t* document) {
  return fromdyn(&document->root);
}

int glean_json_value_type(glean_json_value_t* value) {
  return todyn(value)->type();
}

int64_t glean_json_value_get_int(glean_json_value_t* value) {
  return todyn(value)->getInt();
}

int glean_json_value_get_bool(glean_json_value_t* value) {
  return todyn(value)->getBool();
}

void glean_json_value_get_string(
    glean_json_value_t* value,
    const char** text,
    size_t* size) {
  *text = todyn(value)->c_str();
  *size = todyn(value)->size();
}

size_t glean_json_value_get_size(glean_json_value_t* value) {
  return todyn(value)->size();
}

glean_json_value_t* glean_json_value_get_array_element(
    glean_json_value_t* value,
    size_t index) {
  return fromdyn(&(*todyn(value))[index]);
}

glean_json_value_t* glean_json_value_get_object_field(
    glean_json_value_t* value,
    const char* key_name,
    size_t key_size) {
  return fromdyn(todyn(value)->get_ptr(folly::StringPiece(key_name, key_size)));
}

const char*
glean_json_encode(glean_json_value_t* value, char** out, size_t* size) {
  return ffi::wrap([=] {
    ffi::clone_string(folly::toJson(*todyn(value))).release_to(out, size);
  });
}

size_t glean_json_encode_number(int64_t number, char* out) {
  if (number < 0) {
    *out++ = '-';
    auto unumber = ~static_cast<uint64_t>(number) + 1;
    auto len = folly::to_ascii_size_decimal(unumber);
    return 1 + folly::to_ascii_decimal(out, out + len, unumber);
  } else {
    auto unumber = static_cast<uint64_t>(number);
    auto len = folly::to_ascii_size_decimal(unumber);
    return folly::to_ascii_decimal(out, out + len, unumber);
  }
}

size_t glean_json_string_escaped_size(const uint8_t* text, size_t text_size) {
  return escaped_size(regularEscapes, text, text_size);
}

void glean_json_string_escape(
    const uint8_t* text,
    size_t text_size,
    char* out,
    size_t escaped_size) {
  return escape(regularEscapes, text, text_size, out, escaped_size);
}

size_t glean_json_mangled_string_escaped_size(
    const uint8_t* text,
    size_t text_size) {
  assert(text_size >= 2);
  return escaped_size(mangledEscapes, text, text_size - 2);
}

void glean_json_mangled_string_escape(
    const uint8_t* text,
    size_t text_size,
    char* out,
    size_t escaped_size) {
  assert(text_size >= 2);
  return escape(mangledEscapes, text, text_size - 2, out, escaped_size);
}
}
