/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <glog/logging.h>

#include <folly/FBString.h>
#include <set>

#include "glean/rts/binary.h"
#include "glean/rts/set.h"

#include <stdexcept>

namespace facebook {
namespace glean {
namespace rts {

SetOps::SetToken SetOps::newSet() {
  sets.emplace_back(BytestringSet());
  set_sizes.emplace_back(0);
  return sets.size() - 1;
}

void SetOps::insertOutputSet(SetOps::SetToken token, binary::Output* out) {
  auto& s = sets[token];
  auto size = out->size();
  if (set_sizes[token] + size <= max_set_size) {
    s.insert(out->moveToFbString());
    set_sizes[token] += size;
  } else {
    rts::error(
        "Set size limit exceeded for standard set. Max size: {}. Size: {}",
        max_set_size,
        set_sizes[token] + size);
  }
}

void SetOps::setToArray(SetOps::SetToken token, binary::Output* out) {
  auto& s = sets[token];
  out->packed(s.size());
  for (const auto& v : s) {
    out->bytes(v.data(), v.size());
  }
}

void SetOps::freeSet(SetOps::SetToken token) {
  sets.erase(sets.begin() + token);
  set_sizes.erase(set_sizes.begin() + token);
}

SetOps::SetToken SetOps::newWordSet() {
  wordsets.emplace_back(WordSet());
  return wordsets.size() - 1;
}

void SetOps::insertWordSet(SetOps::SetToken token, uint64_t value) {
  auto& set = wordsets[token];
  if ((set.size() + 1) * sizeof(uint64_t) <= max_set_size) {
    set.insert(value);
  } else {
    rts::error(
        "Set size limit exceeded for nat set. Max size: {}. Size: {}",
        max_set_size,
        set.size() + sizeof(uint64_t));
  }
}

void SetOps::insertBytesWordSet(
    SetToken token,
    const unsigned char* start,
    const unsigned char* end) {
  auto& set = wordsets[token];
  if (set.size() * sizeof(uint64_t) + (end - start) <= max_set_size) {
    for (const unsigned char* p = start; p < end; p++) {
      set.insert(*p);
    }
  } else {
    rts::error(
        "Set size limit exceeded for byte set. Max size: {}. Size: {}",
        max_set_size,
        set.size() + (end - start));
  }
}

void SetOps::wordSetToArray(SetOps::SetToken token, binary::Output* out) {
  auto& s = wordsets[token];
  out->packed(s.size());
  for (const auto& v : s) {
    out->packed(v);
  }
}

void SetOps::byteSetToByteArray(SetOps::SetToken token, binary::Output* out) {
  auto& s = wordsets[token];
  out->packed(s.size());
  for (const auto& v : s) {
    out->fixed<uint8_t>(v);
  }
}

void SetOps::freeWordSet(SetOps::SetToken token) {
  wordsets.erase(wordsets.begin() + token);
}

} // namespace rts
} // namespace glean
} // namespace facebook
