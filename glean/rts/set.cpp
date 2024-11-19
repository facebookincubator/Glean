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

namespace facebook {
namespace glean {
namespace rts {

SetOps::SetToken SetOps::newSet() {
  sets.emplace_back(BytestringSet());
  return sets.size() - 1;
}

void SetOps::insertOutputSet(SetOps::SetToken token, binary::Output* out) {
  sets[token].insert(out->moveToFbString());
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
}

SetOps::SetToken SetOps::newWordSet() {
  wordsets.emplace_back(WordSet());
  return wordsets.size() - 1;
}

void SetOps::insertWordSet(SetOps::SetToken token, uint64_t value) {
  wordsets[token].insert(value);
}

void SetOps::insertBytesWordSet(
    SetToken token,
    const unsigned char* start,
    const unsigned char* end) {
  auto& set = wordsets[token];
  for (const unsigned char* p = start; p < end; p++) {
    set.insert(*p);
  }
}

void SetOps::wordSetToArray(SetOps::SetToken token, binary::Output* out) {
  auto& s = wordsets[token];
  out->packed(s.size());
  for (const auto& v : s) {
    out->packed(v);
  }
}

void SetOps::freeWordSet(SetOps::SetToken token) {
  wordsets.erase(wordsets.begin() + token);
}

} // namespace rts
} // namespace glean
} // namespace facebook
