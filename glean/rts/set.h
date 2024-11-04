/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <folly/FBString.h>
#include <set>

#include "glean/rts/binary.h"

namespace facebook {
namespace glean {
namespace rts {

struct CmpSet {
  bool operator()(const folly::fbstring& lhs, const folly::fbstring& rhs)
      const {
    return std::lexicographical_compare(
        lhs.begin(), lhs.end(), rhs.begin(), rhs.end());
  }
};

using BytestringSet = std::set<folly::fbstring, CmpSet>;

using WordSet = std::set<uint64_t>;

struct SetOps {
  using SetToken = uint64_t;

  SetToken newSet();

  void insertOutputSet(SetToken token, binary::Output* out);

  void setToArray(SetToken token, binary::Output* out);

  void freeSet(SetToken token);

  SetToken newWordSet();

  void insertWordSet(SetToken token, uint64_t out);

  void insertBytesWordSet(
      SetToken token,
      const unsigned char* start,
      const unsigned char* end);

  void wordSetToArray(SetToken token, binary::Output* out);

  void freeWordSet(SetToken token);

  std::vector<BytestringSet> sets = {};
  std::vector<WordSet> wordsets = {};
};

} // namespace rts
} // namespace glean
} // namespace facebook
