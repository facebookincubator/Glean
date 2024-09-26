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

namespace facebook {
namespace glean {
namespace rts {

struct CmpSet {
  bool operator()(const folly::fbstring& lhs
                 ,const folly::fbstring& rhs) const {
    return std::lexicographical_compare(lhs.begin(),lhs.end(),rhs.begin(),rhs.end());
  }
};

using BytestringSet = std::set<folly::fbstring, CmpSet>;

using WordSet = std::set<uint64_t>;

}
}
} // namespace facebook
