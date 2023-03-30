/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <folly/dynamic.h>
#include <folly/json.h>

namespace facebook::glean::cpp{

folly::dynamic* parseJSONFacts(
    const char* str,
    int64_t len,
    char** err) noexcept {
  folly::json::serialization_opts opts;

  // folly's default recursion limit is 100, which is not enough for us.
  opts.recursion_limit = 500;

  try {
    auto parsedJson = parseJson(folly::StringPiece(str, len), opts);

    // The contents is expected to be a list of {"predicate", "facts"} objects
    // We render each fact object into a String as expected by the Write server
    for (auto& obj : parsedJson) {
      for (auto& fact : obj["facts"]) {
        fact = toJson(fact);
      }
    }

    return new folly::dynamic(std::move(parsedJson));

  } catch (const std::exception& e) {
    *err = strdup(e.what());
    return nullptr;
  }
}

}
