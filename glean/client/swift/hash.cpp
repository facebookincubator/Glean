/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/hash.h"
#include <folly/String.h>
#include <folly/ssl/OpenSSLHash.h>
#include <algorithm>
#include <array>
#include <cctype>
#include <string>

namespace facebook::glean::clangx::hash {

std::string hash(const std::string& input) {
  // Use folly's SHA1 implementation
  std::array<uint8_t, SHA_DIGEST_LENGTH> digest{};
  folly::ssl::OpenSSLHash::sha1(
      folly::MutableByteRange(digest.data(), digest.size()),
      folly::ByteRange(
          reinterpret_cast<const uint8_t*>(input.data()), input.size()));

  // Convert first 8 bytes to hex string (16 hex characters)
  std::string result = folly::hexlify(folly::ByteRange(digest.data(), 8));

  // Convert to uppercase to match LLVM's toHex behavior
  std::transform(result.begin(), result.end(), result.begin(), ::toupper);

  return result;
}

} // namespace facebook::glean::clangx::hash
