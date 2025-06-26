/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#ifdef OSS
#include <cpp/wrap.h> // @manual
#else
#include <common/hs/util/cpp/wrap.h>
#endif

#include <boost/algorithm/string.hpp>
#include <folly/String.h>
#include <folly/ssl/OpenSSLHash.h>
#include <string>

using namespace facebook::hs;

namespace facebook::glean::clangx::hash {

// First 8 bytes of the SHA1 hash of the input, as upper-case ASCII hex
// because... presumably reasons.
std::string hash(folly::ByteRange input) {
  std::array<uint8_t, 20> digest; // SHA-1 produces 20 bytes
  folly::ssl::OpenSSLHash::sha1(folly::range(digest), input);
  auto hash = folly::hexlify(folly::range(digest.data(), digest.data() + 8));
  boost::to_upper(hash);
  return hash;
}

} // namespace facebook::glean::clangx::hash

extern "C" {

const char *hash_ffi(const char *input, // nul-terminated
                     char *output, size_t output_size, int *result) {
  return ffi::wrap([=] {
    auto str = facebook::glean::clangx::hash::hash(
        folly::range(input, input + strlen(input)));
    if (str.length() + 1 > output_size) {
      *result = -1; // Buffer too small
    }
    strcpy(output, str.c_str());
    *result = 0; // Success
  });
}

} // extern "C"
