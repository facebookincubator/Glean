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
#include "glean/schema/thrift/gen-cpp2/glean_test_types.h"

#include <folly/Range.h>
#include <thrift/lib/cpp2/protocol/Serializer.h>

using namespace facebook::hs;
using namespace facebook::glean;

extern "C" {

const char* glean_test_compact_reencode(
    const uint8_t* data,
    size_t size,
    uint8_t** out_data,
    size_t* out_size) {
  return ffi::wrap([=] {
    size_t consumed;
    auto fact = apache::thrift::CompactSerializer::deserialize<
        schema::glean::test::Predicate>(
        folly::ByteRange(data, size), &consumed);
    if (consumed != size) {
      throw std::logic_error("extra bytes");
    }
    ffi::clone_bytes(
        apache::thrift::CompactSerializer::serialize<std::string>(fact))
        .release_to(out_data, out_size);
  });
}
}
