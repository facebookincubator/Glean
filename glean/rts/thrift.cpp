/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/thrift.h"

#include <folly/Varint.h>

extern "C" {

size_t glean_thrift_encode_zigzag(
    int64_t value,
    uint8_t *buffer) {
  return folly::encodeVarint(folly::encodeZigZag(value), buffer);
}

size_t glean_thrift_encode_varint(
    uint64_t value,
    uint8_t *buffer) {
  return folly::encodeVarint(value, buffer);
}

}
