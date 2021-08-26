// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

size_t glean_thrift_encode_zigzag(
  int64_t value,
  uint8_t *buffer);
size_t glean_thrift_encode_varint(
  uint64_t value,
  uint8_t *buffer);

#ifdef __cplusplus
}
#endif
