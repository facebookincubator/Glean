// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <cinttypes>
#include <cstddef>

#ifdef __cplusplus
extern "C" {
#endif

struct glean_interprocess_counters_t;

const char *glean_interprocess_counters_create(
  const char *file,
  size_t size
);

const char *glean_interprocess_counters_open(
  const char *file,
  size_t size,
  glean_interprocess_counters_t **counters
);

void glean_interprocess_counters_close(
  glean_interprocess_counters_t *counters
);

const char *glean_interprocess_counters_set(
  glean_interprocess_counters_t *counters,
  size_t index,
  uint64_t value
);

const char *glean_interprocess_counters_get(
  glean_interprocess_counters_t *counters,
  size_t index,
  uint64_t *value
);

#ifdef __cplusplus
}
#endif
