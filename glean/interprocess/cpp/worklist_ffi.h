// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include <cinttypes>
#include <cstddef>

#ifdef __cplusplus
extern "C" {
#endif

struct glean_interprocess_worklist_t;

const char *glean_interprocess_worklist_create(
  const char *file,
  size_t count,
  const uint32_t *indices,
  const uint32_t *sizes
);

const char *glean_interprocess_worklist_open(
  const char *file,
  glean_interprocess_worklist_t **worklist
);

void glean_interprocess_worklist_close(
  glean_interprocess_worklist_t *worklist
);

void glean_interprocess_worklist_get(
  const glean_interprocess_worklist_t *worklist,
  size_t worker,
  uint32_t *start,
  uint32_t *end
);

void glean_interprocess_worklist_next(
  glean_interprocess_worklist_t *worklist,
  size_t worker,
  uint32_t *start,
  uint32_t *end,
  size_t *victim
);

#ifdef __cplusplus
}
#endif
