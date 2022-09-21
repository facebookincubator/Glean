/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/rts/ffi.h"

#ifdef __cplusplus
namespace facebook::glean::rts::benchmarking {
#endif

// @lint-ignore-every CLANGTIDY facebook-hte-Typedef
typedef struct FactBlock FactBlock;

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
namespace facebook::glean::rts::benchmarking::c {
extern "C" {
#endif

const char *glean_benchmarking_factblock_create(
  Lookup *lookup,
  FactBlock **block
);

void glean_benchmarking_factblock_free(
  FactBlock *block
);

size_t glean_benchmarking_factblock_fact_count(
  FactBlock *block
);

size_t glean_benchmarking_factblock_fact_memory(
  FactBlock *block
);

const char *glean_benchmarking_define_each(
  Define *define,
  FactBlock *block,
  bool *result
);

const char *glean_benchmarking_lookup_each_type(
  Lookup *lookup,
  FactBlock *block,
  bool *result
);

const char *glean_benchmarking_lookup_each_by_id(
  Lookup *lookup,
  FactBlock *block,
  bool *result
);

const char *glean_benchmarking_lookup_each_by_key(
  Lookup *lookup,
  FactBlock *block,
  bool *result
);

const char *glean_benchmarking_seek_to_each(
  Lookup *lookup,
  FactBlock *block,
  bool *result
);

const char *glean_benchmarking_seek_count(
  Lookup *lookup,
  const int64_t* pids,
  size_t pids_count,
  size_t *count
);


#ifdef __cplusplus
}
}
#endif