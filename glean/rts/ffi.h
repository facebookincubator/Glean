/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
template <typename T> struct HsArray;
#endif

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace binary {
#endif

typedef struct Output Output;

#ifdef __cplusplus
}
}
}
#endif

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace rts {
#endif

// @lint-ignore-every CLANGTIDY facebook-hte-Typedef
typedef struct Lookup Lookup;
typedef struct Define Define;
typedef struct FactSet FactSet;
typedef struct Inventory Inventory;
typedef struct LookupCache LookupCache;
typedef struct Predicate Predicate;
typedef struct Substitution Substitution;
typedef struct QueryResults QueryResults;
typedef struct OwnershipUnitIterator OwnershipUnitIterator;
typedef struct DerivedFactOwnershipIterator DerivedFactOwnershipIterator;
typedef struct Ownership Ownership;
typedef struct OwnershipStats OwnershipStats;
typedef struct DefineOwnership DefineOwnership;
typedef struct ComputedOwnership ComputedOwnership;
typedef struct Slice Slice;
typedef struct Sliced Sliced;

#ifdef __cplusplus
}
}
}
#endif

#ifdef __cplusplus
namespace facebook {
namespace glean {
namespace rts {
namespace c {

using binary::Output;
#endif

typedef struct SharedLookupCacheStats SharedLookupCacheStats;
typedef struct SharedSubroutine SharedSubroutine;

typedef struct FactCount {
  uint64_t pid;
  uint64_t count;
} FactCount;

#ifdef __cplusplus
using FactOrder = HsArray<int64_t>;
using OwnershipSet = HsArray<uint32_t>;
#else
typedef void FactOrder;
typedef void OwnershipSet;
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef int64_t glean_fact_id_t;
typedef int64_t glean_predicate_id_t;

const char *glean_inventory_new(
  size_t count,
  const int64_t *ids,
  const void * const *name_ptrs,
  const size_t *name_sizes,
  const int32_t *versions,
  SharedSubroutine * const *typecheckers,
  SharedSubroutine * const *traversals,
  Inventory **inventory
);
void glean_inventory_free(
  Inventory *inventory
);

const char *glean_inventory_predicates(
  Inventory *inventory,
  size_t *count,
  const Predicate ***predicates
);

const char *glean_inventory_serialize(
  Inventory *inventory,
  const void **data,
  size_t *size
);

const char *glean_inventory_deserialize(
  const void *data,
  size_t size,
  Inventory **inventory
);

const char *glean_inventory_equal(
  const Inventory *first,
  const Inventory *second,
  bool *result
);

const char *glean_predicate_unpack(
  Predicate *predicate,
  int64_t *id,
  const void **name,
  size_t *name_size,
  int32_t *version
);

const char *glean_snapshot_new(
  Lookup *base,
  int64_t boundary,
  Lookup **snapshot
);


const char *glean_lookupcache_stats_new(
  SharedLookupCacheStats **stats
);
void glean_lookupcache_stats_free(
  SharedLookupCacheStats *stats
);
void glean_lookupcache_stats_read_and_reset_counters(
  SharedLookupCacheStats *stats,
  uint64_t *values,
  size_t size
);


const char *glean_lookupcache_new(
  size_t capacity,
  size_t shards,
  SharedLookupCacheStats *stats,
  LookupCache **cache
);
void glean_lookupcache_free(
  LookupCache *cache
);
const char *glean_lookupcache_clear(
  LookupCache *cache
);

const char *glean_lookupcache_anchor_new(
  Lookup *base,
  LookupCache *cache,
  Lookup **anchor
);
void glean_lookupcache_anchor_free(
  Lookup *anchor
);

const char *glean_query_execute_compiled(
  Inventory *inventory,
  Define *facts,
  DefineOwnership *ownership,
  SharedSubroutine *sub,
  uint64_t pid,
  SharedSubroutine *traverse,
  uint64_t max_results,
  uint64_t max_bytes,
  uint64_t max_time_ms,
  uint64_t recursive,
  uint64_t *expand_pids,
  uint64_t num_expand_pids,
  uint64_t want_stats,
  QueryResults **presults
);

const char *glean_query_restart_compiled(
  Inventory *inventory,
  Define *facts,
  DefineOwnership *ownership,
  void *cont,
  int64_t cont_size,
  uint64_t max_results,
  uint64_t max_bytes,
  uint64_t max_time_ms,
  uint64_t recursive,
  uint64_t *expand_pids,
  uint64_t num_expand_pids,
  uint64_t want_stats,
  QueryResults **results
);

void glean_lookup_free(
  Lookup *lookup
);

const char *glean_lookup_starting_id(
  Lookup *lookup,
  int64_t *id
);
const char *glean_lookup_first_free_id(
  Lookup *lookup,
  int64_t *id
);

const char *glean_lookup_fact(
  Lookup *lookup,
  int64_t id,
  int64_t *type,
  void **key,
  size_t *key_size,
  void **value,
  size_t *value_size
);

const char *glean_define_fact(
  Define *facts,
  glean_predicate_id_t predicate,
  Output *clause,
  size_t key_size,
  glean_fact_id_t *id
);

const char *glean_define_untrusted_batch(
  Define *facts,
  Inventory *inventory,
  int64_t batch_first_id,
  const int64_t *ids,
  size_t batch_count,
  const void *batch_facts_data,
  size_t batch_facts_size,
  Substitution **subst
);

const char *glean_new_subst(
  int64_t first,
  size_t size,
  Substitution **subst
);
void glean_free_subst(
  Substitution *subst
);
const char *glean_subst_compose(
  const Substitution *first,
  const Substitution *second,
  Substitution **result
);
const char *glean_serialize_subst(
  const Substitution *subst,
  int64_t *firstId,
  size_t *count,
  int64_t **ids
);

const char *glean_subst_intervals(
  const Substitution *subst,
  const glean_fact_id_t *ins,
  size_t ins_size,
  glean_fact_id_t **outs,
  size_t *outs_size
);


const char *glean_factset_new(
  int64_t first_id,
  FactSet **facts
);
void glean_factset_free(
  FactSet *facts
);

size_t glean_factset_fact_count(
  FactSet *facts
);

size_t glean_factset_fact_memory(
  FactSet *facts
);

size_t glean_factset_allocated_memory(
  FactSet *facts
);

const char *glean_factset_predicateStats(
  FactSet *facts,
  size_t *count,
  int64_t **ids,
  uint64_t **counts,
  uint64_t **sizes);

Lookup *glean_factset_lookup(FactSet *facts);
Define *glean_factset_define(FactSet *define);

const char *glean_factset_serialize(
  FactSet *facts,
  int64_t *first_id,
  size_t *count,
  void **facts_data,
  size_t *facts_size
);

const char *glean_factset_serializeReorder(
  FactSet *facts,
  uint64_t *order,
  size_t order_size,
  int64_t *first_id,
  size_t *count,
  void **facts_data,
  size_t *facts_size
);

const char *glean_factset_append(
  FactSet *target,
  FactSet *source
);

const char *glean_stacked_lookup_new(
  Lookup *base,
  Lookup *added,
  Lookup **stacked
);

const char *glean_stacked_define_new(
  Lookup *base,
  Define *added,
  Define **stacked
);
void glean_stacked_define_free(
  Define *stacked
);


const char *glean_new_builder(
  Output **builder
);
void glean_free_builder(
  Output *builder
);

size_t glean_builder_size(
  Output *builder
);

const char *glean_finish_builder(
  Output *builder,
  void **data,
  size_t *size
);

const char *glean_reset_builder(
  Output *builder
);

const char *glean_push_value_byte(
  Output *builder,
  unsigned char val
);
const char *glean_push_value_bytes(
  Output *builder,
  const void *data,
  size_t size
);
const char *glean_push_value_nat(
  Output *builder,
  uint64_t val
);
const char *glean_push_value_array(
  Output *builder,
  size_t size
);
const char *glean_push_value_selector(
  Output *builder,
  size_t selector
);
const char *glean_push_value_string(
  Output *builder,
  const void *data,
  size_t size
);
const char *glean_push_value_fact(
  Output *builder,
  glean_fact_id_t fact
);

const char *glean_pop_value_byte(
  const void **start,
  const void *end,
  uint8_t *byte);
const char *glean_pop_value_nat(
  const void **start,
  const void *end,
  uint64_t *nat);
const char *glean_pop_value_array(
  const void **start,
  const void *end,
  size_t *size);
const char *glean_pop_value_bytes_ref(
  const void **start,
  const void *end,
  size_t size,
  const void **bytes);
const char *glean_pop_value_bytes(
  const void **start,
  const void *end,
  size_t size,
  void **bytes);
const char *glean_pop_value_selector(
  const void **start,
  const void *end,
  size_t *selector);
size_t glean_pop_value_trusted_string_ref(
  const void **start,
  const void *end);
const char *glean_pop_value_string(
  const void **start,
  const void *end,
  void **bytes,
  size_t *size);
const char *glean_pop_value_fact(
  const void **start,
  const void *end,
  glean_fact_id_t *fact);



const char *glean_push_fact(
  Output *builder,
  int64_t pid,
  Output *data,
  size_t key_size
);

size_t glean_string_demangle_trusted(
  const uint8_t *start,
  size_t size,
  uint8_t *buffer
);

void glean_free_query_results(
  QueryResults *results
);

const char *glean_subroutine_new(
  const uint64_t *code,
  size_t code_size,
  size_t inputs,
  size_t outputs,
  size_t locals,
  const uint64_t *constants_ptr,
  size_t constants_size,
  const void * const *literal_ptrs,
  const size_t *literal_sizes,
  size_t literal_count,
  SharedSubroutine **sub
);

void glean_subroutine_free(
  SharedSubroutine *sub
);

void glean_subroutine_inspect(
  SharedSubroutine *sub,
  const uint64_t **code,
  size_t *code_size,
  size_t *inputs,
  size_t *outputs,
  size_t *locals,
  const uint64_t **constants,
  size_t *constants_size,
  size_t *lit_count);

size_t glean_subroutine_size(
  SharedSubroutine *sub);

void glean_subroutine_literal(
  SharedSubroutine *sub,
  size_t index,
  const void **ptr,
  size_t *size);

const char *glean_invoke_typechecker(
  const SharedSubroutine *typechecker,
  const void *input,
  size_t input_size,
  void **output,
  size_t *output_size
);

const char *glean_validate(
  const Inventory *inventory,
  char typecheck,
  char keys,
  size_t limit,
  Lookup *lookup
);

void glean_ownership_unit_iterator_free(
  OwnershipUnitIterator *
);

void glean_derived_fact_ownership_iterator_free(
  DerivedFactOwnershipIterator *
);

const char *glean_ownership_compute(
  Inventory *inventory,
  Lookup *lookup,
  OwnershipUnitIterator *iter,
  ComputedOwnership **ownership
);

void glean_ownership_free(Ownership *own);

void glean_computed_ownership_free(ComputedOwnership *own);

const char *glean_get_ownership_set(
  Ownership *ownership,
  uint32_t uset_id,
  int *op,
  OwnershipSet **result
);

const char *glean_get_fact_owner(
  Ownership *ownership,
  glean_fact_id_t fact,
  uint32_t *uset_id
);

const char *glean_slice_compute(
  Ownership *ownership,
  uint32_t *unit_ids,
  size_t unit_ids_size,
  int exclude,
  Slice **slice
);

void glean_slice_free(Slice *slice);

const char *glean_make_sliced(
  Lookup *lookup,
  Ownership *ownership,
  Slice *slice,
  Sliced **sliced
);

void glean_sliced_free(Sliced *sliced);

const char *glean_new_define_ownership(
  Ownership *own,
  int64_t pid,
  int64_t first_id,
  DefineOwnership **result
);

const char *glean_define_ownership_subst(
  DefineOwnership *define,
  const Substitution *subst
);

const char *glean_define_ownership_sort_by_owner(
  DefineOwnership *define,
  uint64_t facts,
  FactOrder *result
);

void glean_define_ownership_free(DefineOwnership *def);

const char *glean_derived_ownership_compute(
  Ownership *own,
  DerivedFactOwnershipIterator *iter,
  ComputedOwnership **result
);

const char *glean_get_ownership_stats(
  Ownership *own,
  OwnershipStats *result
);

#ifdef __cplusplus
}
}
}
}
}
#endif
