#include "glean/rts/ffi.h"
#include "glean/ffi/memory.h"
#include "glean/ffi/wrap.h"
#include "glean/if/gen-cpp2/glean_types.h"
#include "glean/rts/bytecode/subroutine.h"
#include "glean/rts/cache.h"
#include "glean/rts/ffi.h"
#include "glean/rts/id.h"
#include "glean/rts/lookup.h"
#include "glean/rts/ownership.h"
#include "glean/rts/query.h"
#include "glean/rts/sanity.h"
#include "glean/rts/stacked.h"
#include "glean/rts/string.h"
#include "glean/rts/substitution.h"
#include "glean/rts/validate.h"

#include <folly/Exception.h>
#include <vector>
#include <algorithm>

using namespace facebook::glean;
using namespace facebook::glean::rts;

namespace facebook {
namespace glean {
namespace rts {
namespace c {

struct SharedLookupCacheStats {
  std::shared_ptr<facebook::glean::rts::LookupCache::Stats> value;
};

struct SharedSubroutine {
  std::shared_ptr<facebook::glean::rts::Subroutine> value;
};

namespace {

template<typename F>
const char *pop_value(const void **start, const void *end, F f) {
  return ffi::wrap([=]() {
    binary::Input input(*start, end);
    f(input);
    *start = input.data();
  });
}

}

extern "C" {

const char *glean_inventory_new(
    size_t count,
    const int64_t *ids,
    const void * const *name_ptrs,
    const size_t *name_sizes,
    const int32_t *versions,
    SharedSubroutine * const *typecheckers,
    SharedSubroutine * const *traversals,
    Inventory **inventory) {
  return ffi::wrap([=]{
    std::vector<rts::Predicate> predicates;
    predicates.reserve(count);
    for (size_t i = 0; i < count; ++i) {
      predicates.push_back(rts::Predicate{
        Pid::fromThrift(ids[i]),
        std::string(static_cast<const char *>(name_ptrs[i]), name_sizes[i]),
        versions[i],
        typecheckers[i]->value,
        traversals[i]->value
      });
    }
    *inventory = new Inventory(std::move(predicates));
  });
}

void glean_inventory_free(Inventory *inventory) {
  ffi::free_(inventory);
}

const char *glean_inventory_predicates(
    Inventory *inventory,
    size_t *count,
    const Predicate ***predicates) {
  return ffi::wrap([=] {
    const auto preds = inventory->predicates();
    const auto n = preds.size();
    *count = n;
    *predicates = ffi::clone_array(preds.data(), preds.size()).release();
  });
}

const char *glean_inventory_serialize(
    Inventory *inventory,
    const void **data,
    size_t *size) {
  return ffi::wrap([=] {
    ffi::clone_bytes(inventory->serialize()).release_to(data, size);
  });
}

const char *glean_inventory_deserialize(
    const void *data,
    size_t size,
    Inventory **inventory) {
  return ffi::wrap([=] {
    *inventory = new Inventory(Inventory::deserialize(
      {reinterpret_cast<const unsigned char *>(data), size}));
  });
}


const char *glean_predicate_unpack(
    Predicate *predicate,
    int64_t *id,
    const void **name,
    size_t *name_size,
    int32_t *version) {
  return ffi::wrap([=] {
    *id = predicate->id.toThrift();
    *name = predicate->name.data();
    *name_size = predicate->name.size();
    *version = predicate->version;
  });
}

const char *glean_inventory_equal(
    const Inventory *first,
    const Inventory *second,
    bool *result) {
  return ffi::wrap([=] {
    *result = *first == *second;
  });
}

const char *glean_snapshot_new(
    Lookup *base,
    int64_t boundary,
    Lookup **snapshot) {
  return ffi::wrap([=] {
    *snapshot = new Snapshot(base, Id::fromThrift(boundary));
  });
}

const char *glean_lookupcache_stats_new(SharedLookupCacheStats **stats) {
  return ffi::wrap([=] {
    *stats = new SharedLookupCacheStats{
      std::make_shared<LookupCache::Stats>()
    };
  });
}

void glean_lookupcache_stats_free(SharedLookupCacheStats *stats) {
    ffi::free_(stats);
}

void glean_lookupcache_stats_read_and_reset_counters(
    SharedLookupCacheStats *stats,
    uint64_t *values,
    size_t size) {
  auto buffer = stats->value->readAndResetCounters();
  if (size == buffer.size()) {
    std::copy(buffer.begin(), buffer.end(), values);
  } else {
    LOG(ERROR)
      << "glean_lookupcache_stats_read_and_reset_counters: invalid size";
    std::fill(values, values+size, 0);
  }
}


const char *glean_lookupcache_new(
    size_t capacity,
    size_t shards,
    SharedLookupCacheStats *stats,
    LookupCache **cache) {
  return ffi::wrap([=] {
    *cache = new LookupCache(
      LookupCache::Options{capacity, shards},
      stats->value);
  });
}

void glean_lookupcache_free(LookupCache *cache) {
  ffi::free_(cache);
}

const char *glean_lookupcache_clear(LookupCache *cache) {
  return ffi::wrap([=] {
    cache->clear();
  });
}

const char *glean_lookupcache_anchor_new(
    Lookup *base,
    LookupCache *cache,
    Lookup **anchor) {
  return ffi::wrap([=] {
    *anchor = new LookupCache::Anchor(cache->anchor(base));
  });
}

void glean_lookupcache_anchor_free(Lookup *anchor) {
  ffi::free_(anchor);
}

void glean_interrupt_running_queries() {
  interruptRunningQueries();
}

const char *glean_query_execute_compiled(
    Inventory *inventory,
    Define *facts,
    SharedSubroutine *sub,
    uint64_t pid,
    SharedSubroutine *traverse,
    uint64_t max_results,
    uint64_t max_bytes,
    uint64_t max_time_ms,
    uint64_t depth,
    uint64_t *expand_pids,
    uint64_t num_expand_pids,
    uint64_t want_stats,
    QueryResults **presults
) {
  return ffi::wrap([=]() {
    std::unordered_set<Pid, folly::hasher<Pid>> expandPids;
    if (expand_pids) {
      expandPids = std::unordered_set<Pid, folly::hasher<Pid>>(
          reinterpret_cast<Pid*>(expand_pids),
          reinterpret_cast<Pid*>(expand_pids) + num_expand_pids);
    }
    auto results =
      executeQuery(
        *inventory,
        *facts,
        *(sub->value),
        Pid::fromWord(pid),
        traverse ? traverse->value : nullptr,
        max_results == 0 ? folly::none : folly::Optional<uint64_t>(max_results),
        max_bytes == 0 ? folly::none : folly::Optional<uint64_t>(max_bytes),
        max_time_ms == 0 ? folly::none : folly::Optional<uint64_t>(max_time_ms),
        static_cast<Depth>(depth),
        expandPids,
        want_stats,
        folly::none
      );
    *presults = new QueryResults(std::move(results));
  });
}

const char *glean_query_restart_compiled(
    Inventory *inventory,
    Define *facts,
    void *cont,
    int64_t cont_size,
    uint64_t max_results,
    uint64_t max_bytes,
    uint64_t max_time_ms,
    uint64_t depth,
    uint64_t *expand_pids,
    uint64_t num_expand_pids,
    uint64_t want_stats,
    QueryResults **presults
) {
  return ffi::wrap([=]() {
    std::unordered_set<Pid, folly::hasher<Pid>> expandPids;
    if (expand_pids) {
      expandPids = std::unordered_set<Pid, folly::hasher<Pid>>(
          reinterpret_cast<Pid*>(expand_pids),
          reinterpret_cast<Pid*>(expand_pids) + num_expand_pids);
    }
    auto results =
      restartQuery(
        *inventory,
        *facts,
        max_results == 0 ? folly::none : folly::Optional<uint64_t>(max_results),
        max_bytes == 0 ? folly::none : folly::Optional<uint64_t>(max_bytes),
        max_time_ms == 0 ? folly::none : folly::Optional<uint64_t>(max_time_ms),
        static_cast<Depth>(depth),
        expandPids,
        want_stats,
        cont, cont_size
      );
    *presults = new QueryResults(std::move(results));
  });
}

void glean_lookup_free(Lookup *lookup) {
  ffi::free_(lookup);
}

const char *glean_lookup_empty(Lookup** lookup) {
  return ffi::wrap([=] { *lookup = new EmptyLookup(); });
}

const char *glean_lookup_starting_id(Lookup *lookup, int64_t *id) {
  return ffi::wrap([=]{
    *id = lookup->startingId().toThrift();
  });
}

const char *glean_lookup_first_free_id(Lookup *lookup, int64_t *id) {
  return ffi::wrap([=]{
    *id = lookup->firstFreeId().toThrift();
  });
}

const char *glean_lookup_fact(
    Lookup *lookup,
    int64_t id,
    int64_t *type,
    void **key,
    size_t *key_size,
    void **value,
    size_t *value_size) {
  return ffi::wrap([=]() {
    ffi::malloced_array<uint8_t> key_bytes;
    ffi::malloced_array<uint8_t> value_bytes;
    auto found = lookup->factById(
      Id::fromThrift(id),
      [&](auto ty, auto clause) {
        *type = ty.toThrift();
        key_bytes = ffi::clone_bytes(clause.key());
        value_bytes = ffi::clone_bytes(clause.value());
        return true;
      }
    );
    if (!found) {
      *type = 0;
    }
    key_bytes.release_to(key, key_size);
    value_bytes.release_to(value, value_size);
  });
}

const char *glean_define_fact(
    Define *facts,
    glean_predicate_id_t predicate,
    Output *clause,
    size_t key_size,
    glean_fact_id_t *id) {
  return ffi::wrap([=]{
    assert(key_size <= clause->size());
    *id = facts->define(
      Pid::fromThrift(predicate),
      Fact::Clause::from(clause->bytes(), key_size)).toThrift();
  });
}

const char *glean_define_untrusted_batch(
    Define *facts,
    Inventory *inventory,
    int64_t batch_first_id,
    const int64_t *ids,
    size_t batch_count,
    const void *batch_facts_data,
    size_t batch_facts_size,
    Substitution **subst) {
  return ffi::wrap([=] {
    *subst = new Substitution(
      defineUntrustedBatch(
        *facts,
        *inventory,
        Id::fromThrift(batch_first_id),
        reinterpret_cast<const Id*>(ids),
        batch_count,
        folly::ByteRange(
          static_cast<const unsigned char *>(batch_facts_data),
          batch_facts_size)));
  });
}


const char *glean_new_subst(
    int64_t first,
    size_t size,
    Substitution **subst) {
  return ffi::wrap([=]() {
    *subst = new Substitution(Id::fromThrift(first), size);
  });
}
void glean_free_subst(Substitution *subst) {
  ffi::free_(subst);
}

const char *glean_subst_compose(
    const Substitution *first,
    const Substitution *second,
    Substitution **result) {
  return ffi::wrap([=] {
    *result = new Substitution(Substitution::compose(*first, *second));
  });
}

const char *glean_serialize_subst(
    const Substitution *subst,
    int64_t *firstId,
    size_t *count,
    int64_t **ids) {
  return ffi::wrap([=]() {
    thrift::Subst s = subst->serialize();
    *firstId = s.get_firstId();
    *count = s.get_ids().size();
    *ids = ffi::clone_array(s.get_ids().data(), *count).release();
  });
}

const char *glean_subst_intervals(
    const Substitution *subst,
    const glean_fact_id_t *ins,
    size_t ins_size,
    glean_fact_id_t **outs,
    size_t *outs_size) {
  return ffi::wrap([=] {
    std::vector<Id> ids;
    ids.reserve(ins_size);
    std::transform(
      ins,
      ins+ins_size,
      std::back_inserter(ids),
      Id::fromThrift);
    auto res = subst->substIntervals(ids);
    auto fres = ffi::malloc_array<glean_fact_id_t>(res.size());
    std::transform(res.begin(), res.end(), fres.get(), [](auto id) { return id.toThrift(); });
    fres.release_to(outs, outs_size);
  });
}

const char *glean_factset_new(
    int64_t first_id,
    FactSet **facts) {
  return ffi::wrap([=] {
    *facts = new FactSet(Id::fromThrift(first_id));
  });
}

void glean_factset_free(FactSet *facts) {
  ffi::free_(facts);
}

size_t glean_factset_fact_memory(FactSet *facts) {
  return facts->factMemory();
}

int64_t glean_factset_first_free_id(FactSet *facts) {
  return facts->firstFreeId().toThrift();
}

Lookup *glean_factset_lookup(FactSet *facts) {
  return facts;
}

Define *glean_factset_define(FactSet *facts) {
  return facts;
}

const char *glean_factset_serialize(
    FactSet *facts,
    int64_t *first_id,
    size_t *count,
    void **facts_data,
    size_t *facts_size) {
  return ffi::wrap([=] {
    auto batch = facts->serialize();
    *first_id = batch.get_firstId();
    *count = batch.get_count();
    ffi::clone_bytes(batch.get_facts()).release_to(facts_data, facts_size);
  });
}

const char* glean_factset_rebase(
    FactSet* facts,
    const Inventory* inventory,
    int64_t firstId,
    size_t count,
    int64_t* ids,
    LookupCache* cache,
    FactSet** result) {
  return ffi::wrap([=] {
    thrift::Subst thrift_subst;
    auto subst_vec = std::vector<int64_t>();
    // TODO: Remove this copy
    subst_vec.insert(subst_vec.end(), &ids[0], &ids[count]);
    thrift_subst.firstId_ref() = firstId;
    thrift_subst.ids_ref() = subst_vec;
    Substitution subst = Substitution::deserialize(thrift_subst);
    GLEAN_SANITY_CHECK(subst.sanityCheck(false));
    *result = nullptr;
    cache->withBulkStore([&](auto& store) {
      GLEAN_SANITY_CHECK(facts->sanityCheck());
      *result = new FactSet(facts->rebase(*inventory, subst, store));
      GLEAN_SANITY_CHECK((*result)->sanityCheck());
    });
  });
}

const char *glean_factset_append(
    FactSet *target,
    FactSet *source) {
  return ffi::wrap([=] {
    target->append(std::move(*source));
  });
}


const char *glean_stacked_lookup_new(
    Lookup *base,
    Lookup *added,
    Lookup **stacked) {
  return ffi::wrap([=] {
    *stacked = new Stacked<Lookup>(base, added);
  });
}

const char *glean_stacked_define_new(
    Lookup *base,
    Define *added,
    Define **stacked) {
  return ffi::wrap([=] {
    *stacked = new Stacked<Define>(base, added);
  });
}

void glean_stacked_define_free(Define *stacked) {
  return ffi::free_(stacked);
}


const char *glean_new_builder(Output **builder) {
  return ffi::wrap([=]() {
    *builder = new Output;
  });
}

void glean_free_builder(Output *builder) {
  ffi::free_(builder);
}

size_t glean_builder_size(Output *builder) {
  return builder->size();
}

const char *glean_finish_builder(
    Output *builder,
    void **data,
    size_t *size) {
  return ffi::wrap([=]() {
    ffi::clone_bytes(builder->bytes()).release_to(data, size);
  });
}

const char *glean_reset_builder(Output *builder) {
  return ffi::wrap([=]() {
    *builder = binary::Output();
  });
}

const char *glean_push_value_byte(Output *builder, unsigned char val) {
  return ffi::wrap([=]() {
    builder->fixed<unsigned char>(val);
  });
}

const char *glean_push_value_bytes(
    Output *builder,
    const void *data,
    size_t size) {
  return ffi::wrap([=]() {
    builder->bytes(data,size);
  });
}

const char *glean_push_value_nat(Output *builder, uint64_t val) {
  return ffi::wrap([=]() {
    builder->packed(val);
  });
}

const char *glean_push_value_array(Output *builder, size_t size) {
  return ffi::wrap([=]() {
    builder->packed(size);
  });
}

const char *glean_push_value_selector(
    Output *builder,
    size_t selector) {
  return ffi::wrap([=]() {
    builder->packed(selector);
  });
}

const char *glean_push_value_string(
    Output *builder,
    const void *data,
    size_t size) {
  return ffi::wrap([=]() {
    builder->mangleString(folly::ByteRange(
      static_cast<const unsigned char *>(data),size));
  });
}

const char *glean_push_value_fact(
    Output *builder,
    glean_fact_id_t fact) {
  return ffi::wrap([=]() {
    builder->packed(Id::fromThrift(fact));
  });
}

const char *glean_pop_value_byte(
    const void **start,
    const void *end,
    uint8_t *byte) {
  return pop_value(start, end, [=](binary::Input& input) {
    *byte = input.fixed<uint8_t>();
  });
}

const char *glean_pop_value_nat(
    const void **start,
    const void *end,
    uint64_t *nat) {
  return pop_value(start, end, [=](binary::Input& input) {
    *nat = input.packed<uint64_t>();
  });
}

const char *glean_pop_value_array(
    const void **start,
    const void *end,
    size_t *size) {
  return pop_value(start, end, [=](binary::Input& input) {
    *size = input.packed<size_t>();
  });
}

const char *glean_pop_value_bytes_ref(
    const void **start,
    const void *end,
    size_t size,
    const void **bytes) {
  return pop_value(start, end, [=](binary::Input& input) {
    *bytes = input.bytes(size).data();
  });
}

const char *glean_pop_value_bytes(
    const void **start,
    const void *end,
    size_t size,
    void **bytes) {
  return pop_value(start, end, [=](binary::Input& input) {
    *bytes = ffi::clone_bytes(input.bytes(size).data(), size).release();
  });
}

const char *glean_pop_value_selector(
    const void **start,
    const void *end,
    size_t *selector) {
  return pop_value(start, end, [=](binary::Input& input) {
    *selector = input.packed<size_t>();
  });
}

const char *glean_pop_value_string(
    const void **start,
    const void *end,
    void **bytes,
    size_t *size) {
  return pop_value(start, end, [=](binary::Input& input) {
    binary::Output output;
    input.demangleUntrustedString(output);
    output.moveBytes().release_to(bytes,size);
  });
}

size_t glean_pop_value_trusted_string_ref(
    const void **start,
    const void *end) {
  const auto p = static_cast<const unsigned char *>(*start);
  auto r = skipTrustedString({p, static_cast<const unsigned char *>(end)});
  *start = p + r.first;
  return r.second;
}

const char *glean_pop_value_fact(
    const void **start,
    const void *end,
    glean_fact_id_t *fact) {
  return pop_value(start, end, [=](binary::Input& input) {
    *fact = input.packed<Id>().toThrift();
  });
}


const char *glean_push_fact(
    Output *builder,
    int64_t pid,
    Output *clause,
    size_t key_size) {
  return ffi::wrap([=] {
    CHECK_GE(clause->size(), key_size);
    Fact::serialize(
      *builder,
      Pid::fromThrift(pid),
      Fact::Clause::from(clause->bytes(), key_size));
  });
}


size_t glean_string_demangle_trusted(
    const uint8_t *start,
    size_t size,
    uint8_t *buffer) {
  return demangleTrustedString({start, size}, buffer);
}


void glean_free_query_results(QueryResults *results) {
  ffi::free_(results);
}


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
    SharedSubroutine **sub) {
  return ffi::wrap([=] {
    std::vector<uint64_t> constants(
      constants_ptr, constants_ptr + constants_size);
    std::vector<std::string> literals;
    literals.reserve(literal_count);
    for (size_t i = 0; i < literal_count; ++i) {
      literals.push_back(std::string(
        static_cast<const char *>(literal_ptrs[i]),
        literal_sizes[i]));
    }
    *sub = new SharedSubroutine{std::make_shared<Subroutine>(Subroutine{
      std::vector<uint64_t>(code, code + code_size),
      inputs,
      outputs,
      locals,
      std::move(constants),
      std::move(literals)
    })};
  });
}
void glean_subroutine_free(SharedSubroutine *sub) {
  ffi::free_(sub);
}

void glean_subroutine_inspect(
    SharedSubroutine *sub,
    const uint64_t **code,
    size_t *code_size,
    size_t *inputs,
    size_t *outputs,
    size_t *locals,
    const uint64_t **constants,
    size_t *constants_size,
    size_t *lit_count) {
  *code = sub->value->code.data();
  *code_size = sub->value->code.size();
  *inputs = sub->value->inputs;
  *outputs = sub->value->outputs;
  *locals = sub->value->locals;
  *constants = sub->value->constants.data();
  *constants_size = sub->value->constants.size();
  *lit_count = sub->value->literals.size();
}

size_t glean_subroutine_size(
    SharedSubroutine *sub) {
  return sub->value->size();
}

void glean_subroutine_literal(
    SharedSubroutine *sub,
    size_t index,
    const void **ptr,
    size_t *size) {
  if (index < sub->value->literals.size()) {
    *ptr = sub->value->literals[index].data();
    *size = sub->value->literals[index].size();
  } else {
    *ptr = nullptr;
    *size = 0;
  }
}

const char *glean_invoke_typechecker(
    const SharedSubroutine *typechecker,
    const void *input,
    size_t input_size,
    void **output,
    size_t *output_size) {
  return ffi::wrap([=] {
    assert(typechecker->value->inputs == 4);

    const std::function<uint64_t(uint64_t,uint64_t)> rename =
      [](uint64_t id, uint64_t) { return id; };
    binary::Output out;

    const uint64_t args[] = {
      reinterpret_cast<uint64_t>(&rename),
      reinterpret_cast<uint64_t>(input),
      reinterpret_cast<uint64_t>(input) + input_size,
      reinterpret_cast<uint64_t>(&out)
    };

    typechecker->value->execute(args);

    ffi::clone_bytes(out.bytes()).release_to(output, output_size);
  });
}

const char *glean_validate(
    const Inventory *inventory,
    char typecheck,
    char keys,
    size_t limit,
    Lookup *lookup) {
  return ffi::wrap([=] {
    Validate v;
    v.typecheck = typecheck != 0;
    v.keys = keys != 0;
    v.limit = limit;
    validate(*inventory, v, *lookup);
  });
}

void glean_ownership_unit_iterator_free(OwnershipUnitIterator *iter) {
  ffi::free_(iter);
}

const char *glean_ownership_compute(
    Inventory *inventory,
    Lookup *lookup,
    OwnershipUnitIterator *iter,
    MemoryOwnership **result
) {
  return ffi::wrap([=] {
    *result = computeOwnership(*inventory, *lookup, iter).release();
  });
}

void glean_ownership_free(Ownership *own) {
  ffi::free_(own);
}

const char *glean_slice_compute(
  Ownership *ownership,
  uint32_t *unit_ids,
  size_t unit_ids_size,
  int exclude,
  Slice **result) {
  return ffi::wrap([=] {
    auto vec = std::vector<uint32_t>(unit_ids, unit_ids + unit_ids_size);
    std::sort(vec.begin(), vec.end());
    *result = slice(ownership, vec, exclude != 0).release();
  });
}

void glean_slice_free(Slice *slice) {
  ffi::free_(slice);
}


const char *glean_make_sliced(
  Lookup *lookup,
  Ownership *ownership,
  Slice *slice,
  Sliced **sliced) {
  return ffi::wrap([=] {
    *sliced = new Sliced(lookup, ownership, slice);
  });
}

void glean_sliced_free(Sliced *sliced) {
  ffi::free_(sliced);
}


}
}
}
}
}
