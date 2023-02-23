/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <sys/types.h>
#include <cstdint>
#ifdef OSS
#include <cpp/wrap.h> // @manual
#else
#include <common/hs/util/cpp/wrap.h>
#endif
#include "glean/rts/binary.h"
#include "glean/rts/inventory.h"
#include "glean/rts/lookup.h"
#include "glean/rts/stacked.h"
#include "glean/rts/substitution.h"
#include <folly/MPMCQueue.h>
#include <folly/executors/GlobalExecutor.h>
#include <folly/futures/Future.h>
#include <folly/Unit.h>
#include <folly/Synchronized.h>

#include <iostream>
#include <vector>
#include <future>
#include <queue>
#include <list>

using namespace facebook::hs;
using namespace facebook::glean;
using namespace facebook::glean::rts;

namespace {

// A group of facts to be deduplicated together. In the final stage we must
// process these in order, therefore they need a sequence number.
struct Batch {
  size_t sequence_number;
  std::vector<std::shared_ptr<Fact>> facts;
};

// A queue supporting multiple producers and consumers.
using Queue = folly::MPMCQueue<std::optional<Batch>, std::atomic, false>;

// Write to a queue. Blocking.
void write(Queue& q, std::optional<Batch> batch) {
  auto wait = std::chrono::seconds(60);
  auto soon = std::chrono::system_clock::now() + wait;
  bool written = q.tryWriteUntil(soon, std::move(batch));
  if (!written) {
    error("unable to write");
  }
}

void terminate_consumers(Queue& q) {
  write(q, std::nullopt);
}

// Read from a queue until it finishes.
void consume(Queue& q, std::function<void (Batch)> fn) {
  std::optional<Batch> input;
  q.blockingRead(input);
  while(input != std::nullopt) {
    fn(*input);
    q.blockingRead(input);
  }
  // leave the sentinel value in the queue to stop other threads.
  terminate_consumers(q);
}

// An efficient thread-safe substitution for blocks of consecutive ids.
// We protect substitution sections with mutexes.
size_t max_shard_size = 10000;
struct TSubstitution {
public:
  TSubstitution(Id first, size_t size_)
    : size(size_)
    , base(first)
    , shard_size(std::min(size, max_shard_size)) {
    auto shard_count = size/shard_size + std::min(1, (int) (size % shard_size));
    shards = std::vector<folly::Synchronized<Shard>>(shard_count);
    for (auto ix = 0; ix < shards.size(); ix++) {
      shards.at(ix).withWLock([&](Shard& shard) {
        auto size_this_shard = std::min(shard_size, size - (ix * shard_size));
        shard.items = std::vector<std::optional<Id>>(size_this_shard, std::nullopt);
      });
    }
  }

  Id start() const { return base; }
  Id finish() const { return base + size; }
  std::optional<Id> subst(const Id id) {
    if (id >= start() && id < finish()) {
      const auto d = distance(start(), id);
      const auto shard_index = d / shard_size;
      const auto item_index = d % shard_size;
      return shards.at(shard_index).withRLock([&](const Shard& shard) {
        return shard.items.at(item_index);
      });
    } else {
      return Id::invalid();
    }
  }

  void set(const Id pos, const Id id) {
    CHECK(pos >= start() && pos < finish());
    const size_t d = distance(start(),pos);
    const auto shard_index = d / shard_size;
    const auto item_index = d % shard_size;
    shards.at(shard_index).withWLock([&](Shard &shard) {
      shard.items.at(item_index) = id;
      if (id != Id::invalid()) {
        shard.used++;
      }
    });
  }

  size_t mapped() {
    auto total = 0;
    for (auto& shard : shards) {
      shard.withRLock([&](const Shard& shard) {
        total += shard.used;
      });
    }
    return total;
  }

private:
  struct Shard {
    std::vector<std::optional<Id>> items;
    size_t used = 0; // values mapped to a valid Id.
  };

  const size_t size;
  const Id base;
  const size_t shard_size;
  std::vector<folly::Synchronized<Shard>> shards;
};

}

extern "C" {
const char *glean_diff(
    Inventory *second_inventory,
    Lookup *first_lookup,
    Lookup *second_lookup,
    bool log_added,
    size_t batch_size, // how many facts to deduplicate together
    size_t *kept_,
    size_t *added_,
    size_t *removed_) {
  return ffi::wrap([=] {
  const auto first_starting = first_lookup->startingId();
  const auto first_boundary = first_lookup->firstFreeId();
  const auto second_starting = second_lookup->startingId();
  const auto second_boundary = second_lookup->firstFreeId();

  const auto second_size = distance(second_starting, second_boundary);
  const auto first_size = distance(first_starting, first_boundary);
  TSubstitution subst(second_starting, second_size);

  const auto parallel_dedupes = 25;

  // The queue_undeduped queue can be as large as we want, but must not be too small
  // otherwise dedupers might lay idle.
  Queue queue_undeduped(20 * parallel_dedupes);

  // We want the writer to always have things to deduplicate but the larger this queue the
  // less useful are the susbstituions that we have. So we want to keep it as low as possible
  // without ever allowing the writer to be blocked.
  Queue queue_deduped(20 * parallel_dedupes);


  enum class SearchResult { Found, NotFound, Invalid };

  // Find a fact from 'second' in 'first' and save the id mapping if found. If
  // we don't have mappings for some of the referenced ids, they get mapped to
  // Id::invalid. This guarantees that we won't find any match when searching
  // in the DB.
  const auto find = [&](const std::shared_ptr<Fact>& fact_second) {
    auto pred = second_inventory->lookupPredicate(fact_second->type());
    CHECK(pred != nullptr);
    binary::Output out;
    uint64_t key_size;

    bool has_missing_reference = false;
    bool has_invalid_reference = false;
    const auto substitute = syscall([&](Id id, Pid) {
      if (has_invalid_reference || has_missing_reference) {
        return Id::invalid();
      }
      auto found = subst.subst(id);
      if (found) {
        has_invalid_reference = *found == Id::invalid();
        return *found;
      } else {
        has_missing_reference = true;
        return Id::invalid();
      }
    });

    pred->substitute(substitute, fact_second->clause(), out, key_size);
    if (has_invalid_reference) {
      return SearchResult::Invalid;
    }
    if (has_missing_reference) {
      return SearchResult::NotFound;
    }

    // clause from 'second' with translated ids.
    Fact::Clause clause_second = Fact::Clause::from(out.bytes(), key_size);

    // assumes that both lookups use the same Pids.
    Pid type_first = fact_second->type();
    Id id = first_lookup->idByKey(type_first, clause_second.key());

    bool found = false;
    if (id != Id::invalid()) {
      if (clause_second.value_size == 0) {
        found = true;
      } else {
        first_lookup->factById(id, [&](Pid, Fact::Clause clause_first) {
          found = clause_second.value() == clause_first.value();
        });
      }
    }

    // At this point the fact has no missing or invalid references so the
    // search result will be final.
    subst.set(fact_second->id(), id);
    if (found) {
      return SearchResult::Found;
    } else {
      return SearchResult::Invalid;
    }
  };

  const auto dedupe = [&](Batch batch) {
    std::vector<std::shared_ptr<Fact>> not_found {};
    for (auto const& fact : batch.facts) {
      SearchResult found = find(fact);

      if (found == SearchResult::NotFound) {
        not_found.push_back(fact);
      }
    }

    return Batch { batch.sequence_number, not_found };
  };

  auto executor = folly::getGlobalCPUExecutor();
  // start parallel deduplication
  folly::Future<folly::Unit> initial_dedupe = folly::via(executor, [&](){
    std::vector<folly::Future<folly::Unit>> workers {};
    for (auto i = 0; i < parallel_dedupes; i++) {
      workers.push_back(folly::via(executor, [&](){
        consume(queue_undeduped, [&](Batch batch) {
          write(queue_deduped, dedupe(batch));
        });
        return folly::unit;
      }));
    }

    // wait for deduplication to finish
    folly::collect(std::move(workers)).get();
    terminate_consumers(queue_deduped);
    return folly::unit;
  });

  size_t one_percent = second_size / (100 * batch_size);
  size_t announce = one_percent; // announce on every one percent of batches

  auto cmp = [](const Batch& one, const Batch& two) {
    return one.sequence_number > two.sequence_number;
  };
  // min-heap
  std::priority_queue<Batch, std::vector<Batch>, decltype(cmp)> queued {};
  size_t next_index = 0;
  // start final single-threaded deduplication
  folly::Future<folly::Unit> final_dedupe = folly::via(executor, [&](){
    consume(queue_deduped, [&](Batch batch) {
      queued.push(batch);
      while (!queued.empty() && queued.top().sequence_number == next_index) {
        auto next = queued.top();
        auto remaining = dedupe(next);
        queued.pop();
        next_index++;

        if (--announce == 0) {
          announce = one_percent;
          auto percentage = next.sequence_number / one_percent;
          std::cerr << percentage << "%" << std::endl << std::flush;
        }
      }
    });
    return folly::unit;
  });

  folly::Future<folly::Unit> read_db = folly::via(executor, [&](){
    auto i = second_lookup->enumerate();
    auto ref = i->get();
    size_t batch_ix = 0;
    while (ref.id != Id::invalid()) {
      Batch batch { batch_ix, std::vector<std::shared_ptr<Fact>>() };
      batch.facts.reserve(batch_size);

      // fill one batch
      while (ref && batch.facts.size() < batch_size) {
        batch.facts.push_back(Fact::create(ref));
        i->next();
        ref = i->get();
      }

      write(queue_undeduped, batch);
      batch_ix++;
    }
    terminate_consumers(queue_undeduped);
    return folly::unit;
  });

  folly::collect(read_db, initial_dedupe, final_dedupe).get();

  size_t kept = subst.mapped();
  *kept_ = kept;
  *added_ = second_size - kept;
  // removed can be incorrect if the first db is incremental as it includes
  // facts in excluded units.
  *removed_ = first_size - kept;
  });
}

}
