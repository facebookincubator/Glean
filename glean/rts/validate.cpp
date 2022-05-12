/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/binary.h"
#include "glean/rts/validate.h"

#include <vector>
#include <future>
#include <mutex>

namespace facebook {
namespace glean {
namespace rts {

namespace {

struct Fail {
  std::atomic<bool> has_err = false;

  template <class... Args>
  void operator()(std::string fmt, Args&&... args) {
    has_err.store(true);
    rts::error(fmt, std::forward<Args>(args)...);
  }

  operator bool() {
    return has_err.load();
  }
};

}

// Validates facts from startingId until firstFreeId until all items have been
// validated or the validation limit is reached.
void validate(const Inventory& inventory, const Validate& val, Lookup& facts) {
  const auto starting_id = facts.startingId();
  const auto first_free = facts.firstFreeId();

  const auto end_id =
    val.limit < first_free - starting_id
      ? starting_id + val.limit
      : first_free;
  size_t elems = distance(starting_id, end_id);

  size_t max_shard_size = 10000;
  size_t lock_shard_size = std::min(elems, max_shard_size);
  std::vector<std::mutex> locks((elems / lock_shard_size) + 1);
  std::vector<Pid> types(elems, Pid::invalid());

  Fail fail;

  // thread-safe type cache check
  auto expect_type([&](Id id, Pid type) {
    if (id < starting_id || id >= end_id) {
      fail("id {} out of range, expecting id of type {}", id, type);
    }
    auto ix = distance(starting_id, id);
    size_t lock_ix = ix / lock_shard_size;
    const std::lock_guard<std::mutex> lock(locks.at(lock_ix));

    if (types.at(ix) == Pid::invalid()) {
      types[ix] = type;
    }

    auto real_type = types.at(ix);
    if (type != real_type) {
      fail("fact type mismatch");
    }

    return id;
  });

  Renamer checker(expect_type);

  std::atomic<size_t> count = 0;

  size_t last_percent = 0;
  std::mutex percent_lock;
  auto report_progress([&]() {
    const std::lock_guard<std::mutex> lock(percent_lock);
    size_t percent = (100 * count.load()) / elems;
    if (percent != last_percent) {
      last_percent = percent;
      VLOG(1) << percent << "%";
    }
  });

  auto validate_section([&](Id from, Id to) {
    auto allowed_id = from;

    for (auto i = facts.enumerate(from, to); auto fact = i->get(); i->next()) {
      if (fail) {
        break;
      }
      ++count;

      VLOG(3) << "[" << fact.id.toWord() << "]";

      if (fact.id < allowed_id) {
        fail("enumeration out of order");
      }
      if (fact.id >= end_id) {
        fail("fact id out of bounds");
      }

      if (val.typecheck) {
        const auto *predicate = inventory.lookupPredicate(fact.type);
        if (predicate == nullptr) {
          fail("invalid predicate");
        }

        binary::Output out;
        uint64_t key_size;

        predicate->typecheck(checker, fact.clause, out, key_size);

        if (fact.clause.bytes() != out.bytes()) {
          fail("invalid fact");
        }
        if (fact.clause.key_size != key_size) {
          fail("key size mismatch");
        }
      }

      if (val.keys && facts.idByKey(fact.type, fact.key()) != fact.id) {
        fail("idByKey mismatch");
      }

      allowed_id = fact.id + 1;

      expect_type(fact.id, fact.type);
    }
  });

  const auto max_parallel = 10;
  size_t min_ids_per_section = 100000;
  auto ids_per_section = std::min(min_ids_per_section, elems / max_parallel);
  std::atomic<int> section_no = 0;
  auto get_next_section_starting_id([&]() {
    int section = section_no++;
    return starting_id + section * ids_per_section;
  });

  // validate multiple sections
  auto worker([&]() {
    while (true) {
      Id from = get_next_section_starting_id();
      Id to = std::min(from + ids_per_section, end_id);
      VLOG(2) << "worker: " << from.toWord() << "-" << to.toWord();

      if (from >= end_id || fail) {
        break;
      }

      validate_section(from, to);
      report_progress();
    }
  });

  std::vector<std::future<void>> workers(max_parallel);
  // start workers
  for (auto i = 0; i < max_parallel; i++) {
    workers[i] = std::async(worker);
  }

  for (auto& w : workers) {
    w.get();
  }
}

}
}
}
