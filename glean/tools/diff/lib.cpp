/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

#include <iostream>
#include <vector>

using namespace facebook::hs;
using namespace facebook::glean;
using namespace facebook::glean::rts;

extern "C" {
const char *glean_diff(
    Inventory *inventory,
    Lookup *first,
    Lookup *second,
    bool log_added,
    int32_t *kept_,
    int32_t *added_,
    int32_t *removed_) {
  return ffi::wrap([=] {
  const auto first_starting = first->startingId();
  const auto first_boundary = first->firstFreeId();
  const auto second_starting = second->startingId();
  const auto second_boundary = second->firstFreeId();

  const auto second_size = distance(second_starting, second->firstFreeId());
  Substitution subst(second_starting, second_size);
  const auto substitute = syscall([&subst](Id id, Pid) {
    return subst.subst(id);
  });

  size_t kept = 0;
  size_t added = 0;

  size_t percent = second_size / 100;
  size_t announce = percent; // announce every one percent

  // For each fact in second, subtitute it and define it in the Extension.
  // Accumulate new fact ids in the substitution.
  for (auto i = second->enumerate(); auto ref = i->get(); i->next()) {
    // ref enumerates all the fact id in the second db

    auto pred = inventory->lookupPredicate(ref.type);
    CHECK(pred != nullptr);
    binary::Output out;
    uint64_t key_size;
    pred->substitute(substitute, ref.clause, out, key_size);

    Fact::Clause clause = Fact::Clause::from(out.bytes(), key_size);
    Id id = first->idByKey(ref.type, clause.key());

    bool keep = false;
    if (id != Id::invalid()) {
      if (clause.value_size == 0) {
        keep = true;
      } else {
        first->factById(id, [&](Pid, Fact::Clause found) {
          if (clause.value() == found.value()) {
            keep = true;
          }
        });
      }
    }

    if (keep) {
      ++kept;
    } else {
      ++added;
      if (log_added) {
        std::cout << "Added $" << ref.id.toWord() << ":" << pred->name << std::endl;
      }
    }

    subst.set(ref.id, id);

    if (--announce == 0) {
      announce = percent;
      auto percentage = (ref.id.toWord() - second_starting.toWord()) / percent;
      std::cerr << percentage << "%" << std::endl << std::flush;
    }
  }

  // removed can be incorrect if the first db is incremental as it includes
  // facts in excluded units.
  size_t removed = distance(first_starting, first_boundary) - kept;
  *kept_ = static_cast<int32_t>(kept);
  *added_ = static_cast<int32_t>(added);
  *removed_ = static_cast<int32_t>(removed);
  });
}

}
