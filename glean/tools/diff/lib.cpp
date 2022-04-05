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
const char *glean_diff(Inventory *inventory, Lookup *first, Lookup *second) {
  return ffi::wrap([=] {
  const auto first_starting = first->startingId();
  const auto first_boundary = first->firstFreeId();
  FactSet additional(first_boundary);
  Stacked<Define> ext(first, &additional);

  const auto second_starting = second->startingId();
  const auto second_size = distance(second_starting, second->firstFreeId());
  Substitution subst(second_starting, second_size);
  Substituter substituter(&subst);

  size_t kept = 0;
  size_t added = 0;

  // For each fact in second, subtitute it and define it in the Extension.
  // Accumulate new fact ids in the substitution.
  for (auto i = second->enumerate(); auto ref = i->get(); i->next()) {
    auto pred = inventory->lookupPredicate(ref.type);
    CHECK(pred != nullptr);
    binary::Output out;
    uint64_t key_size;
    pred->substitute(substituter, ref.clause, out, key_size);

    auto id = ext.define(ref.type, Fact::Clause::from(out.bytes(), key_size));

    if (id < first_boundary) {
      ++kept;
    } else {
      ++added;
    }

    subst.set(ref.id, id);
  }

  std::cout
    << "kept " << kept
    << ", added " << added
    << ", removed " << distance(first_starting, first_boundary) - kept
    << std::endl;
  });
}

}
