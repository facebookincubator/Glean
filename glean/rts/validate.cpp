/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/binary.h"
#include "glean/rts/validate.h"

#include <vector>

namespace facebook {
namespace glean {
namespace rts {

void validate(const Inventory& inventory, const Validate& val, Lookup& facts) {
  const auto starting_id = facts.startingId();
  const auto first_free = facts.firstFreeId();
  auto allowed_id = starting_id;

  std::vector<Pid> types(distance(starting_id, first_free), Pid::invalid());

  Renamer checker([&](Id id, Pid type) {
    auto real_type = types.at(distance(starting_id, id));
    if (type != real_type) {
      rts::error("fact type mismatch");
    }
    return id;
  });

  size_t count = 0;

  for (auto i = facts.enumerate(); auto fact = i->get(); i->next()) {
    if (count >= val.limit) {
      break;
    }
    ++count;

    if (fact.id < allowed_id) {
      rts::error("enumeration out of order");
    }
    if (fact.id >= first_free) {
      rts::error("fact id out of bounds");
    }

    if (val.typecheck) {
      const auto *predicate = inventory.lookupPredicate(fact.type);
      if (predicate == nullptr) {
        rts::error("invalid predicate");
      }

      binary::Output out;
      uint64_t key_size;

      predicate->typecheck(checker, fact.clause, out, key_size);

      if (fact.clause.bytes() != out.bytes()) {
        rts::error("invalid fact");
      }
      if (fact.clause.key_size != key_size) {
        rts::error("key size mismatch");
      }
    }

    if (val.keys && facts.idByKey(fact.type, fact.key()) != fact.id) {
      rts::error("idByKey mismatch");
    }

    allowed_id = fact.id + 1;
    types[distance(starting_id, fact.id)] = fact.type;
  }
}

}
}
}
