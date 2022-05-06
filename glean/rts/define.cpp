/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/define.h"

#include <folly/container/F14Map.h>
#include <folly/MapUtil.h>

namespace facebook {
namespace glean {
namespace rts {

/// Define all new facts in a Batch. The substition is used and updated
/// with the new facts. The facts are typechecked based on the inventory.
Substitution defineUntrustedBatch(
    Define& def,
    const Inventory& inventory,
    Id first,
    const Id * FOLLY_NULLABLE ids,   // nullptr if there are no named facts
    size_t count,
    folly::ByteRange batch) {
  if (first < Id::lowest()) {
    error("invalid base id {} in batch", first);
  }

  Substitution subst(first, count);

  folly::F14FastMap<Id,Id,folly::Hash> idmap;

  binary::Input input(batch);

  Id max_ref;
  Renamer renamer([&](Id id, Pid type) {
    const auto real_id = subst.subst(folly::get_default(idmap, id, id));
    auto real_type = def.typeById(real_id);
    if (real_type == type) {
      if (real_id > max_ref) {
        max_ref = real_id;
      }
      return real_id;
    } else {
      auto pred = inventory.lookupPredicate(type);
      CHECK_NOTNULL(pred);
      if (!real_type) {
        error("unknown fact {}, expecting a fact of predicate {}.{}",
            id,
            pred->name,
            pred->version);
      } else {
        auto real_pred = inventory.lookupPredicate(real_type);
        CHECK_NOTNULL(real_pred);
        error("invalid reference to fact {}: expected {}.{}, got {}.{}",
              id,
              pred->name,
              pred->version,
              real_pred->name,
              real_pred->version);
      }
    }
  });

  for (size_t i = 0; i < count; ++i) {
    Pid ty;
    Fact::Clause clause;
    Fact::deserialize(input, ty, clause);

    if (const auto *predicate = inventory.lookupPredicate(ty)) {
      max_ref = Id::invalid();

      binary::Output out;
      uint64_t key_size;
      predicate->typecheck(renamer, clause, out, key_size);
      const auto id =
        def.define(ty, Fact::Clause::from(out.bytes(), key_size), max_ref);

      if (!id) {
        error("invalid fact redefinition ({})", predicate->name);
      }

      subst.setAt(i, id);

      if (ids && ids[i]) {
        idmap[ids[i]] = id;
      }
    } else {
      error("invalid predicate id {}", ty);
    }
  }
  return subst;
}

}
}
}
