/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/benchmarking/factblock.h"

namespace facebook {
namespace glean {
namespace rts {
namespace benchmarking {

FactBlock FactBlock::create(FactIterator& iter) {
  FactBlock block;
  auto fact = iter.get();
  if (!fact) {
    block.starting_id = Id::fromWord(1024);
    return block;
  }

  block.starting_id = fact.id;
  auto expected = fact.id;

  while (fact && fact.id == expected) {
    const auto offset = block.data.size();
    block.data.insert(
        block.data.end(),
        fact.clause.data,
        fact.clause.data + fact.clause.size());
    block.refs.push_back(
        Ref{fact.type, offset, fact.clause.key_size, fact.clause.value_size});
    ++expected;
    iter.next();
    fact = iter.get();
  }

  return block;
}

bool FactBlock::defineEach(Define& def) const {
  auto expected = starting_id;
  const auto buf = data.data();
  for (const auto& ref : refs) {
    const auto k = def.define(
        ref.type, Fact::Clause{buf + ref.offset, ref.key_size, ref.value_size});
    if (k != expected) {
      return false;
    }
    ++expected;
  }
  return true;
}

bool FactBlock::lookupEachType(Lookup& lookup) const {
  for (const auto& fact : *this) {
    const auto type = lookup.typeById(fact.id);
    if (type != fact.type) {
      return false;
    }
  }
  return true;
}

bool FactBlock::lookupEachById(Lookup& lookup, bool compare) const {
  bool result = true;
  for (const auto& fact : *this) {
    const auto found = lookup.factById(fact.id, [&](auto type, auto clause) {
      if (type != fact.type || (!compare || clause.key() != fact.key())) {
        result = false;
      }
    });
    if (!found || !result) {
      return false;
    }
  }
  return true;
}

bool FactBlock::lookupEachByKey(Lookup& lookup) const {
  for (const auto& fact : *this) {
    const auto id = lookup.idByKey(fact.type, fact.key());
    if (id != fact.id) {
      return false;
    }
  }
  return true;
}

bool FactBlock::seekToEach(Lookup& lookup) const {
  for (const auto fact : *this) {
    auto iter = lookup.seek(fact.type, fact.key(), 0);
    auto ref = iter->get(FactIterator::Demand::KeyOnly);
    if (ref.id != fact.id) {
      return false;
    }
  }
  return true;
}

} // namespace benchmarking
} // namespace rts
} // namespace glean
} // namespace facebook
