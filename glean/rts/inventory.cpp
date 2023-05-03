/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/bytecode/subroutine.h"
#include "glean/rts/fact.h"
#include "glean/rts/inventory.h"
#include "glean/rts/serialize.h"

namespace facebook {
namespace glean {
namespace rts {

bool Predicate::operator==(const Predicate& other) const {
  return id == other.id &&
      (id == id.invalid() ||
       (name == other.name && version == other.version &&
        *typechecker == *other.typechecker && *traverser == *other.traverser));
}

Inventory::Inventory() : first_id(Pid::lowest())
{}

Inventory::Inventory(std::vector<Predicate> ps) {
  size_t n;
  first_id = Pid::invalid();
  auto last_id = Pid::lowest();
  for (const auto& p : ps) {
    assert(p.id);
    if (!first_id) {
      first_id = p.id;
    } else {
      first_id = std::min(first_id, p.id);
    }
    last_id = std::max(last_id, p.id+1);
  }

  preds = std::vector<Predicate>(
    distance(first_id ? first_id : Pid::lowest(), last_id),
    Predicate{Pid::invalid(), {}, 0, {}});
  for (auto& p : ps) {
    const auto i = distance(first_id, p.id);
    preds[i] = std::move(p);
  }
}

const Predicate * FOLLY_NULLABLE Inventory::lookupPredicate(Pid id) const & {
  if (id >= firstId()) {
    const auto i = distance(firstId(), id);
    return i < preds.size() && preds[i].id ? &preds[i] : nullptr;
  } else {
    return nullptr;
  }
}

std::vector<const Predicate *> Inventory::predicates() const {
  std::vector<const Predicate *> ps;
  ps.reserve(preds.size());
  for (const auto& p : preds) {
    if (p.id) {
      ps.push_back(&p);
    }
  }
  return ps;
}

std::string Inventory::serialize() const {
  binary::Output out;
  using namespace serialize;
  put(out, preds.size());
  for (const auto& p : preds) {
    put(out, p.id);
    if (p.id) {
      put(out, p.name);
      put(out, p.version);
      put(out, *p.typechecker);
      put(out, *p.traverser);
    }
  }
  return out.string();
}

Inventory Inventory::deserialize(folly::ByteRange bytes) {
  binary::Input in(bytes);
  std::vector<Predicate> preds;
  size_t count;
  using namespace serialize;
  get(in, count);
  for (size_t i = 0; i < count; i++) {
    Predicate p;
    get(in, p.id);
    if (p.id) {
      get(in, p.name);
      get(in, p.version);
      p.typechecker = std::make_shared<Subroutine>();
      p.traverser = std::make_shared<Subroutine>();
      serialize::get(in, *p.typechecker);
      serialize::get(in, *p.traverser);
      preds.push_back(std::move(p));
    }
  }
  return Inventory(std::move(preds));
}
}
}
}
