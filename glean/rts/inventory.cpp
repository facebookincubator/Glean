/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/if/gen-cpp2/internal_types.h"
#include "glean/rts/bytecode/subroutine.h"
#include "glean/rts/fact.h"
#include "glean/rts/inventory.h"

#include <thrift/lib/cpp2/protocol/Serializer.h>

namespace facebook {
namespace glean {
namespace rts {

bool Predicate::operator==(const Predicate& other) const {
  return id == other.id
    && name == other.name
    && version == other.version
    && *typechecker == *other.typechecker;
}


Inventory::Inventory() : first_id(Pid::lowest())
{}

Inventory::Inventory(std::vector<Predicate> ps) {
  size_t n;
  first_id = Pid::lowest();
  auto last_id = first_id;
  for (const auto& p : ps) {
    first_id = std::min(first_id, p.id);
    last_id = std::max(last_id, p.id+1);
  }

  preds = std::vector<Predicate>(
    distance(first_id, last_id),
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
  thrift::internal::Inventory inv;
  inv.predicates() = {};
  for (const auto& p : preds) {
    if (p.id) {
      thrift::internal::Predicate ser;
      ser.id() = p.id.toThrift();
      ser.ref() = {};
      ser.ref()->name() = p.name;
      ser.ref()->version() = p.version;
      ser.typechecker() = Subroutine::toThrift(*p.typechecker);
      ser.traverser() =  Subroutine::toThrift(*p.traverser);
      inv.predicates()->push_back(std::move(ser));
    }
  }
  return apache::thrift::CompactSerializer::serialize<std::string>(inv);
}

Inventory Inventory::deserialize(folly::ByteRange bytes) {
  auto inv =
    apache::thrift::CompactSerializer::deserialize<thrift::internal::Inventory>(
      bytes);
  std::vector<Predicate> preds;
  for (auto& ser : inv.predicates().value()) {
    preds.push_back(Predicate{
      Pid::fromThrift(ser.id().value()),
      ser.ref().value().get_name(),
      ser.ref().value().get_version(),
      Subroutine::fromThrift(ser.typechecker().value()),
      Subroutine::fromThrift(ser.traverser().value())
      });
  };
  return Inventory(std::move(preds));
}

}
}
}
