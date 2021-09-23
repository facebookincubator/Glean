// Copyright (c) Facebook, Inc. and its affiliates.

#include <algorithm>

#include "glean/rts/ownership/slice.h"

namespace facebook {
namespace glean {
namespace rts {

using namespace folly::compression;

std::unique_ptr<Slice> slice(
    Ownership *ownership,
    const std::vector<UnitId>& units,
    bool exclude) {
  auto iter = ownership->getSetIterator();
  using Reader = EliasFanoReader<
    folly::compression::EliasFanoEncoderV2<uint32_t,uint32_t>>;
  std::vector<bool> members(iter->size(), false);
  if (!units.empty()) {
    assert(!members.empty());
    while (auto pair = iter->get()) {
      auto i = pair->first;
      VLOG(5) << "slice set: " << i;
      auto reader = Reader(*pair->second);
      if (exclude) {
        // we want members[i] = false iff sets_[i] is a subset of units,
        // indicating that the fact is owned by excluded units only.
        if (!reader.next()) {
          // empty ownership set... include these facts I guess?
          members[i] = true;
          continue;
        }
        auto it = units.begin();
        do {
          it = std::lower_bound(it, units.end(), reader.value());
          if (it == units.end() || reader.value() < *it) {
            // owned by a unit we didn't exclude
            VLOG(5) << "present: " << reader.value();
            members[i] = true;
            break;
          }
          // reader.value() == *it; advance both
          it++;
        } while (reader.next());
        // if the loop condition fails, the set only contains
        // excluded units.
      } else {
        if (!reader.skipTo(units[0])) {
          continue;
        }
        for (auto unit : units) {
          if (reader.value() < unit) {
            if (!reader.skipTo(unit)) {
              break;
            }
          }
          if (reader.value() == unit) {
            VLOG(5) << "present: " << unit;
            members[i] = true;
            break;
          }
        }
      }
    }
  }
  return std::make_unique<Slice>(std::move(members));
}

}
}
}
