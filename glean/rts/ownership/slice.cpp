// Copyright (c) Facebook, Inc. and its affiliates.

#include <algorithm>

#include <folly/experimental/AutoTimer.h>
#include "glean/rts/ownership/slice.h"

namespace facebook {
namespace glean {
namespace rts {

using namespace folly::compression;

std::unique_ptr<Slice> slice(
    Ownership& ownership,
    const std::vector<UsetId>& units,
    bool exclude) {
  folly::AutoTimer t("slice");

  auto iter = ownership.getSetIterator();
  using Reader = EliasFanoReader<EliasFanoEncoder<uint32_t, uint32_t>>;

  auto p = iter->sizes();
  auto first = p.first, size = p.second;
  VLOG(5) << "first=" << first << ", size=" << size;
  std::vector<bool> members(size, false);

  auto isSubset = [&](Reader& reader) {
    if (!reader.next() || reader.value() >= first) {
      return true;
    }
    auto it = units.begin();
    do {
      it = std::lower_bound(it, units.end(), reader.value());
      if (it == units.end() || reader.value() < *it) {
        // owned by a unit not in the set
        VLOG(5) << "present: " << reader.value();
        return false;
      }
      // reader.value() == *it; advance both
      it++;
    } while (reader.next() && reader.value() < first);
    return true; // checked all values, so this is a subset
  };

  auto emptyIntersection = [&](Reader& reader) {
    if (!reader.skipTo(units[0])) {
      return true;
    }
    for (auto unit : units) {
      if (reader.value() < unit) {
        if (!reader.skipTo(unit)) {
          return true;
        }
      }
      if (reader.value() == unit) {
        VLOG(5) << "present: " << unit;
        return false;
      }
    }
    return true;
  };

  auto orVisible = [&](Reader& reader) {
    if (!reader.valid()) {
      return false;
    }
    if (reader.value() < first) {
      if (!reader.skipTo(first)) {
        return false;
      }
    }
    do {
      if (members[reader.value() - first]) {
        return true;
      }
    } while (reader.next());
    return false;
  };

  auto andVisible = [&](Reader& reader) {
    if (!reader.valid()) {
      return true;
    }
    if (reader.value() < first) {
      if (!reader.skipTo(first)) {
        return true;
      }
    }
    do {
      if (!members[reader.value() - first]) {
        return false;
      }
    } while (reader.next());
    return true;
  };

  if (units.empty()) {
    if (exclude) {
      std::fill(members.begin(), members.end(), true);
    }
  } else {
    assert(!members.empty());
    while (auto pair = iter->get()) {
      auto i = pair->first;

      auto reader = Reader(*pair->second.set);
      bool visible;
      switch (pair->second.op) {
        case Or:
          if (exclude) {
            // invisible if all entries are excluded
            visible = ! isSubset(reader) || orVisible(reader);
          } else {
            // visible if any entry is included
            visible = ! emptyIntersection(reader) || orVisible(reader);
          }
          break;

        case And:
          if (exclude) {
            // visible if none of the entries are excluded
            visible = emptyIntersection(reader) && andVisible(reader);
          } else {
            // visible if all entries are included
            visible = isSubset(reader) && andVisible(reader);;
          }
          break;
      }
      members[i - first] = visible;
      VLOG(5) << "slice set: " << i << " " <<
        (visible ? "visible" : "invisible");
    }
  }

  return std::make_unique<Slice>(first, std::move(members));
}

}
}
}
