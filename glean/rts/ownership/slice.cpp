/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <algorithm>
#include <boost/dynamic_bitset.hpp>

#include "glean/rts/ownership/slice.h"
#include "glean/rts/timer.h"

namespace facebook {
namespace glean {
namespace rts {

using namespace folly::compression;

// The ownership set contains
//
//    base.first()...base.end()  references to ownership sets for base DB(s)
//    base.end()..first          units in this DB
//    first...first+size         sets in this DB

std::unique_ptr<Slice> slice(
    Ownership& ownership,
    const Slices& base,
    const std::vector<UsetId>& units,
    bool exclude) {
  auto t = makeAutoTimer("slice");

  auto iter = ownership.getSetIterator();
  using Reader = EliasFanoReader<EliasFanoEncoder<uint32_t, uint32_t>>;

  auto p = iter->sizes();
  auto first = p.first, size = p.second;
  VLOG(1) << folly::sformat(
      "slice: first={}, size={}, base.first={}, base.end={}, units.size={}",
      first,
      size,
      base.first(),
      base.end(),
      units.size());

  if (size == 0) {
    return std::make_unique<Slice>(base.end(), boost::dynamic_bitset<uint64_t>());
  }

  assert(base.empty() || first >= base.end());

  boost::dynamic_bitset<uint64_t> members(size, false);

  // true if reader is a subset of units
  auto isSubset = [&, first](Reader& reader) {
    if (!reader.valid() || reader.value() >= first) {
      return true;
    }
    auto it = units.begin();
    do {
      it = std::lower_bound(it, units.end(), reader.value());
      if (it == units.end() || reader.value() != *it) {
        VLOG(5) << folly::sformat("isSubset: {} not in set", reader.value());
        // owned by a unit not in the set
        return false;
      }
      VLOG(5) << folly::sformat("isSubset: {} in set", reader.value());
      // reader.value() == *it; advance both
      it++;
    } while (reader.next() && reader.value() < first);
    return true; // checked all values, so this is a subset
  };

  // true if the intersection of reader and units is empty
  auto emptyIntersection = [&](Reader& reader) {
    if (!reader.valid() || units.empty()) {
      return true;
    }
    auto it = units.begin();
    do {
      auto unit = *it;
      if (unit < reader.value()) {
        it = std::lower_bound(it, units.end(), reader.value());
        if (it == units.end()) {
          return true;
        }
      } else {
        if (!reader.skipTo(unit)) {
          return true;
        }
      }
      if (reader.value() == unit) {
        VLOG(5) << folly::sformat("emptyIntersection: {} in set",
                                  reader.value());
        return false;
      }
      VLOG(5) << folly::sformat("emptyIntersection: {} not in set",
                                reader.value());
    } while (reader.value() < first);
    return true;
  };

  auto orVisibleBase = [&](Reader& reader) {
    if (!reader.valid() || base.empty()) {
      return false;
    }
    if (reader.value() < base.first()) {
      if (!reader.skipTo(base.first())) {
        return false;
      }
    }
    do {
      if (reader.value() >= base.end()) {
        return false;
      }
      if (base.visible(reader.value())) {
        VLOG(5) << folly::sformat("orVisibleBase: visible({})", reader.value());
        return true;
      }
      VLOG(5) << folly::sformat("orVisibleBase: invisible({})", reader.value());
    } while (reader.next());
    return false;
  };

  auto orVisible = [&, first](Reader& reader) {
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
        VLOG(5) << folly::sformat("orVisible: visible({})", reader.value());
        return true;
      }
      VLOG(5) << folly::sformat("orVisible: invisible({})", reader.value());
    } while (reader.next());
    return false;
  };

  auto andVisibleBase = [&](Reader& reader) {
    if (!reader.valid() || base.empty()) {
      return true;
    }
    if (reader.value() < base.first()) {
      if (!reader.skipTo(base.first())) {
        return true;
      }
    }
    do {
      if (reader.value() >= base.end()) {
        return true;
      }
      if (!base.visible(reader.value())) {
        VLOG(5) << folly::sformat("andVisibleBase: invisible({})", reader.value());
        return false;
      }
      VLOG(5) << folly::sformat("andVisibleBase: visible({})", reader.value());
    } while (reader.next());
    return true;
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
        VLOG(5) << folly::sformat("andVisible: invisible({})", reader.value());
        return false;
      }
      VLOG(5) << folly::sformat("andVisible: visible({})", reader.value());
    } while (reader.next());
    return true;
  };

  while (auto pair = iter->get()) {
    auto i = pair->first;

    auto reader = Reader(*pair->second.set);
    reader.next();
    bool visible;
    switch (pair->second.op) {
      case Or:
        if (exclude) {
          // invisible if all entries are excluded
          visible = orVisibleBase(reader) || ! isSubset(reader) || orVisible(reader);
        } else {
          // visible if any entry is included
          visible = orVisibleBase(reader) || ! emptyIntersection(reader) || orVisible(reader);
        }
        break;

      case And:
        if (exclude) {
          // visible if none of the entries are excluded
          visible = andVisibleBase(reader) && emptyIntersection(reader) && andVisible(reader);
        } else {
          // visible if all entries are included
          visible = andVisibleBase(reader) && isSubset(reader) && andVisible(reader);;
        }
        break;
    }
    members[i - first] = visible;
    VLOG(5) << "slice set: " << i << " " <<
      (visible ? "visible" : "invisible");
  }

  return std::make_unique<Slice>(first, std::move(members));
}

}
}
}
