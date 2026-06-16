/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <boost/dynamic_bitset.hpp>
#include <fmt/core.h>
#include <algorithm>

#include "glean/rts/ownership/slice.h"
#include "glean/rts/timer.h"

namespace facebook {
namespace glean {
namespace rts {

using namespace folly::compression;

// Compute a Slice for the DB whose ownership data is `ownership`, marking
// each of this DB's ownership sets visible or invisible according to the
// given `units`. The result is layered on top of `base`, the combined
// Slice(s) of the DBs lower in the stack.
//
// `units` are the units to include; if `exclude` is set they are the units
// to exclude instead. A unit not mentioned in `units` is therefore
// invisible in include mode and visible in exclude mode.
//
// `ownership` enumerates this DB's sets in increasing UsetId order. Each
// set is an Or/And expression over elements, and an element's value tells
// us what kind of thing it refers to:
//
//    base.first()...base.end()  references to ownership sets in base DB(s)
//    base.end()...first         units in this DB
//    first...first+size         sets in this DB
//
// We compute one visibility bit per set and store it in bit `i - first` of
// the result, which becomes a Slice covering [first, first+size). Because
// sets are visited in UsetId order and a set only references lower-numbered
// sets, the bits for any sets an element refers to are already computed by
// the time we need them.
//
// The visibility of a single element depends on its range:
//   - a base-DB set    -> base.visible(element)
//   - a unit           -> whether it is in `units` (negated when exclude)
//   - a set in this DB  -> the bit already computed for it
//
// A set is then visible if its op is Or and any element is visible, or its
// op is And and all elements are visible. The helper lambdas below evaluate
// each element range; the exact include/exclude pairing is documented at
// the switch on the set's op:
//   orVisibleBase / andVisibleBase  -> base-DB-set elements
//   isSubset / emptyIntersection    -> unit elements
//   orVisible / andVisible          -> this-DB-set elements

std::unique_ptr<Slice> slice(
    Ownership& ownership,
    const Slices& base,
    const std::vector<UsetId>& units,
    bool exclude) {
  auto t = makeAutoTimer("slice");

  std::unique_ptr<OwnershipSetIterator> iter = ownership.getSetIterator();
  using Reader = EliasFanoReader<EliasFanoEncoder<uint32_t, uint32_t>>;

  std::pair<size_t, size_t> p = iter->sizes();
  auto first = p.first, size = p.second;
  // [first, first + size) is the range of UsetIds for the sets defined in
  // this DB; these are the sets we compute visibility bits for.
  VLOG(1) << fmt::format(
      "slice: first={}, size={}, base.first={}, base.end={}, units.size={}",
      first,
      size,
      base.first(),
      base.end(),
      units.size());

  if (size == 0) {
    return std::make_unique<Slice>(
        base.end(), boost::dynamic_bitset<uint64_t>());
  }

  assert(base.empty() || first >= base.end());

  // The result bit set
  boost::dynamic_bitset<uint64_t> members(size, false);

  // `reader` reads an Elias-Fano encoded set of usets. isSubset returns
  // true if the set's unit elements are all contained in `units`.
  auto isSubset = [&, first](Reader& reader) {
    if (!reader.valid() ||
        reader.value() >=
            first /* i.e. value is the ID of a set, not a unit */) {
      return true;
    }
    auto it = units.begin();
    do {
      it = std::lower_bound(it, units.end(), reader.value());
      if (it == units.end() || reader.value() != *it) {
        VLOG(5) << fmt::format("isSubset: {} not in set", reader.value());
        // owned by a unit not in the set
        return false;
      }
      VLOG(5) << fmt::format("isSubset: {} in set", reader.value());
      // reader.value() == *it; advance both
      it++;
    } while (reader.next() && reader.value() < first);
    return true; // checked all values, so this is a subset
  };

  // true if none of the set's unit elements appear in `units`, i.e. the set
  // and `units` are disjoint over the unit range.
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
        VLOG(5) << fmt::format("emptyIntersection: {} in set", reader.value());
        return false;
      }
      VLOG(5) << fmt::format(
          "emptyIntersection: {} not in set", reader.value());
    } while (reader.value() < first);
    return true;
  };

  // true if among all elements of the read set that are also in the base (i.e.
  // elem < base.end), at least one is visible
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
        VLOG(5) << fmt::format("orVisibleBase: visible({})", reader.value());
        return true;
      }
      VLOG(5) << fmt::format("orVisibleBase: invisible({})", reader.value());
    } while (reader.next());
    return false;
  };

  // true if among all elements of the read set that are also in the current
  // DB's usets (i.e. elem >= first), at least one is visible.
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
        VLOG(5) << fmt::format("orVisible: visible({})", reader.value());
        return true;
      }
      VLOG(5) << fmt::format("orVisible: invisible({})", reader.value());
    } while (reader.next());
    return false;
  };

  // true if all elements of the read set that are also in the base (i.e.
  // elem < base.end) are visible
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
        VLOG(5) << fmt::format("andVisibleBase: invisible({})", reader.value());
        return false;
      }
      VLOG(5) << fmt::format("andVisibleBase: visible({})", reader.value());
    } while (reader.next());
    return true;
  };

  // true if all elements of the read set that are also in the current
  // DB's usets (i.e. elem >= first) are visible.
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
        VLOG(5) << fmt::format("andVisible: invisible({})", reader.value());
        return false;
      }
      VLOG(5) << fmt::format("andVisible: visible({})", reader.value());
    } while (reader.next());
    return true;
  };

  while (auto pair = iter->get()) {
    auto i = pair->first; // UsetId of the set being evaluated
    auto reader = Reader(*pair->second.set);
    reader.next(); // advance onto the first element
    bool visible;
    switch (pair->second.op) {
      case Or:
        if (exclude) {
          // invisible if all entries are excluded
          visible =
              orVisibleBase(reader) || !isSubset(reader) || orVisible(reader);
        } else {
          // visible if any entry is included
          visible = orVisibleBase(reader) || !emptyIntersection(reader) ||
              orVisible(reader);
        }
        break;

      case And:
        if (exclude) {
          // visible if none of the entries are excluded
          visible = andVisibleBase(reader) && emptyIntersection(reader) &&
              andVisible(reader);
        } else {
          // visible if all entries are included
          visible =
              andVisibleBase(reader) && isSubset(reader) && andVisible(reader);
        }
        break;
    }
    members[i - first] = visible;
    VLOG(5) << "slice set: " << i << " " << (visible ? "visible" : "invisible");
  }

  return std::make_unique<Slice>(first, std::move(members));
}

void Slice::serialize(binary::Output& o) const {
  serialize::put(o, first_);
  std::vector<uint64_t> words(set_.num_blocks());
  to_block_range(set_, words.begin());
  serialize::put(o, words, serialize::AsBytes());
}

std::unique_ptr<Slice> Slice::deserialize(binary::Input& i) {
  UsetId first;
  serialize::get(i, first);
  std::vector<uint64_t> words;
  serialize::get(i, words, serialize::AsBytes());
  boost::dynamic_bitset<uint64_t> set(words.size() * 64);
  from_block_range(words.begin(), words.end(), set);
  return std::make_unique<Slice>(first, std::move(set));
}

} // namespace rts
} // namespace glean
} // namespace facebook
