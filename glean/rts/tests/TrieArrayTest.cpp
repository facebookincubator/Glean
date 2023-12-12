/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>
#include <rapidcheck/gtest.h>
#include <boost/icl/interval_set.hpp>

#include <glean/rts/tests/arbitrary.h>
#include <glean/rts/ownership.h>
#include <glean/rts/ownership/triearray.h>
#include <glean/rts/ownership/uset.h>

using namespace ::testing;
using namespace facebook::glean::rts;

struct UnitData {
  UnitId unit;
  std::vector<OwnershipUnit::Ids> ids;
};

struct Data {
  std::vector<UnitData> data;
};

namespace rc {

template <>
struct Arbitrary<OwnershipUnit::Ids> {
  static Gen<OwnershipUnit::Ids> arbitrary() {
    const auto l = Id::lowest().toWord();
    return gen::map(
        gen::pair(
            gen::inRange<uint32_t>(l, l + 10000),
            gen::inRange<uint32_t>(l, l + 10000)),
        [](std::pair<uint32_t, uint32_t> x) {
          auto lo = std::min(x.first, x.second);
          auto hi = std::max(x.first, x.second);
          return OwnershipUnit::Ids{Id::fromWord(lo), Id::fromWord(hi)};
        });
  }
};

template <>
struct Arbitrary<UnitData> {
  static Gen<UnitData> arbitrary() {
    return gen::mapcat(
        gen::map(
            gen::arbitrary<std::vector<OwnershipUnit::Ids>>(),
            [](std::vector<OwnershipUnit::Ids> ids) {
              // Ranges must be non-overlapping, so normalise using an
              // interval_set
              boost::icl::
                  interval_set<Id, std::less, boost::icl::closed_interval<Id>>
                      is;
              for (auto& id : ids) {
                is.insert({id.start, id.finish});
              }
              std::vector<OwnershipUnit::Ids> intervals;
              intervals.reserve(is.iterative_size() * 2);
              for (auto p : is) {
                intervals.push_back({p.lower(), p.upper()});
              }
              return intervals;
            }),
        [](const std::vector<OwnershipUnit::Ids>& intervals) {
          return gen::construct<UnitData>(
              gen::arbitrary<UnitId>(), gen::just(intervals));
        });
  }
};

template <>
struct Arbitrary<Data> {
  static Gen<Data> arbitrary() {
    return gen::construct<Data>(gen::map(
        gen::arbitrary<std::vector<UnitData>>(),
        [](const std::vector<UnitData>& d) {
          // Units are required to be in ascending order, so sort first
          std::vector<UnitData> c = d;
          std::sort(
              c.begin(), c.end(), [](const UnitData& a, const UnitData& b) {
                return a.unit < b.unit;
              });
          return c;
        }));
  }
};
} // namespace rc

TrieArray<Uset> createTrieFromData(const std::vector<UnitData>& data) {
  TrieArray<Uset> utrie;
  LOG(INFO) << folly::sformat("data size: {}", data.size());
  for (const auto& d : data) {
    for (auto& ids : d.ids) {
      VLOG(1) << folly::sformat("{}-{}", ids.start, ids.finish);
    }
    utrie.insert(
        d.ids.data(),
        d.ids.data() + d.ids.size(),
        [&](Uset* FOLLY_NULLABLE prev, uint32_t refs) {
          if (prev != nullptr) {
            assert(prev->refs > 0);
            VLOG(1) << folly::sformat(
                "unit {} prev->refs {}, refs {}", d.unit, prev->refs, refs);
            if (prev->refs == refs) {
              // Do an in-place append if possible (determined via reference
              // count).
              VLOG(1) << "in-place append";
              prev->exp.set.append(d.unit);
              return prev;
            } else {
              auto entry = std::make_unique<Uset>(
                  SetU32(prev->exp.set, SetU32::copy_capacity), refs);
              entry->exp.set.append(d.unit);
              prev->refs -= refs;
              return entry.release();
            }
          } else {
            VLOG(1) << folly::sformat("unit {} prev == nullptr, refs {}", d.unit, refs);
            auto entry = std::make_unique<Uset>(SetU32(), refs);
            entry->exp.set.append(d.unit);
            return entry.release();
          }
        });
  }
  return utrie;
}

void deleteTrie(TrieArray<Uset> utrie) {
  std::vector<Uset*> sets;
  utrie.foreach([&](Uset *uset) {
    if (!uset->link()) {
      sets.push_back(uset);
      uset->link(uset);
    }
    return uset;
  });

  // This should delete each set, otherwise there will be a leak
  for (auto s : sets) {
    delete s;
  }
}

RC_GTEST_PROP(
    TrieArrayTest,
    random,
    (const Data& data)) {
  deleteTrie(createTrieFromData(data.data));
  RC_ASSERT(1);
}
