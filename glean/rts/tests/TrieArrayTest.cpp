/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>
#include <glean/rts/ownership.h>
#include <glean/rts/ownership/triearray.h>
#include <glean/rts/ownership/uset.h>

using namespace ::testing;
using namespace facebook::glean::rts;

TEST(TrieArrayTest, leak) {
  TrieArray<Uset> utrie;

    struct Data {
        UnitId unit;
        std::vector<OwnershipUnit::Ids> ids;
    };

    const std::vector<Data> data = {
        { 1, {{Id::fromWord(1), Id::fromWord(2)}}},
        { 2, {{Id::fromWord(1), Id::fromWord(2)}}}, // could be in-place
        { 3, {{Id::fromWord(2), Id::fromWord(3)}}},
    };

    // This test is mainly checking that the TrieArray doesn't leak any
    // values, and maintains correct reference counts.

    for (const auto& d : data) {
      utrie.insert(
          d.ids.data(),
          d.ids.data() + d.ids.size(),
          [&](Uset* FOLLY_NULLABLE prev, uint32_t refs) {
            if (prev != nullptr) {
              LOG(INFO) << folly::sformat("prev->refs {}, refs {}", prev->refs, refs);
              if (prev->refs == refs) {
                // Do an in-place append if possible (determined via reference
                // count).
                LOG(INFO) << "in-place append";
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
              auto entry = std::make_unique<Uset>(SetU32(), refs);
              entry->exp.set.append(d.unit);
              return entry.release();
            }
          });
    }

    auto flattened = utrie.flatten(1,4); // end is max key + 1
    std::vector<Uset*> sets;
    for (Uset* entry : flattened.dense) {
      if (!entry->link()) {
        sets.push_back(entry);
        entry->link(entry);;
      }
    }
    // This should delete each set, otherwise there will be a leak
    for (auto s : sets) {
      delete s;
    }
}
