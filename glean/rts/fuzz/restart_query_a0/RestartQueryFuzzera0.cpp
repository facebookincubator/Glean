/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "security/lionhead/utils/lib_ftest/ftest.h"

#include "glean/rts/define.h"
#include "glean/rts/lookup.h"
#include "glean/rts/query.h"

using namespace facebook::security::lionhead::fdp;
using namespace facebook::glean::rts;

namespace {

struct StubDefine : Define {
  Id idByKey(Pid, folly::ByteRange) override {
    return Id::invalid();
  }
  Pid typeById(Id) override {
    return Pid::invalid();
  }
  bool factById(Id, std::function<void(Pid, Fact::Clause)>) override {
    return false;
  }
  Id startingId() const override {
    return Id::lowest();
  }
  Id firstFreeId() const override {
    return Id::lowest();
  }
  Interval count(Pid) const override {
    return 0;
  }
  std::unique_ptr<FactIterator> enumerate(Id, Id) override {
    return std::make_unique<EmptyIterator>();
  }
  std::unique_ptr<FactIterator> enumerateBack(Id, Id) override {
    return std::make_unique<EmptyIterator>();
  }
  std::unique_ptr<FactIterator>
  seek(Pid, folly::ByteRange, std::optional<Fact::Ref>) override {
    return std::make_unique<EmptyIterator>();
  }
  std::unique_ptr<FactIterator> seekWithinSection(
      Pid,
      folly::ByteRange,
      Id,
      Id,
      std::optional<Fact::Ref>) override {
    return std::make_unique<EmptyIterator>();
  }
  UsetId getOwner(Id) override {
    return INVALID_USET;
  }
  Id define(Pid, Fact::Clause, Id) override {
    return Id::invalid();
  }
};

} // namespace

FUZZ(RestartQuery, Fuzz) {
  auto data = f.all_remaining_bytes();

  StubDefine facts;
  Inventory inventory;
  std::unordered_set<Pid, folly::hasher<Pid>> expandPids;

  try {
    restartQuery(
        inventory,
        facts,
        nullptr,
        folly::none,
        folly::none,
        folly::none,
        folly::none,
        Depth::ResultsOnly,
        expandPids,
        false,
        const_cast<void*>(static_cast<const void*>(data.data())),
        data.size());
  } catch (...) {
  }
}
