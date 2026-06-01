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

static constexpr size_t kFakeKeySize = 512;
static unsigned char kFakeKeyBuf[kFakeKeySize] = {};

struct StubFactIterator final : FactIterator {
  Fact::Ref ref_;
  bool consumed_ = false;

  StubFactIterator(Id id, Pid type)
      : ref_{id, type, Fact::Clause{kFakeKeyBuf, kFakeKeySize, 0}} {}

  void next() override {
    consumed_ = true;
  }
  Fact::Ref get(Demand) override {
    if (consumed_) {
      return Fact::Ref::invalid();
    }
    return ref_;
  }
  std::optional<Id> lower_bound() override {
    return std::nullopt;
  }
  std::optional<Id> upper_bound() override {
    return std::nullopt;
  }
};

struct SmartStubDefine : Define {
  bool find_facts;
  Id last_queried_id = Id::invalid();

  explicit SmartStubDefine(bool find) : find_facts(find) {}

  Id idByKey(Pid, folly::ByteRange) override {
    return Id::invalid();
  }
  Pid typeById(Id) override {
    return Pid::invalid();
  }

  bool factById(Id id, std::function<void(Pid, Fact::Clause)> f) override {
    if (!find_facts) {
      return false;
    }
    last_queried_id = id;
    Fact::Clause clause{kFakeKeyBuf, kFakeKeySize, 0};
    f(Pid::lowest(), clause);
    return true;
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
  seek(Pid type, folly::ByteRange, std::optional<Fact::Ref>) override {
    return std::make_unique<StubFactIterator>(last_queried_id, type);
  }
  std::unique_ptr<FactIterator> seekWithinSection(
      Pid type,
      folly::ByteRange,
      Id,
      Id,
      std::optional<Fact::Ref>) override {
    return std::make_unique<StubFactIterator>(last_queried_id, type);
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
  bool find_facts = f.boolean("find_facts");
  auto data = f.all_remaining_bytes();

  SmartStubDefine facts(find_facts);
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
