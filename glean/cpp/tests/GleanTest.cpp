/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/cpp/glean.h"

#include <gtest/gtest.h>

#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "glean/bytecode/instruction.h"
#include "glean/rts/bytecode/subroutine.h"

namespace facebook::glean::cpp {
namespace {

const auto kLeafPid = rts::Pid::lowest();
const auto kRootPid = rts::Pid::lowest() + 1;

uint64_t op(rts::Op opcode) {
  return static_cast<uint64_t>(opcode);
}

std::shared_ptr<rts::Subroutine> copyClauseTypechecker() {
  // The typechecker ABI supplies ten rename syscalls followed by the clause
  // begin, key end, clause end, and output registers.
  return std::make_shared<rts::Subroutine>(
      std::vector<uint64_t>{
          op(rts::Op::OutputBytes),
          10,
          12,
          13,
          op(rts::Op::PtrDiff),
          10,
          11,
          14,
          op(rts::Op::Ret),
      },
      14,
      1,
      1,
      std::vector<uint64_t>{},
      std::vector<std::string>{});
}

std::shared_ptr<rts::Subroutine> noReferencesTraverser() {
  return std::make_shared<rts::Subroutine>(
      std::vector<uint64_t>{op(rts::Op::Ret)},
      4,
      0,
      0,
      std::vector<uint64_t>{},
      std::vector<std::string>{});
}

std::shared_ptr<rts::Subroutine> leafReferenceTraverser() {
  // Traversal receives the callback and clause bounds in registers 0-3.
  // Register 4 holds the referenced predicate and register 5 the decoded ID.
  return std::make_shared<rts::Subroutine>(
      std::vector<uint64_t>{
          op(rts::Op::InputNat),
          1,
          2,
          5,
          op(rts::Op::CallFun_2_0),
          0,
          5,
          4,
          op(rts::Op::Ret),
      },
      4,
      0,
      2,
      std::vector<uint64_t>{kLeafPid.toWord()},
      std::vector<std::string>{});
}

SchemaInventory makeInventory() {
  std::vector<rts::Predicate> predicates;
  predicates.push_back(
      rts::Predicate{
          kLeafPid,
          "test.Leaf",
          1,
          copyClauseTypechecker(),
          noReferencesTraverser()});
  predicates.push_back(
      rts::Predicate{
          kRootPid,
          "test.Root",
          1,
          copyClauseTypechecker(),
          leafReferenceTraverser()});
  return {rts::Inventory(std::move(predicates)), {}};
}

rts::Id defineStringFact(
    BatchBase& batch,
    rts::Pid type,
    const std::string& key,
    const std::string& value = {}) {
  const std::string bytes = key + value;
  return batch.define(
      type, rts::Fact::Clause::from(binary::byteRange(bytes), key.size()));
}

rts::Id defineReferenceFact(BatchBase& batch, rts::Id referenced) {
  binary::Output key;
  key.packed(referenced);
  return batch.define(kRootPid, rts::Fact::Clause::fromKey(key.bytes()));
}

using SerializedFact = std::tuple<int64_t, std::string, std::string>;

std::vector<SerializedFact> deserialize(
    const rts::FactSet::Serialized& serialized) {
  std::vector<SerializedFact> facts;
  binary::Input input(serialized.facts.bytes());
  for (size_t i = 0; i < serialized.count; ++i) {
    rts::Pid type = rts::Pid::invalid();
    rts::Fact::Clause clause;
    rts::Fact::deserialize(input, type, clause);
    facts.emplace_back(
        type.toThrift(), clause.key().str(), clause.value().str());
  }
  return facts;
}

class BatchBaseTest : public testing::Test {
 protected:
  SchemaInventory inventory_ = makeInventory();
};

TEST_F(BatchBaseTest, DefineDeduplicatesFactsAndPreservesSerializedData) {
  BatchBase batch(&inventory_, 8, "test-schema");

  const auto first = defineStringFact(batch, kLeafPid, "alpha", "one");
  const auto duplicate = defineStringFact(batch, kLeafPid, "alpha", "one");
  const auto second = defineStringFact(batch, kLeafPid, "beta", "two");

  EXPECT_EQ(duplicate, first);
  EXPECT_EQ(second, first + 1);
  EXPECT_EQ(batch.getSchemaId(), "test-schema");
  EXPECT_EQ(batch.bufferStats().count, 2);
  EXPECT_EQ(
      deserialize(batch.serialize()),
      (std::vector<SerializedFact>{
          {kLeafPid.toThrift(), "alpha", "one"},
          {kLeafPid.toThrift(), "beta", "two"},
      }));
}

TEST_F(BatchBaseTest, EndUnitOwnsRootsButNotFactsReachableFromThem) {
  BatchBase batch(&inventory_, 8, "test-schema");
  batch.beginUnit("source.cpp");
  const auto leaf = defineStringFact(batch, kLeafPid, "leaf");
  const auto root = defineReferenceFact(batch, leaf);

  batch.endUnit();

  EXPECT_EQ(
      batch.serializeOwnership(),
      (std::map<std::string, std::vector<int64_t>>{
          {"source.cpp", {root.toThrift(), root.toThrift()}},
      }));
}

TEST_F(BatchBaseTest, RebaseCachesGlobalFactsAndRewritesOwnership) {
  BatchBase batch(&inventory_, 1024, "test-schema");
  batch.beginUnit("source.cpp");
  const auto local = defineStringFact(batch, kLeafPid, "shared");
  batch.endUnit();
  const auto global = rts::Id::lowest() + 100;

  batch.rebase(rts::Substitution(local, std::vector<rts::Id>{global}));
  const auto cached = defineStringFact(batch, kLeafPid, "shared");

  EXPECT_EQ(cached, global);
  EXPECT_EQ(batch.bufferStats().count, 0);
  EXPECT_EQ(batch.firstFreeId(), global + 1);
  EXPECT_EQ(batch.cacheStats().facts.count, 1);
  EXPECT_EQ(
      batch.serializeOwnership(),
      (std::map<std::string, std::vector<int64_t>>{
          {"source.cpp", {global.toThrift(), global.toThrift()}},
      }));
}

TEST_F(BatchBaseTest, OwnershipTracksUnitsIndependentlyAndCanBeCleared) {
  BatchBase batch(&inventory_, 8, "test-schema");
  batch.beginUnit("first.cpp");
  const auto first = defineStringFact(batch, kLeafPid, "first");
  batch.endUnit();
  batch.beginUnit("second.cpp");
  const auto second = defineStringFact(batch, kLeafPid, "second");
  batch.endUnit();

  EXPECT_EQ(
      batch.serializeOwnership(),
      (std::map<std::string, std::vector<int64_t>>{
          {"first.cpp", {first.toThrift(), first.toThrift()}},
          {"second.cpp", {second.toThrift(), second.toThrift()}},
      }));

  batch.clearOwnership();
  EXPECT_TRUE(batch.serializeOwnership().empty());
}

} // namespace
} // namespace facebook::glean::cpp
