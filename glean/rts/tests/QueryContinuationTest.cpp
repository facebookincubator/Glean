/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <folly/lang/Bits.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <array>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

#include "glean/rts/binary.h"
#include "glean/rts/bytecode/subroutine.h"
#include "glean/rts/factset.h"
#include "glean/rts/inventory.h"
#include "glean/rts/query.h"
#include "glean/rts/set.h"

using namespace facebook::glean;
using namespace facebook::glean::rts;
using ::testing::HasSubstr;

namespace {

// Facts of one predicate with distinct 4-byte keys and no values.
FactSet factsWithDistinctKeys(uint32_t n) {
  FactSet facts(Id::lowest());
  for (uint32_t i = 0; i < n; ++i) {
    const uint32_t key = folly::Endian::big(i);
    facts.define(
        Pid::lowest(),
        Fact::Clause::from(
            folly::ByteRange(
                reinterpret_cast<const unsigned char*>(&key), sizeof(key)),
            sizeof(key)));
  }
  return facts;
}

// Query bytecode with the shape Codegen.hs emits for a set comprehension
// `S = all (... p X ...)`: create a set, loop over a fact generator, insert
// into the set, and suspend when `next` reports a timeout (ok == 2) so the
// query can be resumed from its continuation; then convert and free the set.
// Register layout follows executeQuery: syscall handlers in r0-r18, r19-r21
// scalar args, r22 the binary::Output for set elements, locals from r23.
Subroutine setComprehensionQuery(Pid pid) {
  auto op = [](Op o) { return static_cast<uint64_t>(o); };
  enum : uint64_t {
    kSeek = 0,
    kNext = 4,
    kNewSet = 10,
    kInsertOutputSet = 11,
    kSetToArray = 12,
    kFreeSet = 13,
    kOut = 22,
    kSet = 23,
    kType = 24,
    kKeyBegin = 25,
    kKeyEnd = 26,
    kSeekToken = 27,
    kOk = 28,
    kClause = 29,
    kKeyEndOut = 30,
    kClauseEnd = 31,
    kId = 32,
    kNeedValue = 33,
  };
  std::vector<uint64_t> code{
      op(Op::CallFun_0_1),
      kNewSet,
      kSet,
      op(Op::LoadConst),
      pid.toWord(),
      kType,
      op(Op::LoadConst),
      0,
      kKeyBegin,
      op(Op::LoadConst),
      0,
      kKeyEnd,
      op(Op::LoadConst),
      0,
      kNeedValue,
      op(Op::CallFun_3_1),
      kSeek,
      kType,
      kKeyBegin,
      kKeyEnd,
      kSeekToken};
  const auto loop = code.size();
  code.insert(
      code.end(),
      {op(Op::CallFun_2_5),
       kNext,
       kSeekToken,
       kNeedValue,
       kOk,
       kClause,
       kKeyEndOut,
       kClauseEnd,
       kId,
       op(Op::Select),
       kOk,
       2,
       0, // patched: no more facts -> end
       0}); // patched: fact -> insert
  const auto selectTargets = code.size() - 2;
  const auto afterSelect = code.size();
  code.insert(code.end(), {op(Op::Suspend), kOk, 0}); // patched: -> loop
  const auto afterSuspend = code.size();
  code.insert(
      code.end(),
      {op(Op::CallFun_2_0), kInsertOutputSet, kSet, kOut, op(Op::Jump), 0});
  const auto afterJump = code.size();
  code.insert(
      code.end(),
      {op(Op::CallFun_2_0),
       kSetToArray,
       kSet,
       kOut,
       op(Op::CallFun_1_0),
       kFreeSet,
       kSet,
       op(Op::Ret)});

  code[selectTargets] = afterJump - afterSelect;
  code[selectTargets + 1] = afterSuspend - afterSelect;
  code[afterSuspend - 1] = static_cast<uint64_t>(
      static_cast<int64_t>(loop) - static_cast<int64_t>(afterSuspend));
  code[afterJump - 1] = static_cast<uint64_t>(
      static_cast<int64_t>(loop) - static_cast<int64_t>(afterJump));

  return Subroutine(std::move(code), kOut + 1, 1, kNeedValue - kOut, {}, {});
}

std::unique_ptr<QueryResults>
runQuery(FactSet& facts, Subroutine& sub, folly::Optional<uint64_t> maxTimeMs) {
  Inventory inventory;
  std::unordered_set<Pid, folly::hasher<Pid>> expandPids;
  return executeQuery(
      inventory,
      facts,
      nullptr,
      sub,
      Pid::lowest(),
      nullptr,
      folly::none,
      folly::none,
      maxTimeMs,
      folly::none,
      Depth::ResultsOnly,
      expandPids,
      false);
}

std::unique_ptr<QueryResults> resumeQuery(
    FactSet& facts,
    std::string& continuation) {
  Inventory inventory;
  std::unordered_set<Pid, folly::hasher<Pid>> expandPids;
  return restartQuery(
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
      continuation.data(),
      continuation.size());
}

template <typename F>
std::string errorMessage(F&& f) {
  try {
    f();
  } catch (const std::runtime_error& e) {
    return e.what();
  }
  return "";
}

binary::Output outputOf(const std::string& s) {
  binary::Output out;
  out.bytes(s.data(), s.size());
  return out;
}

} // namespace

TEST(QueryContinuationTest, SetComprehensionWithoutTimeBudgetCompletes) {
  auto facts = factsWithDistinctKeys(1000);
  auto sub = setComprehensionQuery(Pid::lowest());

  const auto results = runQuery(facts, sub, folly::none);

  EXPECT_EQ(results->continuation.size(), 0);
}

// A query that times out inside a set comprehension returns a continuation
// that records the set token held in a local register but not the set
// itself, so the restarted query has no set for that token. Resuming must
// fail the query rather than index the restarted query's empty set table.
TEST(QueryContinuationTest, ResumingInsideSetComprehensionFailsCleanly) {
  // Enough facts that building the seek index outlasts a 0 ms time budget
  // before the 100th `next` checks the clock.
  auto facts = factsWithDistinctKeys(100000);
  auto sub = setComprehensionQuery(Pid::lowest());
  const auto first = runQuery(facts, sub, uint64_t{0});
  std::string cont(first->continuation.data(), first->continuation.size());
  ASSERT_FALSE(cont.empty()) << "query finished before timing out";

  const auto error = errorMessage([&] { resumeQuery(facts, cont); });

  EXPECT_THAT(error, HasSubstr("invalid set token"));
}

namespace {

// Query bytecode for two nested fact generators, `A _; B _`, in the shape
// Codegen.hs emits (compileFactGenerator): seek, then a loop of `next` that
// suspends on timeout (ok == 2) and ends with endSeek.
Subroutine nestedGeneratorsQuery(Pid outer, Pid inner) {
  auto op = [](Op o) { return static_cast<uint64_t>(o); };
  auto rel = [](size_t to, size_t from) {
    return static_cast<uint64_t>(
        static_cast<int64_t>(to) - static_cast<int64_t>(from));
  };
  enum : uint64_t {
    kSeek = 0,
    kEndSeek = 3,
    kNext = 4,
    kOuterType = 22,
    kInnerType = 23,
    kNoKey = 24,
    kNeedValue = 25,
    kOuterToken = 26,
    kInnerToken = 27,
    kOk = 28,
    kClause = 29,
    kKeyEndOut = 30,
    kClauseEnd = 31,
    kId = 32,
    kFrameSize = 33,
  };
  std::vector<uint64_t> code{
      op(Op::LoadConst),
      outer.toWord(),
      kOuterType,
      op(Op::LoadConst),
      inner.toWord(),
      kInnerType,
      op(Op::LoadConst),
      0,
      kNoKey,
      op(Op::LoadConst),
      0,
      kNeedValue,
      op(Op::CallFun_3_1),
      kSeek,
      kOuterType,
      kNoKey,
      kNoKey,
      kOuterToken};
  // Appends `next token; select ok [end, body]; suspend -> loop` and returns
  // {loop, index of the end target, position after select}.
  auto generator = [&](uint64_t token) {
    const auto loop = code.size();
    code.insert(
        code.end(),
        {op(Op::CallFun_2_5),
         kNext,
         token,
         kNeedValue,
         kOk,
         kClause,
         kKeyEndOut,
         kClauseEnd,
         kId,
         op(Op::Select),
         kOk,
         2,
         0,
         0});
    const auto endTarget = code.size() - 2;
    const auto afterSelect = code.size();
    code.insert(code.end(), {op(Op::Suspend), kOk, 0});
    const auto body = code.size();
    code[body - 1] = rel(loop, body);
    code[endTarget + 1] = rel(body, afterSelect);
    return std::array<size_t, 3>{loop, endTarget, afterSelect};
  };
  const auto [outerLoop, outerEndTarget, outerAfterSelect] =
      generator(kOuterToken);
  code.insert(
      code.end(),
      {op(Op::CallFun_3_1), kSeek, kInnerType, kNoKey, kNoKey, kInnerToken});
  const auto [innerLoop, innerEndTarget, innerAfterSelect] =
      generator(kInnerToken);
  code.insert(code.end(), {op(Op::Jump), 0});
  code.back() = rel(innerLoop, code.size());
  code[innerEndTarget] = rel(code.size(), innerAfterSelect);
  code.insert(
      code.end(),
      {op(Op::CallFun_1_0), kEndSeek, kInnerToken, op(Op::Jump), 0});
  code.back() = rel(outerLoop, code.size());
  code[outerEndTarget] = rel(code.size(), outerAfterSelect);
  code.insert(
      code.end(), {op(Op::CallFun_1_0), kEndSeek, kOuterToken, op(Op::Ret)});
  return Subroutine(
      std::move(code), kOuterType, 0, kFrameSize - kOuterType, {}, {});
}

} // namespace

// The timeout is checked on every 100th `next`. With an empty inner
// generator every even call is the inner generator's first `next`, so the
// query suspends with a not-yet-started iterator that has no current fact.
// The continuation records it as finished, and resuming must read only
// iterator state that was restored.
TEST(QueryContinuationTest, ResumingAtAnEmptyIteratorReadsOnlyRestoredState) {
  auto facts = factsWithDistinctKeys(100000);
  auto sub = nestedGeneratorsQuery(Pid::lowest(), Pid::lowest() + 1);
  const auto first = runQuery(facts, sub, uint64_t{0});
  std::string cont(first->continuation.data(), first->continuation.size());
  ASSERT_FALSE(cont.empty()) << "query finished before timing out";

  const auto rest = resumeQuery(facts, cont);

  EXPECT_EQ(rest->continuation.size(), 0);
}

TEST(SetOpsTest, SetOperationsRejectTokensThatWereNeverIssued) {
  SetOps ops;
  auto out = outputOf("a");
  const unsigned char byte = 1;

  EXPECT_THAT(
      errorMessage([&] { ops.insertOutputSet(0, &out); }),
      HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.setToArray(0, &out); }),
      HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.freeSet(0); }), HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.insertWordSet(0, 1); }),
      HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.insertBytesWordSet(0, &byte, &byte + 1); }),
      HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.wordSetToArray(0, &out); }),
      HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.byteSetToByteArray(0, &out); }),
      HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.freeWordSet(0); }),
      HasSubstr("invalid set token"));
}

TEST(SetOpsTest, SetOperationsRejectFreedTokens) {
  SetOps ops;
  auto out = outputOf("a");
  ops.freeSet(ops.newSet());
  ops.freeWordSet(ops.newWordSet());

  EXPECT_THAT(
      errorMessage([&] { ops.insertOutputSet(0, &out); }),
      HasSubstr("invalid set token"));
  EXPECT_THAT(
      errorMessage([&] { ops.insertWordSet(0, 1); }),
      HasSubstr("invalid set token"));
}

TEST(SetOpsTest, SetTokensAreCheckedAgainstTheirOwnKindOfSet) {
  SetOps ops;
  auto out = outputOf("a");
  ops.newWordSet();

  EXPECT_THAT(
      errorMessage([&] { ops.insertOutputSet(0, &out); }),
      HasSubstr("invalid set token"));
}

TEST(SetOpsTest, BytestringSetCollectsDistinctElements) {
  SetOps ops;
  const auto token = ops.newSet();
  for (const auto* s : {"b", "a", "b"}) {
    auto element = outputOf(s);
    ops.insertOutputSet(token, &element);
  }

  binary::Output array;
  ops.setToArray(token, &array);
  ops.freeSet(token);

  binary::Output expected;
  expected.packed(uint64_t{2});
  expected.bytes("a", 1);
  expected.bytes("b", 1);
  EXPECT_EQ(array.string(), expected.string());
}

TEST(SetOpsTest, WordSetCollectsDistinctElements) {
  SetOps ops;
  const auto words = ops.newWordSet();
  ops.insertWordSet(words, 7);
  ops.insertWordSet(words, 3);
  ops.insertWordSet(words, 7);
  const auto bytes = ops.newWordSet();
  const unsigned char input[] = {9, 2, 9};
  ops.insertBytesWordSet(bytes, input, input + sizeof(input));

  binary::Output wordArray;
  ops.wordSetToArray(words, &wordArray);
  binary::Output byteArray;
  ops.byteSetToByteArray(bytes, &byteArray);
  ops.freeWordSet(bytes);
  ops.freeWordSet(words);

  binary::Output expectedWords;
  expectedWords.packed(uint64_t{2});
  expectedWords.packed(uint64_t{3});
  expectedWords.packed(uint64_t{7});
  binary::Output expectedBytes;
  expectedBytes.packed(uint64_t{2});
  expectedBytes.fixed<uint8_t>(2);
  expectedBytes.fixed<uint8_t>(9);
  EXPECT_EQ(wordArray.string(), expectedWords.string());
  EXPECT_EQ(byteArray.string(), expectedBytes.string());
}
