/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "db.h"

#include <clang/Frontend/CompilerInstance.h>
#include <clang/Frontend/FrontendAction.h>
#include <clang/Tooling/Tooling.h>
#include <gtest/gtest.h>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <gflags/gflags.h>
#include "glean/cpp/glean.h"
#include "glean/rts/fact.h"
#include "glean/rts/inventory.h"
#include "glean/schema/cpp/schema.h"

DEFINE_bool(index_on_error, false, "Index files that have compilation errors");

namespace facebook::glean::clangx {

std::unique_ptr<clang::ASTConsumer> newASTConsumer(ClangDB* db);

namespace {

rts::Pid pidForIndex(size_t index) {
  return rts::Pid::lowest() + index;
}

template <typename Predicate>
rts::Pid pidFor() {
  return pidForIndex(SCHEMA::index<Predicate>::value);
}

template <size_t Index>
rts::Predicate makePredicate() {
  using Predicate = typename SCHEMA::predicate<Index>::type;
  return rts::Predicate{
      pidForIndex(Index),
      Predicate::GLEAN_name(),
      static_cast<int32_t>(Predicate::GLEAN_version()),
      {},
      {}};
}

template <size_t... Indexes>
rts::Inventory makeTestInventory(std::index_sequence<Indexes...>) {
  std::vector<rts::Predicate> predicates;
  predicates.reserve(sizeof...(Indexes));
  (predicates.push_back(makePredicate<Indexes>()), ...);
  return rts::Inventory(std::move(predicates));
}

rts::Inventory makeTestInventory() {
  return makeTestInventory(std::make_index_sequence<SCHEMA::count>());
}

std::map<rts::Pid, size_t> predicateCounts(
    const rts::FactSet::Serialized& serialized) {
  std::map<rts::Pid, size_t> counts;
  binary::Input input(serialized.facts.bytes());
  for (size_t i = 0; i < serialized.count; ++i) {
    rts::Pid type = rts::Pid::invalid();
    rts::Fact::Clause clause;
    rts::Fact::deserialize(input, type, clause);
    ++counts[type];
  }
  return counts;
}

struct IndexResult {
  bool parsed = false;
  std::map<rts::Pid, size_t> counts;

  template <typename Predicate>
  size_t count() const {
    const auto it = counts.find(pidFor<Predicate>());
    return it == counts.end() ? 0 : it->second;
  }
};

class TestIndexAction : public clang::ASTFrontendAction {
 public:
  explicit TestIndexAction(IndexResult& result) : result_(result) {}

  std::unique_ptr<clang::ASTConsumer> CreateASTConsumer(
      clang::CompilerInstance& compiler,
      llvm::StringRef) override {
    schema_ = std::make_unique<DbSchema<SCHEMA>>(makeTestInventory());
    batch_ = std::make_unique<Batch<SCHEMA>>(schema_.get(), 0);
    const auto locator = batch_->fact<Buck::Locator>(
        nothing(),
        std::string("//glean/lang/clang:ast_test"),
        std::string("clang"));
    ClangDB::Env env{
        folly::none,
        locator,
        folly::none,
        std::filesystem::current_path(),
        folly::none,
        folly::none,
        *batch_};
    db_ = std::make_unique<ClangDB>(env, compiler, nullptr);

    auto& sourceManager = compiler.getSourceManager();
    db_->enterFile(
        sourceManager.getLocForStartOfFile(sourceManager.getMainFileID()),
        folly::none);
    return newASTConsumer(db_.get());
  }

  void EndSourceFileAction() override {
    if (batch_) {
      result_.counts = predicateCounts(batch_->base().serialize());
    }
  }

 private:
  IndexResult& result_;
  std::unique_ptr<DbSchema<SCHEMA>> schema_;
  std::unique_ptr<Batch<SCHEMA>> batch_;
  std::unique_ptr<ClangDB> db_;
};

IndexResult indexCode(const std::string& code) {
  IndexResult result;
  result.parsed = clang::tooling::runToolOnCodeWithArgs(
      std::make_unique<TestIndexAction>(result),
      code,
      {"-std=c++17"},
      "test.cpp");
  return result;
}

} // namespace

TEST(AstTest, SkipsDeletedFunctionDeclarations) {
  const auto result = indexCode(R"cpp(
void removed() = delete;
void kept();
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::FunctionDeclaration>(), 1);
  EXPECT_EQ(result.count<Cxx::FunctionDefinition>(), 0);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, RecordsUsingDeclarationAndUseSiteXRefs) {
  const auto result = indexCode(R"cpp(
namespace source {
int choose();
}
namespace target {
using source::choose;
int result = choose();
}
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::UsingDeclaration>(), 1);
  EXPECT_EQ(result.count<Cxx::VariableDeclaration>(), 1);
  EXPECT_GE(result.count<Cxx::FileXRefs>(), 1);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, RecordsMethodOverrideEdges) {
  const auto result = indexCode(R"cpp(
struct Base {
  virtual int value() const & {
    return 1;
  }
};
struct Derived : Base {
  int value() const & override {
    return 2;
  }
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::RecordDeclaration>(), 2);
  EXPECT_EQ(result.count<Cxx::FunctionDeclaration>(), 2);
  EXPECT_EQ(result.count<Cxx::MethodOverrides>(), 1);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

} // namespace facebook::glean::clangx
