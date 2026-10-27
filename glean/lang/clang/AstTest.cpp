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
#include <glog/logging.h>
#include <gtest/gtest.h>
#include <algorithm>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <folly/Synchronized.h>
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

using Identifiers = std::set<std::string>;

struct IndexResult {
  bool parsed = false;
  std::map<rts::Pid, size_t> counts;
  Identifiers thriftIdentifiers;

  template <typename Predicate>
  size_t count() const {
    const auto it = counts.find(pidFor<Predicate>());
    return it == counts.end() ? 0 : it->second;
  }
};

void collectFacts(
    const rts::FactSet::Serialized& serialized,
    IndexResult& result) {
  const auto identifierPid = pidFor<Fbthrift::Identifier>();
  binary::Input input(serialized.facts.bytes());
  for (size_t i = 0; i < serialized.count; ++i) {
    rts::Pid type = rts::Pid::invalid();
    rts::Fact::Clause clause;
    rts::Fact::deserialize(input, type, clause);
    ++result.counts[type];
    if (type == identifierPid) {
      binary::Output name;
      binary::Input(clause.key()).demangleUntrustedString(name);
      result.thriftIdentifiers.insert(name.string());
    }
  }
}

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
      collectFacts(batch_->base().serialize(), result_);
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

constexpr std::string_view kDuplicateAnnotationWarning =
    "Multiple Thrift json annotations";
constexpr std::string_view kMalformedSidlWarning =
    "Skipping malformed Sidl json annotation";
constexpr std::string_view kMisnamedSidlEnumeratorWarning =
    "Skipping Sidl json annotation for enumerator";
constexpr std::string_view kGleanParseWarning =
    "Couldn't parse Glean json annotation";

class CapturingLogSink : public google::LogSink {
 public:
  void send(
      google::LogSeverity severity,
      const char* /*full_filename*/,
      const char* /*base_filename*/,
      int /*line*/,
      const struct ::tm* /*tm_time*/,
      const char* message,
      size_t messageLen) override {
    if (severity >= google::GLOG_WARNING) {
      warnings_.wlock()->emplace_back(message, messageLen);
    }
  }

  size_t warningCount() const {
    return warnings_.rlock()->size();
  }

  size_t countContaining(std::string_view needle) const {
    auto warnings = warnings_.rlock();
    return static_cast<size_t>(std::count_if(
        warnings->begin(), warnings->end(), [needle](const std::string& w) {
          return w.find(needle) != std::string::npos;
        }));
  }

 private:
  folly::Synchronized<std::vector<std::string>> warnings_;
};

class ScopedLogSink {
 public:
  explicit ScopedLogSink(CapturingLogSink& sink) : sink_(sink) {
    google::AddLogSink(&sink_);
  }

  ~ScopedLogSink() {
    google::RemoveLogSink(&sink_);
  }

  ScopedLogSink(const ScopedLogSink&) = delete;
  ScopedLogSink& operator=(const ScopedLogSink&) = delete;

 private:
  CapturingLogSink& sink_;
};

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

TEST(AstTest, RecordsUsingDirectiveAsIndirectUseSiteXRef) {
  const auto result = indexCode(R"cpp(
namespace source {
void choose();
}
namespace target {
using namespace source;
void call() {
  choose();
}
}
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::UsingDirective>(), 1);
  EXPECT_EQ(result.count<Cxx::FunctionDeclaration>(), 2);
  EXPECT_EQ(result.count<Cxx::FunctionDefinition>(), 1);
  EXPECT_EQ(result.count<Cxx::XRefIndirectTarget>(), 1);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, FindsUnscopedEnumeratorThroughUsingDirective) {
  const auto result = indexCode(R"cpp(
namespace source {
enum Status {
  Ready,
};
}
namespace target {
using namespace source;
int state = Ready;
}
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::EnumDeclaration>(), 1);
  EXPECT_EQ(result.count<Cxx::EnumDefinition>(), 1);
  EXPECT_EQ(result.count<Cxx::UsingDirective>(), 1);
  EXPECT_EQ(result.count<Cxx::VariableDeclaration>(), 1);
  EXPECT_EQ(result.count<Cxx::XRefIndirectTarget>(), 1);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, DoesNotRetargetThroughClassScopeUsingDeclarations) {
  const auto result = indexCode(R"cpp(
struct Base {
  void run();
};
struct Derived : Base {
  using Base::run;
  void call() {
    run();
  }
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::RecordDeclaration>(), 2);
  EXPECT_EQ(result.count<Cxx::FunctionDeclaration>(), 2);
  EXPECT_EQ(result.count<Cxx::UsingDeclaration>(), 1);
  EXPECT_EQ(result.count<Cxx::XRefIndirectTarget>(), 0);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, SkipsGleanAnnotationWhenNewlineBeforeBrace) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean
   * {"file": "some/module.thrift", "service": "MyService", "function": "skipped"} */
  virtual void skipped();
  /** Glean
   * {"file": "some/module.thrift", "service": "MyService", "function": "skipped_inner", "note": "Glean", "nested": {"file": "evil.thrift", "service": "Evil", "function": "evil"}} */
  virtual void skipped_inner();
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept"} */
  virtual void kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "kept"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, ExtractsMultiLineGleanAnnotation) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "multiline"
   } */
  virtual void multiline();
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept"} */
  virtual void kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 2);
  EXPECT_EQ(
      result.thriftIdentifiers,
      (Identifiers{"MyService", "kept", "multiline"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, ExtractsMultiLineGleanAnnotationWithDocCommentStars) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {"file": "some/module.thrift", "service": "MyService",
   *   "function": "starred"} */
  virtual void starred();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "starred"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, ExtractsMultiLineGleanAnnotationFromLineDocComments) {
  const auto result = indexCode(R"cpp(
struct S {
  /// Glean {"file": "some/module.thrift", "service": "MyService",
  ///   "function": "slashed"}
  virtual void slashed();
  //! Glean {"file": "some/module.thrift", "service": "MyService",
  //!   "function": "banged"}
  virtual void banged();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 2);
  EXPECT_EQ(
      result.thriftIdentifiers,
      (Identifiers{"MyService", "banged", "slashed"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, ExtractsEveryGleanAnnotationInComment) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {"idl": "ligen", "kind": "method", "name": "skipped"} Glean {"file": "some/module.thrift", "service": "MyService", "function": "second_kept"} */
  virtual void second_kept();
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "first_kept"} Glean {"idl": "ligen", "kind": "method", "name": "skipped"} */
  virtual void first_kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 2);
  EXPECT_EQ(
      result.thriftIdentifiers,
      (Identifiers{"MyService", "first_kept", "second_kept"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, HandlesNestedObjectsAndBracesInStrings) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept", "note": "brace } and \"quote\"", "nested": {"list": [{"x": 1}, {"y": "}"}]}} */
  virtual void kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "kept"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, DecodesSidlAnnotationWithNestedJson) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"idl":"sidl","file":"x.sidl","name":"Nested","kind":"struct","extra":{"opts":[{"k":1,"v":"}","p":"a\\"}],"tags":["{","Glean"]},"note":"}{\"}","line":10,"column":0,"endLine":11,"endColumn":1} */
struct Nested {};
enum class NestedEnum {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"NestedVal","kind":"enumerator","extra":{"opts":[{"k":1,"v":"}","p":"a\\"}],"tags":["{","Glean"]},"note":"}{\"}","line":13,"column":2,"endLine":13,"endColumn":11} */
  NestedVal = 0,
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 2);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.warningCount(), 0);
}

TEST(AstTest, DecodesEverySidlKind) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"idl":"sidl","file":"x.sidl","name":"S","kind":"struct","line":1,"column":0,"endLine":2,"endColumn":1} */
struct S {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"f","kind":"field","line":2,"column":2,"endLine":2,"endColumn":3} */
  int f;
};
/** Glean {"idl":"sidl","file":"x.sidl","name":"E","kind":"enum","line":3,"column":0,"endLine":5,"endColumn":1} */
enum class E {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"V","kind":"enumerator","line":4,"column":2,"endLine":4,"endColumn":3} */
  V = 0,
};
/** Glean {"idl":"sidl","file":"x.sidl","name":"A","kind":"alias","line":6,"column":0,"endLine":6,"endColumn":10} */
using A = long;
/** Glean {"idl":"sidl","file":"x.sidl","name":"kC","kind":"const","line":7,"column":0,"endLine":7,"endColumn":10} */
inline constexpr int kC = 7;
/** Glean {"idl":"sidl","file":"x.sidl","name":"I","kind":"interface","line":8,"column":0,"endLine":10,"endColumn":1} */
struct I {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"m","kind":"method","line":9,"column":2,"endLine":9,"endColumn":10} */
  virtual int m(int x);
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 8);
  EXPECT_EQ(result.count<Cxx::SidlDeclaration>(), 8);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 0);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.warningCount(), 0);
}

TEST(AstTest, DecodesSidlFieldAnnotations) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
struct Config {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"timeout","kind":"field","line":2,"column":2,"endLine":2,"endColumn":9} */
  int timeout;
  /** Glean {"idl":"sidl","file":"x.sidl","name":"retries","kind":"field","line":3,"column":2,"endLine":3,"endColumn":9} */
  int retries;
  int plain;
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 2);
  EXPECT_EQ(result.count<Cxx::SidlDeclaration>(), 2);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.warningCount(), 0);
}

TEST(AstTest, SkipsUnknownSidlKind) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"idl":"sidl","file":"x.sidl","name":"Mystery","kind":"spaceship","line":1,"column":0,"endLine":2,"endColumn":1} */
struct Mystery {};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 0);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kMalformedSidlWarning), 1);
  EXPECT_EQ(sink.countContaining("unknown kind"), 1);
  EXPECT_EQ(sink.countContaining("Mystery"), 1);
  EXPECT_EQ(sink.countContaining("x.sidl"), 1);
  EXPECT_EQ(sink.countContaining("spaceship"), 1);
  EXPECT_EQ(sink.warningCount(), 1);
}

TEST(AstTest, SkipsMalformedSidlRanges) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"idl":"sidl","file":"x.sidl","name":"Broken","kind":"struct"} */
struct Broken {};
/** Glean {"idl":"sidl","file":"x.sidl","name":"Inverted","kind":"struct","line":5,"column":0,"endLine":4,"endColumn":1} */
struct Inverted {};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 0);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kMalformedSidlWarning), 2);
  EXPECT_EQ(sink.countContaining("missing range"), 1);
  EXPECT_EQ(sink.countContaining("endLine < line"), 1);
  EXPECT_EQ(sink.warningCount(), 2);
}

TEST(AstTest, SkipsMisnamedSidlEnumerator) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
enum class E {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"Other","kind":"enumerator","line":2,"column":2,"endLine":2,"endColumn":7} */
  Misnamed = 0,
  /** Glean {"idl":"sidl","file":"x.sidl","name":"Kept","kind":"enumerator","line":3,"column":2,"endLine":3,"endColumn":6} */
  Kept = 1,
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 1);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kMisnamedSidlEnumeratorWarning), 1);
  EXPECT_EQ(sink.warningCount(), 1);
}

TEST(AstTest, WarnsOnceForReferencedSidlEnumerator) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
enum class E {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"Other","kind":"enumerator","line":2,"column":2,"endLine":2,"endColumn":7} */
  Misnamed = 0,
};
E use() { return E::Misnamed; }
E useAgain() { return E::Misnamed; }
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 0);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kMisnamedSidlEnumeratorWarning), 1);
  EXPECT_EQ(sink.warningCount(), 1);
}

TEST(AstTest, EmitsEverySidlAnnotationOnDeclarationsAndEnumerators) {
  const auto result = indexCode(R"cpp(
/** Glean {"idl":"sidl","file":"x.sidl","name":"S","kind":"struct","line":1,"column":0,"endLine":2,"endColumn":1} Glean {"idl":"sidl","file":"y.sidl","name":"S","kind":"struct","line":1,"column":0,"endLine":2,"endColumn":1} */
struct S {};
enum class E {
  /** Glean {"idl":"sidl","file":"x.sidl","name":"V","kind":"enumerator","line":4,"column":2,"endLine":4,"endColumn":3} Glean {"idl":"sidl","file":"y.sidl","name":"V","kind":"enumerator","line":4,"column":2,"endLine":4,"endColumn":3} */
  V = 0,
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 4);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, SkipsMalformedSidlEnumeratorAnnotations) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
enum class E {
  /** Glean {"idl":"sidl", nope} */
  NotJson = 0,
  /** Glean {"idl":"sidl","file":"x.sidl","name":"Mystery","kind":"spaceship","line":3,"column":2,"endLine":3,"endColumn":13} */
  Mystery = 1,
  /** Glean {"idl":"sidl","file":"x.sidl","name":"NoRange","kind":"enumerator"} */
  NoRange = 2,
  /** Glean {"idl":"sidl","file":"x.sidl","name":"Kept","kind":"enumerator","line":5,"column":2,"endLine":5,"endColumn":10} */
  Kept = 3,
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 1);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kGleanParseWarning), 1);
  EXPECT_EQ(sink.countContaining(kMalformedSidlWarning), 2);
  EXPECT_EQ(sink.warningCount(), 3);
}

TEST(AstTest, LogsOnlyExtractedAnnotationOnJsonParseError) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {not json} Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept"} */
  virtual void kept();
};
enum class E {
  /** Glean {not json} Glean {"idl":"sidl","file":"x.sidl","name":"Kept","kind":"enumerator","line":2,"column":2,"endLine":2,"endColumn":6} */
  Kept = 0,
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "kept"}));
  EXPECT_EQ(result.count<Cxx::CxxToSidl>(), 1);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(
      sink.countContaining("Couldn't parse Glean json annotation: {not json}"),
      2);
  EXPECT_EQ(sink.countContaining("some/module.thrift"), 0);
  EXPECT_EQ(sink.countContaining("x.sidl"), 0);
}

TEST(AstTest, DoesNotExtractAnnotationFromUnclosedObjectString) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "outer", "note": "Glean {"file": "evil.thrift", "service": "Evil", "function": "evil"}" */
  virtual void evil();
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept"} */
  virtual void kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "kept"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, FindsValidAnnotationAfterNewlineBrokenGlean) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean
   * Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept"} */
  virtual void kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "kept"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, FindsValidAnnotationAfterUnclosedAnnotation) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {"file": "some/module.thrift", "service": "MyService", "function": "unclosed" Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept"} */
  virtual void kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "kept"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, FindsAnnotationAfterInStringGleanInUnclosedObject) {
  const auto result = indexCode(R"cpp(
struct S {
  /** Glean {"function": "unclosed", "note": "Glean {"file": "evil.thrift", "service": "Evil", "function": "evil"}" Glean {"file": "some/module.thrift", "service": "MyService", "function": "kept"} */
  virtual void kept();
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 1);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"MyService", "kept"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
}

TEST(AstTest, WarnsOnMultipleThriftAnnotationsForClass) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"file": "some/module.thrift", "name": "First", "kind": "struct"} Glean {"file": "some/module.thrift", "name": "Second", "kind": "struct"} */
struct DuplicateClass {};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.thriftIdentifiers, (Identifiers{"First", "Second"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kDuplicateAnnotationWarning), 1);
}

TEST(AstTest, WarnsOnMultipleThriftAnnotationsForNamespace) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"file": "a.thrift"} Glean {"file": "b.thrift"} */
namespace duplicate_ns {}
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kDuplicateAnnotationWarning), 1);
}

TEST(AstTest, WarnsOnMultipleThriftAnnotationsForEnum) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"file": "some/module.thrift", "name": "FirstEnum", "kind": "enum"} Glean {"file": "some/module.thrift", "name": "SecondEnum", "kind": "enum"} */
enum class DuplicateEnum { A, B };
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(
      result.thriftIdentifiers,
      (Identifiers{"A", "B", "FirstEnum", "SecondEnum"}));
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kDuplicateAnnotationWarning), 1);
}

TEST(AstTest, DoesNotWarnOnSingleThriftAnnotation) {
  CapturingLogSink sink;
  ScopedLogSink scopedSink(sink);
  const auto result = indexCode(R"cpp(
/** Glean {"file": "some/module.thrift", "name": "Only", "kind": "struct"} */
struct SingleClass {};
/** Glean {"file": "some/module.thrift", "name": "OnlyEnum", "kind": "enum"} */
enum class SingleEnum { X };
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Src::IndexFailure>(), 0);
  EXPECT_EQ(sink.countContaining(kDuplicateAnnotationWarning), 0);
}

TEST(AstTest, PreservesThriftContextWhenLaterAnnotationHasNoContext) {
  const auto result = indexCode(R"cpp(
/** Glean {"file": "some/module.thrift", "name": "MyStruct", "kind": "struct"} Glean {"file": "some/module.thrift", "service": "MyService", "function": "do_it"} */
struct MyStruct {
  /** Glean {"field": "my_field"} */
  int my_field;
};
)cpp");

  ASSERT_TRUE(result.parsed);
  EXPECT_EQ(result.count<Cxx::CxxToThrift>(), 3);
  EXPECT_EQ(
      result.thriftIdentifiers,
      (Identifiers{"MyService", "MyStruct", "do_it", "my_field"}));
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
