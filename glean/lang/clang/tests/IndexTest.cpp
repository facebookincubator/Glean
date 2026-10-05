/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <gtest/gtest.h>
#include <folly/testing/TestUtil.h>
#include <algorithm>
#include <filesystem>
#include <string>
#include <vector>

// The indexer's testable plumbing has internal linkage in its main translation
// unit. Rename its entry point so the test can exercise that plumbing directly.
#define main gleanClangIndexMainForTest
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wheader-hygiene"
#include "glean/lang/clang/index.cpp" // @manual
#pragma clang diagnostic pop
#undef main

namespace {

std::filesystem::path pathOf(const folly::test::TemporaryDirectory& directory) {
  return std::filesystem::path(directory.path().native());
}

bool writeCompilationDatabase(
    const std::filesystem::path& directory,
    const std::string& commandDirectory,
    const std::vector<std::string>& commandArguments) {
  folly::dynamic arguments = folly::dynamic::array;
  for (const auto& argument : commandArguments) {
    arguments.push_back(argument);
  }

  folly::dynamic command = folly::dynamic::object;
  command["directory"] = commandDirectory;
  command["arguments"] = std::move(arguments);
  command["file"] = "input.cpp";

  folly::dynamic commands = folly::dynamic::array;
  commands.push_back(std::move(command));
  return folly::writeFile(
      folly::toJson(commands), (directory / "compile_commands.json").c_str());
}

TEST(
    IndexTest,
    CompilationDatabaseResolvesDotDirectoryAgainstCurrentWorkingDirectory) {
  folly::test::TemporaryDirectory temporaryDirectory;
  const auto directory = pathOf(temporaryDirectory);
  ASSERT_TRUE(writeCompilationDatabase(
      directory, ".", {"clang++", "-c", "input.cpp"}));

  const auto database = loadCompilationDatabase(directory.string());
  const auto commands = database->getAllCompileCommands();

  ASSERT_EQ(commands.size(), 1);
  EXPECT_EQ(commands.front().Directory, std::filesystem::current_path());
}

TEST(IndexTest, CompilationDatabaseExpandsResponseFiles) {
  folly::test::TemporaryDirectory temporaryDirectory;
  const auto directory = pathOf(temporaryDirectory);
  ASSERT_TRUE(folly::writeFile(
      std::string("-DINDEX_TEST=1 -c input.cpp"),
      (directory / "arguments.rsp").c_str()));
  ASSERT_TRUE(writeCompilationDatabase(
      directory, directory.string(), {"clang++", "@arguments.rsp"}));

  const auto database = loadCompilationDatabase(directory.string());
  const auto commands = database->getAllCompileCommands();

  ASSERT_EQ(commands.size(), 1);
  EXPECT_NE(
      std::find(
          commands.front().CommandLine.begin(),
          commands.front().CommandLine.end(),
          "-DINDEX_TEST=1"),
      commands.front().CommandLine.end());
}

TEST(IndexTest, CompilationDatabaseRejectsMissingFile) {
  folly::test::TemporaryDirectory temporaryDirectory;

  EXPECT_THROW(
      loadCompilationDatabase(pathOf(temporaryDirectory).string()),
      std::runtime_error);
}

TEST(IndexTest, CompilationDatabaseRejectsMalformedJson) {
  folly::test::TemporaryDirectory temporaryDirectory;
  const auto directory = pathOf(temporaryDirectory);
  ASSERT_TRUE(folly::writeFile(
      std::string("not json"),
      (directory / "compile_commands.json").c_str()));

  EXPECT_THROW(loadCompilationDatabase(directory.string()), std::runtime_error);
}

TEST(IndexTest, CDBReusesDatabaseForConsecutiveSourcesInSameDirectory) {
  folly::test::TemporaryDirectory temporaryDirectory;
  const auto directory = pathOf(temporaryDirectory);
  ASSERT_TRUE(writeCompilationDatabase(
      directory, ".", {"clang++", "-c", "input.cpp"}));
  const facebook::glean::clangx::SourceFile source{
      "cell//project:target", folly::none, directory.string(), "input.cpp"};
  CDB database;
  const auto* loaded = database.load(source);
  ASSERT_TRUE(folly::writeFile(
      std::string("not json"),
      (directory / "compile_commands.json").c_str()));

  const auto* reused = database.load(source);

  EXPECT_EQ(reused, loaded);
  EXPECT_EQ(reused->getAllCompileCommands().size(), 1);
}

TEST(IndexTest, CountersRejectSpecWithoutNumericIndex) {
  const auto originalCounterFile = FLAGS_counter_file;
  const auto originalCounters = FLAGS_counters;
  const auto restoreFlags = folly::makeGuard([&] {
    FLAGS_counter_file = originalCounterFile;
    FLAGS_counters = originalCounters;
  });
  FLAGS_counter_file = "unused";
  FLAGS_counters = "fact_cache_hits";

  EXPECT_THROW(Counters(), std::runtime_error);
}

TEST(IndexTest, LLVMFatalErrorPreservesReason) {
  try {
    handleLLVMError(nullptr, "invalid LLVM state", false);
    FAIL() << "Expected the LLVM error handler to throw";
  } catch (const FatalLLVMError& error) {
    EXPECT_STREQ(error.what(), "invalid LLVM state");
  }
}

} // namespace
