/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lang/clang/path.h"

#include <gtest/gtest.h>
#include <folly/testing/TestUtil.h>
#include <filesystem>
#include <string>

namespace facebook::glean::clangx {

namespace {

std::filesystem::path testRoot() {
#ifdef _WIN32
  return "C:\\";
#else
  return "/";
#endif
}

std::string normalized(const std::filesystem::path& path) {
  return path.generic_string();
}

} // namespace

TEST(PathTest, GoodPathLeavesRelativePathInsideRoot) {
  const std::filesystem::path root = testRoot();

  EXPECT_EQ(normalized(goodPath(root / "foo", "bar/../baz")), "baz");
}

TEST(PathTest, GoodPathRemovesRootSuffixFromRelativePath) {
  const std::filesystem::path root = testRoot();

  EXPECT_EQ(normalized(goodPath(root / "foo", "../foo/bar")), "bar");
  EXPECT_EQ(normalized(goodPath(root / "1/2/3", "../../2/3/foo")), "foo");
}

TEST(PathTest, GoodPathKeepsRelativePathWhenItDoesNotResolveUnderRoot) {
  const std::filesystem::path root = testRoot();

  EXPECT_EQ(normalized(goodPath(root / "1/2/3", "../4/5")), "../4/5");
}

TEST(PathTest, GoodPathRelativizesAbsolutePathUnderRoot) {
  const std::filesystem::path root = testRoot();

  EXPECT_EQ(normalized(goodPath(root / "1/2", root / "1/2/3/foo")), "3/foo");
  EXPECT_TRUE(goodPath(root / "foo", root / "foo").empty());
}

TEST(PathTest, GoodPathKeepsAbsolutePathOutsideRoot) {
  const std::filesystem::path root = testRoot();

  EXPECT_EQ(goodPath(root / "foo", root / "bar"), root / "bar");
  EXPECT_EQ(goodPath(root / "1/2/3", root / "1/2/4/5"), root / "1/2/4/5");
}

TEST(PathTest, GoodPathKeepsEmptyPathEmpty) {
  const std::filesystem::path root = testRoot();

  EXPECT_TRUE(goodPath(root / "foo", "").empty());
}

TEST(PathTest, FollowSymlinksInsideRootLeavesPlainPathUnchanged) {
  using namespace std::filesystem;

  folly::test::TemporaryDirectory tmpdir;
  const auto dir = path(tmpdir.path().native());
  const auto root = dir / "root";
  create_directories(root / "a/c");

  EXPECT_EQ(followSymlinksInsideRoot(root, path("a/c")), path("a/c"));
}

TEST(PathTest, FollowSymlinksInsideRootResolvesInternalSymlinkWithTail) {
  using namespace std::filesystem;

  folly::test::TemporaryDirectory tmpdir;
  const auto dir = path(tmpdir.path().native());
  const auto root = dir / "root";
  create_directories(root / "a/c");
  create_directory_symlink("a", root / "b");

  EXPECT_EQ(followSymlinksInsideRoot(root, path("b/c")), path("a/c"));
}

TEST(PathTest, FollowSymlinksInsideRootResolvesNestedSymlink) {
  using namespace std::filesystem;

  folly::test::TemporaryDirectory tmpdir;
  const auto dir = path(tmpdir.path().native());
  const auto root = dir / "root";
  create_directories(root / "a/c");
  create_directory_symlink("a", root / "b");
  create_directory_symlink("../b/c", root / "a/d");

  EXPECT_EQ(followSymlinksInsideRoot(root, path("a/d")), path("a/c"));
}

TEST(PathTest, FollowSymlinksInsideRootLeavesEscapingSymlinkUnresolved) {
  using namespace std::filesystem;

  folly::test::TemporaryDirectory tmpdir;
  const auto dir = path(tmpdir.path().native());
  const auto root = dir / "root";
  create_directory(root);
  const auto out = dir / "out";
  create_directory(out);
  create_directory_symlink(out, root / "escape");

  EXPECT_EQ(followSymlinksInsideRoot(root, path("escape")), path("escape"));
  EXPECT_EQ(followSymlinksInsideRoot(root, path("escape/c")), path("escape/c"));
}

TEST(PathTest, FollowSymlinksInsideRootCanonicalizesEscapingPathBackIntoRoot) {
  using namespace std::filesystem;

  folly::test::TemporaryDirectory tmpdir;
  const auto dir = path(tmpdir.path().native());
  const auto root = dir / "root";
  create_directories(root / "a/c");
  const auto out = dir / "out";
  create_directory(out);
  create_directory_symlink(out, root / "escape");
  create_directory_symlink("../root/a/c", out / "back");

  EXPECT_EQ(followSymlinksInsideRoot(root, path("escape/back")), path("a/c"));
}

} // namespace facebook::glean::clangx
