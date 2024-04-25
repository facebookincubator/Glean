/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lang/clang/path.h"

#include <gtest/gtest.h>
#include <folly/experimental/TestUtil.h>
#include <filesystem>

namespace facebook::glean::clangx {

TEST(PathTest, goodPath) {
  static std::filesystem::path root =
#ifdef _WIN32
    "C:\\";
#else
    "/";
#endif

  EXPECT_EQ(goodPath(root/"foo", "bar").generic_string(), "bar");

  EXPECT_EQ(goodPath(root/"foo", "bar/../baz").generic_string(), "baz");

  EXPECT_EQ(goodPath(root/"foo", "../foo/bar").generic_string(), "bar");
  EXPECT_EQ(goodPath(root/"1/2/3", "../../2/3/foo").generic_string(), "foo");

  EXPECT_EQ(goodPath(root/"1/2", root/"1/2/3/foo").generic_string(), "3/foo");
  // FIXME: should this be ../bar?
  EXPECT_EQ(goodPath(root/"foo", root/"bar").generic_string(), root/"bar");
  // FIXME: should this be ../4/5?
  EXPECT_EQ(goodPath(root/"1/2/3", root/"1/2/4/5").generic_string(), root/"1/2/4/5");

  EXPECT_EQ(goodPath(root/"1/2/3","../4/5").generic_string(), "../4/5");

  EXPECT_EQ(goodPath(root/"1/2/3", "4/../../3/5").generic_string(), "5");

  // FIXME: don't know what we want here
  EXPECT_TRUE(goodPath(root/"foo", root/"foo").empty());

  EXPECT_TRUE(goodPath(root/"foo", "").empty());
}

TEST(PathTest, followSymlinksInsideRoot) {
  using namespace std::filesystem;

  folly::test::TemporaryDirectory tmpdir;
  auto dir = path(tmpdir.path().native());

  auto root = dir/"root";
  create_directory(root);

  auto out = dir/"out";
  create_directory(out);

  create_directory(root/"a");
  create_directory(root/"a/c");
  create_directory_symlink("a", root/"b");
  create_directory_symlink(out, root/"escape");

  // symlink that traverses another symlink
  create_directory_symlink("../b/c", root/"a/d");

  // symlink going back into root
  create_directory_symlink("../root/a/c", out/"back");

  // no symlinks
  EXPECT_EQ(followSymlinksInsideRoot(root, path("a/c")), path("a/c"));

  // follow internal symlink
  EXPECT_EQ(followSymlinksInsideRoot(root, path("b")), path("a"));

  // follow dir symlink, relative and absolute
  EXPECT_EQ(followSymlinksInsideRoot(root, path("b/c")), path("a/c"));

  // don't follow external symlink
  EXPECT_EQ(followSymlinksInsideRoot(root, path("escape")), path("escape"));

  // don't follow external dir symlink
  EXPECT_EQ(followSymlinksInsideRoot(root, path("escape/c")), path("escape/c"));

  // complicated case
  EXPECT_EQ(followSymlinksInsideRoot(root, path("a/d")), path("a/c"));

  // escape from root and then go back via a relative symlink
  EXPECT_EQ(followSymlinksInsideRoot(root, path("escape/back")), path("a/c"));
}
}
