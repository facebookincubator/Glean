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
  EXPECT_EQ(goodPath("/foo", "bar").native(), "bar");

  EXPECT_EQ(goodPath("/foo", "bar/../baz").native(), "baz");

  EXPECT_EQ(goodPath("/foo", "../foo/bar").native(), "bar");
  EXPECT_EQ(goodPath("/1/2/3", "../../2/3/foo").native(), "foo");

  EXPECT_EQ(goodPath("/1/2", "/1/2/3/foo").native(), "3/foo");
  // FIXME: should this be ../bar?
  EXPECT_EQ(goodPath("/foo", "/bar").native(), "/bar");
  // FIXME: should this be ../4/5?
  EXPECT_EQ(goodPath("/1/2/3", "/1/2/4/5").native(), "/1/2/4/5");

  EXPECT_EQ(goodPath("/1/2/3","../4/5").native(), "../4/5");

  EXPECT_EQ(goodPath("/1/2/3", "4/../../3/5").native(), "5");

  // FIXME: don't know what we want here
  EXPECT_TRUE(goodPath("/foo", "/foo").empty());

  EXPECT_TRUE(goodPath("/foo","").empty());

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
  create_symlink("a", root/"b");
  create_symlink(out, root/"escape");

  // symlink that traverses another symlink
  create_symlink("../b/c", root/"a/d");

  // symlink going back into root
  create_symlink("../root/a/c", out/"back");

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
