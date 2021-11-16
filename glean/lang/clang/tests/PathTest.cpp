/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lang/clang/path.h"

#include <gtest/gtest.h>

namespace facebook {
namespace glean {
namespace clangx {

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

TEST(PathTest, betterPath) {
  EXPECT_EQ(betterPath("/foo","/bar").native(), "/foo");
  EXPECT_EQ(betterPath("/foo","bar").native(), "bar");
  EXPECT_EQ(betterPath("foo","/bar").native(), "foo");
  EXPECT_EQ(betterPath("foo","bar").native(), "foo");
  EXPECT_EQ(betterPath("","bar").native(), "bar");
}

}
}
}
