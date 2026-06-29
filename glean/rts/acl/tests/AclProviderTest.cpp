/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <folly/FileUtil.h>
#include <folly/testing/TestUtil.h>
#include <gtest/gtest.h>

#include "glean/rts/acl/acl_provider.h"

namespace facebook::glean::rts::acl {

// Tests for `createJsonACLProvider` — the factory in `acl_provider.cpp`.
//
// `JsonACLProvider`'s own semantics (inheritance, permissive mode, error
// paths) are covered by `JsonAclProviderTest`. These tests are intentionally
// narrow and focus on the factory's contract, which `JsonAclProviderTest`
// cannot exercise: the return type is the abstract `ACLProvider`, and the
// optional `permissive` argument is forwarded to the underlying provider.
class AclProviderTest : public ::testing::Test {
 protected:
  void SetUp() override {
    tempDir_ = std::make_unique<folly::test::TemporaryDirectory>();
    jsonPath_ = tempDir_->path().string() + "/acls.json";
    folly::writeFile(
        std::string(R"({"acls": {"src/foo": 7}})"), jsonPath_.c_str());
  }

  std::unique_ptr<folly::test::TemporaryDirectory> tempDir_;
  std::string jsonPath_;
};

TEST_F(AclProviderTest, FactoryReturnsWorkingProviderViaAbstractInterface) {
  // The factory returns the abstract `ACLProvider`, not the concrete
  // `JsonACLProvider`. Holding the result through the interface and
  // dispatching `getACLs` virtually proves the polymorphic wiring is in
  // place; this is unique to the factory and is not exercised by tests
  // that construct `JsonACLProvider` directly.
  std::unique_ptr<ACLProvider> provider = createJsonACLProvider(jsonPath_);
  ASSERT_NE(provider, nullptr);
  const std::vector<int> expected{7};
  EXPECT_EQ(provider->getACLs("src/foo"), expected);
}

TEST_F(AclProviderTest, FactoryForwardsPermissiveArgument) {
  // The two calls differ only in the `permissive` argument (default vs.
  // explicit `true`). A single observable difference on a sub-directory
  // path is sufficient to prove the argument is forwarded by the factory.
  // Full permissive/inheritance semantics are owned by JsonAclProviderTest.
  auto defaulted = createJsonACLProvider(jsonPath_);
  auto permissive = createJsonACLProvider(jsonPath_, /*permissive=*/true);
  ASSERT_NE(defaulted, nullptr);
  ASSERT_NE(permissive, nullptr);
  EXPECT_FALSE(defaulted->getACLs("src/foo/bar").empty());
  EXPECT_TRUE(permissive->getACLs("src/foo/bar").empty());
}

} // namespace facebook::glean::rts::acl
