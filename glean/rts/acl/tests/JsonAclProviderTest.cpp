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

#include "glean/rts/acl/json_acl_provider.h"

namespace facebook::glean::rts::acl {

class JsonACLProviderTest : public ::testing::Test {
 protected:
  void SetUp() override {
    // Create a temporary directory for test files
    tempDir_ = std::make_unique<folly::test::TemporaryDirectory>();
  }

  std::string writeJsonFile(const std::string& content) {
    auto path = tempDir_->path().string() + "/acls.json";
    folly::writeFile(content, path.c_str());
    return path;
  }

  std::unique_ptr<folly::test::TemporaryDirectory> tempDir_;
};

TEST_F(JsonACLProviderTest, LoadValidJsonFile) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 1,
      "src/bar": 2
    }
  })");

  JsonACLProvider provider(jsonPath);
  EXPECT_EQ(provider.entryCount(), 2);
}

TEST_F(JsonACLProviderTest, ExactMatchLookup) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 5
    }
  })");

  JsonACLProvider provider(jsonPath);

  const std::vector<int> expected{5};
  EXPECT_EQ(provider.getACLs("src/foo"), expected);
}

TEST_F(JsonACLProviderTest, SubdirectoryInheritance) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 1
    }
  })");

  // Default mode: sub-directories inherit parent's ACLs
  JsonACLProvider provider(jsonPath, false);

  const std::vector<int> expected{1};

  // Exact match
  EXPECT_EQ(provider.getACLs("src/foo"), expected);

  // Sub-directory inherits from src/foo
  EXPECT_EQ(provider.getACLs("src/foo/bar"), expected);

  // Deeply nested sub-directory also inherits
  EXPECT_EQ(provider.getACLs("src/foo/bar/baz/qux"), expected);
}

TEST_F(JsonACLProviderTest, PermissiveModeNoInheritance) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 1
    }
  })");

  // Permissive mode: only exact matches apply
  JsonACLProvider provider(jsonPath, true);
  EXPECT_TRUE(provider.isPermissive());

  // Exact match works
  const std::vector<int> expected{1};
  EXPECT_EQ(provider.getACLs("src/foo"), expected);

  // Sub-directory does NOT inherit - returns empty (ALL_GROUPS)
  EXPECT_TRUE(provider.getACLs("src/foo/bar").empty());

  // Unrelated path also returns empty
  EXPECT_TRUE(provider.getACLs("other/path").empty());
}

TEST_F(JsonACLProviderTest, NestedDirectoryOverrides) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src": 0,
      "src/foo": 1,
      "src/foo/bar": 3
    }
  })");

  JsonACLProvider provider(jsonPath, false);

  // Each level gets its own explicit ACLs
  const std::vector<int> expectedSrc{0};
  const std::vector<int> expectedFoo{1};
  const std::vector<int> expectedBar{3};

  EXPECT_EQ(provider.getACLs("src"), expectedSrc);
  EXPECT_EQ(provider.getACLs("src/foo"), expectedFoo);
  EXPECT_EQ(provider.getACLs("src/foo/bar"), expectedBar);

  // Deeper path inherits from nearest parent
  EXPECT_EQ(provider.getACLs("src/foo/bar/baz"), expectedBar);

  // Path without explicit entry inherits from parent
  EXPECT_EQ(provider.getACLs("src/other"), expectedSrc);
}

TEST_F(JsonACLProviderTest, PathNormalization) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 1
    }
  })");

  JsonACLProvider provider(jsonPath, false);

  const std::vector<int> expected{1};

  // With leading slash
  EXPECT_EQ(provider.getACLs("/src/foo"), expected);

  // With trailing slash
  EXPECT_EQ(provider.getACLs("src/foo/"), expected);

  // With both
  EXPECT_EQ(provider.getACLs("/src/foo/"), expected);
}

TEST_F(JsonACLProviderTest, EmptyPath) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 1
    }
  })");

  JsonACLProvider provider(jsonPath, false);

  EXPECT_TRUE(provider.getACLs("").empty()); // ALL_GROUPS
}

TEST_F(JsonACLProviderTest, NoMatchReturnsEmpty) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 1
    }
  })");

  JsonACLProvider provider(jsonPath, false);

  // Path that doesn't match any entry or parent
  EXPECT_TRUE(provider.getACLs("other/path").empty()); // ALL_GROUPS
}

TEST_F(JsonACLProviderTest, InvalidJsonThrows) {
  auto jsonPath = writeJsonFile("not valid json {{{");

  EXPECT_THROW(JsonACLProvider(jsonPath, false), std::runtime_error);
}

TEST_F(JsonACLProviderTest, NonExistentFileThrows) {
  EXPECT_THROW(
      JsonACLProvider("/nonexistent/path/acls.json", false),
      std::runtime_error);
}

TEST_F(JsonACLProviderTest, NonObjectTopLevelThrows) {
  auto jsonPath = writeJsonFile("[1, 2, 3]");

  EXPECT_THROW(JsonACLProvider(jsonPath, false), std::runtime_error);
}

TEST_F(JsonACLProviderTest, MissingAclsKeyThrows) {
  auto jsonPath = writeJsonFile(R"({
    "other": {"alpha": 1}
  })");

  EXPECT_THROW(JsonACLProvider(jsonPath, false), std::runtime_error);
}

TEST_F(JsonACLProviderTest, NewFormatInheritance) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 5
    }
  })");

  // Default mode: sub-directories inherit
  JsonACLProvider provider(jsonPath, false);

  const std::vector<int> expected{5};

  EXPECT_EQ(provider.getACLs("src/foo"), expected);

  // Sub-directory inherits
  EXPECT_EQ(provider.getACLs("src/foo/bar/baz"), expected);

  // Unrelated path returns empty
  EXPECT_TRUE(provider.getACLs("other/path").empty());
}

TEST_F(JsonACLProviderTest, NewFormatPermissive) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/foo": 3
    }
  })");

  // Permissive mode
  JsonACLProvider provider(jsonPath, true);

  const std::vector<int> expected{3};
  EXPECT_EQ(provider.getACLs("src/foo"), expected);

  // Sub-directory does NOT inherit in permissive mode
  EXPECT_TRUE(provider.getACLs("src/foo/bar").empty());
}

TEST_F(JsonACLProviderTest, NewFormatInvalidIdFiltered) {
  auto jsonPath = writeJsonFile(R"({
    "acls": {
      "src/valid": 100,
      "src/negative": -1,
      "src/too_high": 254
    }
  })");

  JsonACLProvider provider(jsonPath);
  // Only the valid entry should be loaded
  EXPECT_EQ(provider.entryCount(), 1);

  const std::vector<int> expected{100};
  EXPECT_EQ(provider.getACLs("src/valid"), expected);

  // Invalid entries should not be present
  EXPECT_TRUE(provider.getACLs("src/negative").empty());
  EXPECT_TRUE(provider.getACLs("src/too_high").empty());
}

} // namespace facebook::glean::rts::acl
