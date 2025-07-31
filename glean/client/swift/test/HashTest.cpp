/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/client/swift/hash.h"
#include <gtest/gtest.h>
#include <string>

class HashTest : public ::testing::Test {
 protected:
  void SetUp() override {}
  void TearDown() override {}
};

TEST_F(HashTest, EmptyString) {
  // Test hash of empty string
  std::string input;
  std::string result = facebook::glean::clangx::hash::hash(input);

  // SHA1 of empty string should be DA39A3EE5E6B4B0D3255BFEF95601890AFD80709
  // First 8 bytes (16 hex chars) should be: DA39A3EE5E6B4B0D
  EXPECT_EQ(result.length(), 16);
  EXPECT_EQ(result, "DA39A3EE5E6B4B0D");
}

TEST_F(HashTest, SingleCharacter) {
  // Test hash of single character
  std::string input = "a";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // SHA1 of "a" should be 86F7E437FAA5A7FCE15D1DDCB9EAEAEA377667B8
  // First 8 bytes (16 hex chars) should be: 86F7E437FAA5A7FC
  EXPECT_EQ(result.length(), 16);
  EXPECT_EQ(result, "86F7E437FAA5A7FC");
}

TEST_F(HashTest, SimpleString) {
  // Test hash of simple string
  std::string input = "abc";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // SHA1 of "abc" should be A9993E364706816ABA3E25717850C26C9CD0D89D
  // First 8 bytes (16 hex chars) should be: A9993E364706816A
  EXPECT_EQ(result.length(), 16);
  EXPECT_EQ(result, "A9993E364706816A");
}

TEST_F(HashTest, LongerString) {
  // Test hash of longer string
  std::string input = "The quick brown fox jumps over the lazy dog";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // SHA1 of this string should be 2FD4E1C67A2D28FCED849EE1BB76E7391B93EB12
  // First 8 bytes (16 hex chars) should be: 2FD4E1C67A2D28FC
  EXPECT_EQ(result.length(), 16);
  EXPECT_EQ(result, "2FD4E1C67A2D28FC");
}

TEST_F(HashTest, SwiftUSRExample) {
  // Test hash of a typical Swift USR
  std::string input =
      "s:12IGFriendsMap0aB4ViewC03mapC0So05MKMapC0CvgAFyXEfU_ADL_AFvp";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // Verify it produces a valid 16-character uppercase hex string
  EXPECT_EQ(result.length(), 16);

  // Verify all characters are valid uppercase hex
  for (char c : result) {
    EXPECT_TRUE((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))
        << "Invalid hex character: " << c;
  }
}

TEST_F(HashTest, CppUSRExample) {
  // Test hash of a typical C++ USR
  std::string input = "c:@N@std@S@vector>#T#$@N@std@S@allocator>#T";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // Verify it produces a valid 16-character uppercase hex string
  EXPECT_EQ(result.length(), 16);

  // Verify all characters are valid uppercase hex
  for (char c : result) {
    EXPECT_TRUE((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))
        << "Invalid hex character: " << c;
  }
}

TEST_F(HashTest, SpecialCharacters) {
  // Test hash with special characters
  std::string input = "!@#$%^&*()_+-=[]{}|;':\",./<>?";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // Verify it produces a valid 16-character uppercase hex string
  EXPECT_EQ(result.length(), 16);

  // Verify all characters are valid uppercase hex
  for (char c : result) {
    EXPECT_TRUE((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))
        << "Invalid hex character: " << c;
  }
}

TEST_F(HashTest, UnicodeCharacters) {
  // Test hash with Unicode characters
  std::string input = "Hello 世界 🌍";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // Verify it produces a valid 16-character uppercase hex string
  EXPECT_EQ(result.length(), 16);

  // Verify all characters are valid uppercase hex
  for (char c : result) {
    EXPECT_TRUE((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))
        << "Invalid hex character: " << c;
  }
}

TEST_F(HashTest, LongString) {
  // Test hash of very long string (> 64 bytes to test multi-block processing)
  std::string input = std::string(1000, 'x'); // 1000 'x' characters
  std::string result = facebook::glean::clangx::hash::hash(input);

  // Verify it produces a valid 16-character uppercase hex string
  EXPECT_EQ(result.length(), 16);

  // Verify all characters are valid uppercase hex
  for (char c : result) {
    EXPECT_TRUE((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))
        << "Invalid hex character: " << c;
  }
}

TEST_F(HashTest, Consistency) {
  // Test that the same input always produces the same hash
  std::string input = "consistency_test_string";
  std::string result1 = facebook::glean::clangx::hash::hash(input);
  std::string result2 = facebook::glean::clangx::hash::hash(input);
  std::string result3 = facebook::glean::clangx::hash::hash(input);

  EXPECT_EQ(result1, result2);
  EXPECT_EQ(result2, result3);
  EXPECT_EQ(result1, result3);
}

TEST_F(HashTest, DifferentInputsDifferentHashes) {
  // Test that different inputs produce different hashes
  std::string input1 = "test1";
  std::string input2 = "test2";
  std::string input3 = "test11"; // Similar but different

  std::string result1 = facebook::glean::clangx::hash::hash(input1);
  std::string result2 = facebook::glean::clangx::hash::hash(input2);
  std::string result3 = facebook::glean::clangx::hash::hash(input3);

  EXPECT_NE(result1, result2);
  EXPECT_NE(result2, result3);
  EXPECT_NE(result1, result3);
}

TEST_F(HashTest, CaseSensitivity) {
  // Test that hash is case sensitive
  std::string input1 = "Test";
  std::string input2 = "test";
  std::string input3 = "TEST";

  std::string result1 = facebook::glean::clangx::hash::hash(input1);
  std::string result2 = facebook::glean::clangx::hash::hash(input2);
  std::string result3 = facebook::glean::clangx::hash::hash(input3);

  EXPECT_NE(result1, result2);
  EXPECT_NE(result2, result3);
  EXPECT_NE(result1, result3);
}

TEST_F(HashTest, OutputFormat) {
  // Test that output is always uppercase hex
  std::string input = "format_test";
  std::string result = facebook::glean::clangx::hash::hash(input);

  // Should be exactly 16 characters
  EXPECT_EQ(result.length(), 16);

  // Should contain only uppercase hex characters
  for (char c : result) {
    EXPECT_TRUE((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F'))
        << "Invalid hex character: " << c;
  }
}

TEST_F(HashTest, KnownVectorValidation) {
  // Test against known SHA1 test vectors to ensure correctness

  // Test vector 1: "abc"
  EXPECT_EQ(facebook::glean::clangx::hash::hash("abc"), "A9993E364706816A");

  // Test vector 2: ""
  EXPECT_EQ(facebook::glean::clangx::hash::hash(""), "DA39A3EE5E6B4B0D");

  // Test vector 3: "a"
  EXPECT_EQ(facebook::glean::clangx::hash::hash("a"), "86F7E437FAA5A7FC");

  // Test vector 4: "message digest"
  EXPECT_EQ(
      facebook::glean::clangx::hash::hash("message digest"),
      "C12252CEDA8BE899");

  // Test vector 5: "abcdefghijklmnopqrstuvwxyz"
  EXPECT_EQ(
      facebook::glean::clangx::hash::hash("abcdefghijklmnopqrstuvwxyz"),
      "32D10C7B8CF96570");
}
