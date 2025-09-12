/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <glean/glass/if/gen-cpp2/glass_types.h>
#include <gtest/gtest.h>
#include "glean/client/swift/GlassAccess.h"

// Test to verify that paths are returned without URL encoding
TEST(GlassAccessTest, PathsAreNotUrlEncoded) {
  GlassAccess glassAccess("/Users/test/repo");

  // Create a mock USRSymbolDefinition with a path that would need URL encoding
  ::glean::USRSymbolDefinition definition;

  // Create a location with a path containing spaces and special characters
  ::glean::LocationRange locationRange;
  ::glean::Range range;
  range.lineBegin() = 10;
  range.columnBegin() = 5;
  range.lineEnd() = 10;
  range.columnEnd() = 20;
  locationRange.range() = range;
  locationRange.filepath() = "path/with spaces/and&special=chars.swift";

  definition.location() = locationRange;

  // Convert to protocol::LocationList
  protocol::LocationList locations =
      glassAccess.convertUSRSymbolDefinitionToLocations(definition);

  // Verify that we got one location
  ASSERT_EQ(locations.size(), 1);

  // Verify that the URI contains the plain path (not URL encoded)
  const auto& location = locations[0];
  std::string expectedUri =
      "file:///Users/test/repo/path/with spaces/and&special=chars.swift";
  EXPECT_EQ(location.uri, expectedUri);

  // The path should contain spaces and special characters as-is (not URL
  // encoded)
  EXPECT_TRUE(location.uri.find("with spaces") != std::string::npos);
  EXPECT_TRUE(location.uri.find("&special=") != std::string::npos);

  // Should NOT contain URL encoding patterns
  EXPECT_TRUE(
      location.uri.find("%20") == std::string::npos); // No encoded spaces
  EXPECT_TRUE(location.uri.find("%26") == std::string::npos); // No encoded &
  EXPECT_TRUE(location.uri.find("%3D") == std::string::npos); // No encoded =
}
