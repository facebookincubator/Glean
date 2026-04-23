/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "glean/rts/acl/acl_provider.h"

namespace folly {
struct dynamic;
} // namespace folly

namespace facebook {
namespace glean {
namespace rts {
namespace acl {

/**
 * JsonACLProvider reads ACL group mappings from a JSON file.
 *
 * Expected JSON format:
 * {
 *   "acls": {
 *     "dirpath": 1,
 *     "another/path": 2
 *   }
 * }
 *
 * The "acls" section maps directory paths to integer group IDs.
 *
 * By default, sub-directories inherit ACL groups from their parent directories.
 * For example, if "foo" has group 1, then "foo/bar" and "foo/bar/baz"
 * will also return [1] unless they have their own explicit entry.
 *
 * If permissive mode is enabled, only exactly specified directories get their
 * groups applied; sub-directories without explicit entries return empty
 * (ALL_GROUPS - accessible to everyone).
 */
class JsonACLProvider : public ACLProvider {
 public:
  /**
   * Construct a JsonACLProvider from a JSON file.
   *
   * @param jsonPath Path to the JSON file containing ACL mappings
   * @param permissive If true, only exact directory matches apply;
   *                   if false (default), sub-directories inherit parent ACLs
   * @throws std::runtime_error if the file cannot be read or parsed
   */
  explicit JsonACLProvider(
      const std::string& jsonPath,
      bool permissive = false);

  ~JsonACLProvider() override = default;

  /**
   * Get ACL groups for a directory path.
   *
   * In default mode: looks up the path, then parent directories recursively
   * until a match is found.
   *
   * In permissive mode: only returns groups if the exact path is found.
   *
   * @param dirPath The directory path to look up
   * @return Vector of group IDs, or empty if no ACL is defined (ALL_GROUPS)
   */
  std::vector<int> getACLs(const std::string& dirPath) const override;

  /**
   * Check if the provider is in permissive mode.
   */
  bool isPermissive() const {
    return permissive_;
  }

  /**
   * Get the number of entries loaded from the JSON file.
   */
  size_t entryCount() const {
    return aclMap_.size();
  }

 private:
  // Load and parse the JSON file
  void loadFromFile(const std::string& jsonPath);

  // Find ACL groups with parent directory inheritance
  std::vector<int> findWithInheritance(const std::string& dirPath) const;

  // Normalize a directory path (remove trailing slashes, etc.)
  static std::string normalizeDirPath(const std::string& path);

  // Get the parent directory of a path
  static std::string getParentDir(const std::string& path);

  // Parse JSON: {"acls": {...}}
  void loadNewFormat(const folly::dynamic& json);

  // Map from directory path to ACL groups
  std::unordered_map<std::string, std::vector<int>> aclMap_;

  // If true, only exact matches apply; if false, sub-directories inherit
  bool permissive_;
};

} // namespace acl
} // namespace rts
} // namespace glean
} // namespace facebook
