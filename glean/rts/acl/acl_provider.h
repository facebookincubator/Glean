/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <memory>
#include <string>
#include <vector>

namespace facebook {
namespace glean {
namespace rts {
namespace acl {

/**
 * ACLProvider is the interface for looking up ACL groups for a file path.
 */
class ACLProvider {
 public:
  virtual ~ACLProvider() = default;

  /**
   * Get the list of ACL group IDs for a given directory path.
   *
   * @param dirPath The directory path (relative to repo root, without leading
   * /)
   * @return Vector of group IDs that have access to files in this directory.
   * This has OR semantic, i.e. either ACL is needed to access.
   * Returns empty vector if no specific ACL is defined (treated as public)
   */
  virtual std::vector<int> getACLs(const std::string& dirPath) const = 0;
};

/**
 * Factory function to create an ACLProvider from a JSON file.
 *
 * @param jsonPath Path to the JSON file containing ACL mappings
 * @param permissive If true, only exact directory matches apply;
 *                   if false (default), sub-directories inherit parent ACLs
 * @return A unique_ptr to the created JsonACLProvider
 * @throws std::runtime_error if the file cannot be read or parsed
 */
std::unique_ptr<ACLProvider> createJsonACLProvider(
    const std::string& jsonPath,
    bool permissive = false);

} // namespace acl
} // namespace rts
} // namespace glean
} // namespace facebook
