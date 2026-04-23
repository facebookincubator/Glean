/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/rts/acl/json_acl_provider.h"

#include <folly/FileUtil.h>
#include <folly/json/dynamic.h>
#include <folly/json/json.h>
#include <glog/logging.h>

namespace facebook {
namespace glean {
namespace rts {
namespace acl {

JsonACLProvider::JsonACLProvider(const std::string& jsonPath, bool permissive)
    : permissive_(permissive) {
  loadFromFile(jsonPath);
  LOG(INFO) << "JsonACLProvider: loaded " << aclMap_.size() << " entries from "
            << jsonPath
            << (permissive_ ? " (permissive mode)" : " (inherited mode)");
}

void JsonACLProvider::loadFromFile(const std::string& jsonPath) {
  std::string contents;
  if (!folly::readFile(jsonPath.c_str(), contents)) {
    throw std::runtime_error(
        "JsonACLProvider: failed to read file: " + jsonPath);
  }

  folly::dynamic json;
  try {
    json = folly::parseJson(contents);
  } catch (const std::exception& e) {
    throw std::runtime_error(
        "JsonACLProvider: failed to parse JSON from " + jsonPath + ": " +
        e.what());
  }

  if (!json.isObject()) {
    throw std::runtime_error(
        "JsonACLProvider: expected JSON object at top level in " + jsonPath);
  }

  loadNewFormat(json);
}

void JsonACLProvider::loadNewFormat(const folly::dynamic& json) {
  // Parse "acls" section: {dir_path: int}
  auto* aclsPtr = json.get_ptr("acls");
  if (!aclsPtr || !aclsPtr->isObject()) {
    throw std::runtime_error(
        "JsonACLProvider: new format missing 'acls' object");
  }

  for (const auto& kv : aclsPtr->items()) {
    const auto& dirPath = kv.first.asString();
    const auto& value = kv.second;

    if (!value.isInt()) {
      LOG(WARNING) << "JsonACLProvider: expected integer value for path: "
                   << dirPath;
      continue;
    }

    auto rawId = value.asInt();
    if (rawId < 0 || rawId > 253) {
      LOG(WARNING) << "JsonACLProvider: group ID " << rawId
                   << " out of range [0, 253] for path: " << dirPath;
      continue;
    }

    int groupId = static_cast<int>(rawId);
    std::string normalizedPath = normalizeDirPath(dirPath);
    aclMap_[normalizedPath] = {groupId};

    VLOG(2) << "JsonACLProvider: loaded ACL for " << normalizedPath
            << ": group " << groupId;
  }
}

std::vector<int> JsonACLProvider::getACLs(const std::string& dirPath) const {
  std::string normalizedPath = normalizeDirPath(dirPath);

  // First, check for an exact match
  auto it = aclMap_.find(normalizedPath);
  if (it != aclMap_.end()) {
    VLOG(2) << "JsonACLProvider: exact match for " << normalizedPath;
    return it->second;
  }

  // In permissive mode, only exact matches apply
  if (permissive_) {
    VLOG(2) << "JsonACLProvider: no exact match for " << normalizedPath
            << " (permissive mode, returning ALL_GROUPS)";
    return {};
  }

  // In default mode, walk from leaf to root looking for nearest parent with
  // an ACL entry. This is O(depth) hash lookups, which is efficient given the
  // sparse ACL map and typical directory depths of 3-6 levels.
  return findWithInheritance(normalizedPath);
}

std::vector<int> JsonACLProvider::findWithInheritance(
    const std::string& dirPath) const {
  std::string current = dirPath;

  while (!current.empty()) {
    current = getParentDir(current);
    if (current.empty()) {
      break;
    }

    auto it = aclMap_.find(current);
    if (it != aclMap_.end()) {
      VLOG(2) << "JsonACLProvider: inherited ACLs from " << current << " for "
              << dirPath;
      return it->second;
    }
  }

  VLOG(2) << "JsonACLProvider: no ACLs found for " << dirPath
          << " (returning ALL_GROUPS)";
  return {};
}

std::string JsonACLProvider::normalizeDirPath(const std::string& path) {
  if (path.empty()) {
    return "";
  }

  std::string result = path;

  // Remove trailing slashes
  while (!result.empty() && result.back() == '/') {
    result.pop_back();
  }

  // Remove leading slashes
  while (!result.empty() && result.front() == '/') {
    result.erase(0, 1);
  }

  return result;
}

std::string JsonACLProvider::getParentDir(const std::string& path) {
  if (path.empty()) {
    return "";
  }

  auto pos = path.rfind('/');
  if (pos == std::string::npos) {
    return "";
  }

  return path.substr(0, pos);
}

} // namespace acl
} // namespace rts
} // namespace glean
} // namespace facebook
