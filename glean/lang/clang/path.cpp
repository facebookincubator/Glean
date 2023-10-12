/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lang/clang/path.h"

#include <algorithm>
#include <cassert>
#include <glog/logging.h>

namespace facebook {
namespace glean {
namespace clangx {

std::filesystem::path goodPath(
    std::filesystem::path root,
    std::filesystem::path path) {
  assert(root.is_absolute());
  path = path.lexically_normal();
  // NOTE: we can't use std's lexically_relative because
  // "../foo/bar".lexically_relative("/foo") isn't "bar".
  if (path.is_relative()) {
    auto root_begin = root.begin();
    auto root_end = root.end();
    auto path_begin = path.begin();
    auto path_end = path.end();
    auto done = false;
    while (root_begin != root_end && path_begin != path_end && !done) {
      if (*path_begin == "..") {
        --root_end;
        ++path_begin;
      } else if (root_end != root.end() && *path_begin == *root_end) {
        ++root_end;
        ++path_begin;
      } else {
        done = true;
      }
    }
    std::filesystem::path result;
    while (root_end != root.end()) {
      result /= "..";
      ++root_end;
    }
    while (path_begin != path_end) {
      result /= *path_begin;
      ++path_begin;
    }
    return result;
  } else {
    auto d = std::mismatch(
      path.begin(), path.end(), root.begin(), root.end());
    if (d.second == root.end()) {
      std::filesystem::path ret;
      for (auto i = d.first; i != path.end(); ++i) {
        ret /= *i;
      }
      return ret;
    } else {
      return path;
    }
  }
}

std::filesystem::path followSymlinksInsideRoot(
    std::filesystem::path root, std::filesystem::path source) {
  CHECK(source.is_relative());

  std::filesystem::path path; // current path
  std::filesystem::path tail = source; // remaining path

  for (auto next = tail.begin(); next != tail.end(); ) {
    auto component = *next;
    auto cur = root / path / component;
    if (std::filesystem::is_symlink(cur)) {
      auto target = std::filesystem::read_symlink(cur);
      auto resolved = goodPath(root, path / target);
      if (resolved.is_relative()) {
        // append the remaining tail to the symlink target
        for (auto it = ++next; it != tail.end(); it++) {
          target = target / *it;
        }
        tail = target;
        next = tail.begin();
        continue;
      } else {
        // we're not resolving this symlink because it points outside the root
        // let's try canonicalising the whole path just in case that gets us
        // back into the root.
        path /= component;
        for (auto it = ++next; it != tail.end(); it++) {
          path /= *it;
        }
        std::error_code ec;
        auto canon = std::filesystem::canonical(root / path, ec);
        if (!ec) {
          canon = goodPath(root, canon);
          if (canon.is_relative()) {
            return canon;
          }
        }
        return path;
      }
    } else {
      path = goodPath(root, path / component);
      next++;
    }
  }
  return path;
}

}
}
}
