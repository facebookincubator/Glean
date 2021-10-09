// Copyright (c) Facebook, Inc. and its affiliates.

#include "glean/lang/clang/path.h"

#include <algorithm>

namespace facebook {
namespace glean {
namespace clangx {

boost::filesystem::path goodPath(
    boost::filesystem::path root,
    boost::filesystem::path path) {
  assert(root.is_absolute());
  path = path.lexically_normal();
  // NOTE: we can't use boost's lexically_relative because
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
    boost::filesystem::path result;
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
      boost::filesystem::path ret;
      for (auto i = d.first; i != path.end(); ++i) {
        ret /= *i;
      }
      return ret;
    } else {
      return path;
    }
  }
}

boost::filesystem::path betterPath(
    boost::filesystem::path path1,
    boost::filesystem::path path2) {
  return
    !path1.empty() && (path1.is_relative() || path2.is_absolute())
      ? path1
      : path2;
}

}
}
}
