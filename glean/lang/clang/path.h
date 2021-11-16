/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <boost/filesystem.hpp>

namespace facebook {
namespace glean {
namespace clangx {

/// Munge a Clang path into something nice.
///
/// Absolute paths: try to make them relative to root.
///
/// Relative paths: Resolve leading "..". For instance, if the path is
/// "../src/something" and we're in "src", change it to "something".
///
/// This doesn't resolve through symbolic links. For the moment, the code that
/// calls us does but we probably should do that here eventually. Sadly, it is
/// entirely unclear what the exact semantics of various boost::filesystem
/// functions (e.g., relative) is. None of them seem to quite do what we want.
///
/// TODO: We probably want to either always return a path that's relative to
/// root or just fail if path isn't under root. We probably also want to resolve
/// symlinks as long as they don't point to something outside of root.
boost::filesystem::path goodPath(
  boost::filesystem::path root,
  boost::filesystem::path path);

/// Given two paths, pick the better one, preferring the first. At the moment,
/// this only prefers relative paths to absolute ones.
boost::filesystem::path betterPath(
  boost::filesystem::path path1,
  boost::filesystem::path path2);

}
}
}
