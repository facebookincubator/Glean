/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <filesystem>

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
/// entirely unclear what the exact semantics of various std::filesystem
/// functions (e.g., relative) is. None of them seem to quite do what we want.
///
/// TODO: We probably want to either always return a path that's relative to
/// root or just fail if path isn't under root. We probably also want to resolve
/// symlinks as long as they don't point to something outside of root.
std::filesystem::path goodPath(
  std::filesystem::path root,
  std::filesystem::path path);

// Resolve symlinks in source, but only those that keep the path inside root.
// Where the path traverses symlinks, those are resolved too.
//
// root must be absolute, source must be relative.
//
std::filesystem::path followSymlinksInsideRoot(
    std::filesystem::path root, std::filesystem::path source);
}
}
}
