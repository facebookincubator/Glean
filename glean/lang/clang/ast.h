/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "glean/lang/clang/db.h"

namespace facebook::glean::clangx {

// Create a new ASTConsumer that writes to a particular ClangDB
std::unique_ptr<clang::ASTConsumer> newASTConsumer(ClangDB* db);

} // namespace facebook::glean::clangx
