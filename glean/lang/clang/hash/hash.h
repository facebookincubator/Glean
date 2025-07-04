/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <llvm/ADT/StringRef.h>
#include <string>

namespace facebook::glean::clangx::hash {

std::string hash(llvm::StringRef usr);

} // namespace facebook::glean::clangx::hash

extern "C" {

int hash_ffi(const char* usr, char* hash, size_t hash_size);
}
