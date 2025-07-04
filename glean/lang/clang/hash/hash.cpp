/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/SHA1.h>
#include <string>

namespace facebook::glean::clangx::hash {

std::string hash(llvm::StringRef input) {
  auto hash = llvm::SHA1::hash(llvm::arrayRefFromStringRef(input));
  return llvm::toHex(llvm::ArrayRef(hash.data(), 8));
}

} // namespace facebook::glean::clangx::hash

extern "C" {

int hash_ffi(const char* input, char* output, size_t output_size) {
  auto result = facebook::glean::clangx::hash::hash(input);
  if (result.length() + 1 > output_size) {
    return -1; // Buffer too small
  }
  strcpy(output, result.c_str());
  return 0; // Success
}
}
