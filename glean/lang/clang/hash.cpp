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
