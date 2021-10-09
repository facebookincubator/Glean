// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/lang/clang/db.h"

namespace facebook {
namespace glean {
namespace clangx {

// Create a new PPCallbacks that write to a particular ClangDB
std::unique_ptr<clang::PPCallbacks> newPPCallbacks(ClangDB *db);

}
}
}
