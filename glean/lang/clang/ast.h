// Copyright (c) Facebook, Inc. and its affiliates.

#pragma once

#include "glean/lang/clang/db.h"

namespace facebook {
namespace glean {
namespace clangx {

// Create a new ASTConsumer that writes to a particular ClangDB
std::unique_ptr<clang::ASTConsumer> newASTConsumer(ClangDB* db);

}
}
}
