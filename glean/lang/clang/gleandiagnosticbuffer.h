/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/LLVM.h"
#include "clang/Basic/SourceLocation.h"

#include <string>
#include <vector>

namespace facebook {
namespace glean {
namespace clangx {

/*
 * Diagnostic consumer that buffers `maxDiagnosticsSize` error/fatal level
 * diagnostics. If `targetConsumer` is set, it also forwards diagnostics along
 * to an existing, already-initialized diagnostic consumer.
 */
class GleanDiagnosticBuffer : public clang::DiagnosticConsumer {
 public:
  using DiagList = std::vector<std::pair<clang::SourceLocation, std::string>>;
  using const_iterator = DiagList::const_iterator;

  explicit GleanDiagnosticBuffer(
      std::uint32_t maxDiagnosticsSize,
      std::unique_ptr<clang::DiagnosticConsumer> targetCosumer = {})
      : maxDiagnosticsSize_{maxDiagnosticsSize},
        targetCosumer_{std::move(targetCosumer)} {}

  void BeginSourceFile(
      const clang::LangOptions& LO,
      const clang::Preprocessor* PP) override {
    DiagnosticConsumer::BeginSourceFile(LO, PP);
    if (targetCosumer_) {
      targetCosumer_->BeginSourceFile(LO, PP);
    }
  }

  void clear() override {
    DiagnosticConsumer::clear();
    errors_.clear();
    if (targetCosumer_) {
      targetCosumer_->clear();
    }
  }

  void EndSourceFile() override {
    DiagnosticConsumer::EndSourceFile();
    if (targetCosumer_) {
      targetCosumer_->EndSourceFile();
    }
  }

  void finish() override {
    DiagnosticConsumer::finish();
    if (targetCosumer_) {
      targetCosumer_->EndSourceFile();
    }
  }

  bool IncludeInDiagnosticCounts() const override {
    return targetCosumer_
        ? targetCosumer_->IncludeInDiagnosticCounts()
        : clang::DiagnosticConsumer::IncludeInDiagnosticCounts();
  }

  void HandleDiagnostic(
      clang::DiagnosticsEngine::Level diagLevel,
      const clang::Diagnostic& info) override {
    // Default implementation (Warnings/errors count).
    DiagnosticConsumer::HandleDiagnostic(diagLevel, info);

    clang::SmallString<100> buff;
    info.FormatDiagnostic(buff);
    if (errors_.size() < maxDiagnosticsSize_ &&
        (diagLevel == clang::DiagnosticsEngine::Error ||
         diagLevel == clang::DiagnosticsEngine::Fatal)) {
      errors_.emplace_back(info.getLocation(), std::string(buff.str()));
    }

    if (targetCosumer_) {
      targetCosumer_->HandleDiagnostic(diagLevel, info);
    }
  }

  const_iterator err_begin() const {
    return errors_.begin();
  }
  const_iterator err_end() const {
    return errors_.end();
  }

 private:
  DiagList errors_;
  const std::uint32_t maxDiagnosticsSize_;
  std::unique_ptr<clang::DiagnosticConsumer> targetCosumer_;
};
} // namespace clangx
} // namespace glean
} // namespace facebook
