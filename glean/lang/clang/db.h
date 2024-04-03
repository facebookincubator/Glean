/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#pragma once

#include <filesystem>
#include <variant>

#include <clang/Frontend/CompilerInstance.h>
#include <clang/Tooling/Tooling.h>
#include <llvm/Config/llvm-config.h>

#include <folly/gen/Base.h>
#include <folly/MapUtil.h>

#include "glean/lang/clang/gleandiagnosticbuffer.h"
#include "glean/lang/clang/schema.h"

namespace facebook::glean::clangx {

using namespace facebook::glean::cpp;
namespace Buck = schema::Buck;
namespace Cxx = schema::Cxx1;
namespace Fbthrift = schema::Fbthrift;
namespace Digest = schema::Digest;
namespace Pp = schema::Pp1;
namespace Src = schema::Src;
namespace Sys = schema::Sys;
using SCHEMA = schema::SCHEMA;


/**
 * A Clang AST batch
 *
 * Mostly provides utility functions for converting Clang things to
 * Glean data.
 *
 */

class ClangDB {
public:
  struct Env {
    // Buck cell of the file being indexed, relative to repo root
    folly::Optional<std::string> cell;
    Fact<Buck::Locator> locator;
    folly::Optional<Fact<Buck::Platform>> platform;
    std::filesystem::path root;
    folly::Optional<std::string> subdir;
    folly::Optional<std::string> path_prefix;
    Batch<SCHEMA>& batch;
  };

  ClangDB(
      const Env& env,
      clang::CompilerInstance& ci,
      GleanDiagnosticBuffer* diagnosticBuffer)
      : cell(env.cell),
        locator(env.locator),
        platform(env.platform),
        root(env.root),
        subdir(env.subdir),
        path_prefix(env.path_prefix),
        batch(env.batch),
        compilerInstance(ci),
        diagnosticBuffer(diagnosticBuffer) {}
  ClangDB(const ClangDB&) = delete;
  ClangDB operator=(const ClangDB&) = delete;

  // Files

  std::optional<std::pair<Fact<Src::File>, std::filesystem::path>>
  fileFromEntry(const clang::FileEntry& entry);

  bool isPhysicalFile(clang::FileID id) {
    return sourceManager().getFileEntryForID(id);
  }

  std::optional<std::pair<Fact<Src::File>, std::filesystem::path>> physicalFile(
      clang::FileID id) {
    std::optional<std::pair<Fact<Src::File>, std::filesystem::path>> res;
    if (auto data = folly::get_default(files, id, nullptr)) {
      res = {data->fact, data->path};
    } else if (auto entry = sourceManager().getFileEntryForID(id)) {
      res = fileFromEntry(*entry);
    }
    if (res) {
      batch.fact<Src::FileLanguage>(res->first, getLanguage());
    }
    return res;
  }

  Fact<Src::File> file(clang::FileID id) {
    auto x = physicalFile(id);
    return x ? x->first : batch.fact<Src::File>(std::string("<builtin>"));
  }

  void IndexFailure(const clang::ASTContext& ctx) {
    std::string errorMsgs;
    if (diagnosticBuffer != nullptr) {
        errorMsgs = fmt::format("{} error(s) occurred while indexing", diagnosticBuffer->getNumErrors());
        auto it = diagnosticBuffer->err_begin();
        auto end = diagnosticBuffer->err_end();
        for ( ; it != end; ++it) {
          errorMsgs.push_back('\n');
          errorMsgs.append(it->first.printToString(sourceManager()));
          errorMsgs.push_back(' ');
          errorMsgs.append(it->second);
        }
    }

    batch.fact<Src::IndexFailure>(
        file(ctx.getSourceManager().getMainFileID()),
        Src::IndexFailureReason::CompileError,
        errorMsgs);
  }

  // Names

  Fact<Cxx::Name> name(std::string s) {
    return batch.fact<Cxx::Name>(std::move(s));
  }

  Fact<Cxx::Name> name(clang::StringRef ref) {
    return name(ref.str());
  }

  Fact<Cxx::Name> name(const clang::Token& token) {
    return name(token.getIdentifierInfo()->getName());
  }

  struct Include {
    clang::SourceLocation hash;
    clang::CharSourceRange name;
    const clang::FileEntry *entry;
  };

  struct PreInclude {
    Fact<Pp::Include> include;
    folly::Optional<clang::FileID> file;
  };

  using PrePPEvent = std::variant<Cxx::PPEvent, PreInclude>;

  struct FileData;

  struct SourceRange {
    FileData *file;
    Src::ByteSpan span;
    Src::Range range;
  };

  using FullSourceRange = std::variant<
      SourceRange, // normal range
      std::pair<
          SourceRange, // expansion range
          std::optional<SourceRange>>>; // spelling range

  struct CrossRef {
    using SortID = std::variant<SourceRange, std::vector<std::string>>;
    using Spans = std::vector<Src::ByteSpan>;

    Cxx::XRefTarget target;
    SortID sort_id;
    Spans spans, expansions, spellings;
  };

  struct FileData {
    void xref(
        Src::ByteSpan span,
        CrossRef::Spans CrossRef::*get_spans,
        Cxx::XRefTarget target,
        CrossRef::SortID sort_id,
        bool local);

    clang::FileID id;
    std::filesystem::path path;
    Fact<Src::File> fact;
    std::vector<std::pair<Src::ByteSpan, Cxx::Declaration>> declarations;
    std::vector<PrePPEvent> events;
    struct {
      std::deque<CrossRef> fixed;
      std::deque<CrossRef> variable;
      std::map<Cxx::XRefTarget, CrossRef*> lookup;
    } xrefs;
    folly::Optional<Fact<Cxx::Trace>> trace;
    folly::Optional<Fact<Cxx::IncludeTree>> include_tree;
  };

  void ppevent(
    PrePPEvent event,
    SourceRange range);
  void include(
    const Include& inc,
    Fact<Src::File> file,
    folly::Optional<clang::FileID> id);
  void enterFile(clang::SourceLocation loc, folly::Optional<Include> inc);
  void skipFile(folly::Optional<Include> inc, const clang::FileEntry *entry);

  void declaration(const SourceRange& range, Cxx::Declaration decl) {
    if (range.file) {
      range.file->declarations.emplace_back(range.span, decl);
    }
  }

  void xref(
    clang::SourceRange range,
    Cxx::XRefTarget target,
    CrossRef::SortID sort_id,
    bool local);

  // Locations

  Src::Loc srcLoc(clang::SourceLocation loc);

  template<typename T>
  SourceRange srcRange(T x) {
    return immediateSrcRange(sourceManager().getExpansionRange(x));
  }

  FullSourceRange fullSrcRange(clang::SourceRange range);

public:

  SourceRange immediateSrcRange(clang::CharSourceRange r);

  clang::StringRef srcText(clang::SourceRange range) const;

  void finish();

  template<typename P, typename... Ts>
  Fact<P> fact(Ts&&... xs) {
    return batch.fact<P>(std::forward<Ts>(xs)...);
  }

  template<typename P, typename Key, typename Value>
  Fact<P> factV(Key&& key, Value&& value) {
    return batch.factV<P>(std::forward<Key>(key), std::forward<Value>(value));
  }

  clang::SourceManager& sourceManager() const {
    return compilerInstance.getSourceManager();
  }

  clang::Preprocessor& preprocessor() const {
    return compilerInstance.getPreprocessor();
  }

  const folly::Optional<std::string> cell;

private:
  Fact<Buck::Locator> locator;
  folly::Optional<Fact<Buck::Platform>> platform;
  const std::filesystem::path root;
  const folly::Optional<std::string> subdir;
  const folly::Optional<std::string> path_prefix;
  Batch<SCHEMA>& batch;
  clang::CompilerInstance& compilerInstance;
  const GleanDiagnosticBuffer *diagnosticBuffer;

  struct HashFileID {
    size_t operator()(clang::FileID id) const {
      return folly::Hash()(id.getHashValue());
    }
  };

  std::deque<FileData> file_data;
  folly::F14FastMap<clang::FileID, FileData *, HashFileID> files;

  /// returns the language processed by the compilerInstance
  Src::Language getLanguage() const {
    auto opts = compilerInstance.getLangOpts();
#if LLVM_VERSION_MAJOR >= 9
    if (opts.ObjC) {
#else
    // From
    // https://our.internmc.facebook.com/intern/diffusion/OMEXTLLVMPROJECT/browse/toolchain%252Fdev/clang/lib/CodeGen/CGObjCGNU.cpp?lines=431
    /// The version of the protocol class.  Used to differentiate between ObjC1
    /// and ObjC2 protocols.  Objective-C 1 protocols can not contain optional
    /// components and can not contain declared properties.  We always emit
    /// Objective-C 2 property structures, but we have to pretend that they're
    /// Objective-C 1 property structures when targeting the GCC runtime or it
    /// will abort.
    if (opts.ObjC1 || opts.ObjC2) {
#endif
      return opts.CPlusPlus ? Src::Language::ObjCpp : Src::Language::ObjC;
    }
    return opts.CPlusPlus ? Src::Language::Cpp : Src::Language::C;
  }
};

}
