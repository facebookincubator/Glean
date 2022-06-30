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

namespace facebook {
namespace glean {
namespace clangx {

namespace Buck = schema::Buck;
namespace Cxx = schema::Cxx1;
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
      : locator(env.locator),
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

  Fact<Src::File> fileFromEntry(const clang::FileEntry& entry);

  bool isPhysicalFile(clang::FileID id) {
    return sourceManager().getFileEntryForID(id);
  }

  folly::Optional<Fact<Src::File>> physicalFile(clang::FileID id) {
    folly::Optional<Fact<Src::File>> res = folly::none;
    if (auto data = folly::get_default(files, id, nullptr)) {
      res = data->fact;
    } else if (auto entry = sourceManager().getFileEntryForID(id)) {
      res = fileFromEntry(*entry);
    }
    if (res) {
      batch.fact<Src::FileLanguage>(res.value(), getLanguage());
    }
    return res;
  }

  Fact<Src::File> file(clang::FileID id) {
    if (auto x = physicalFile(id)) {
      return x.value();
    } else {
      return batch.fact<Src::File>(std::string("<builtin>"));
    }
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

  Fact<Cxx::Name> name(const clang::Token& name) {
    return batch.fact<Cxx::Name>(
      static_cast<std::string>(name.getIdentifierInfo()->getName()));
  }

  Fact<Cxx::Name> name(clang::StringRef ref) {
    return batch.fact<Cxx::Name>(static_cast<std::string>(ref));
  }

  struct Include {
    clang::SourceLocation hash;
    clang::CharSourceRange name;
    const clang::FileEntry *entry;
  };

  struct PreInclude {
    Fact<Pp::Include> include;
    clang::FileID file;
  };

  using PrePPEvent = std::variant<Cxx::PPEvent, PreInclude>;

  struct CrossRef {
    Src::ByteSpan span;
    bool local;
    Cxx::XRefTarget target;
  };

  struct FileData {
    clang::FileID id;
    Fact<Src::File> fact;
    std::vector<std::pair<Src::ByteSpan, Cxx::Declaration>> declarations;
    std::vector<PrePPEvent> events;
    std::vector<CrossRef> xrefs;
    folly::Optional<Fact<Cxx::Trace>> trace;
  };

  struct SourceRange {
    FileData *file;
    Src::ByteSpan span;
    Src::Range range;
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
      range.file->declarations.push_back({range.span, decl});
    }
  }

  void xref(
    clang::SourceRange range,
    folly::Optional<clang::SourceLocation> loc,
    Cxx::XRefTarget target);

  // Locations

  Src::Loc srcLoc(clang::SourceLocation loc);

  /// Return the SourceRange of the token starting at the beginning of the range
  /// if it isn't a macro expansion and the range itself otherwise.
  clang::SourceRange rangeOfToken(clang::SourceRange range) const;

  template<typename T>
  ClangDB::SourceRange srcRange(T x) {
    return immediateSrcRange(sourceManager().getExpansionRange(x));
  }
  SourceRange immediateSrcRange(clang::CharSourceRange r);

  clang::SourceRange spellingRange(clang::SourceRange range) const;

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
}
}
