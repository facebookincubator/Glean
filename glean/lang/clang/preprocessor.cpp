/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lang/clang/preprocessor.h"
#include <llvm/Config/llvm-config.h>

namespace {

using namespace facebook::glean::clangx;
using namespace facebook::glean::cpp;

struct PPCallbacks final : public clang::PPCallbacks {
  explicit PPCallbacks(ClangDB* d) : db(*d) {}

  Fact<Pp::Macro> macro(const clang::Token& name) {
    return db.fact<Pp::Macro>(static_cast<std::string>(name.getIdentifierInfo()->getName()));
  }

  // clang::PPCallbacks overrides
  void FileChanged(
      clang::SourceLocation loc,
      FileChangeReason reason,
      clang::SrcMgr::CharacteristicKind,
      clang::FileID) override {
    if (reason == clang::PPCallbacks::EnterFile) {
      db.enterFile(loc, last_include);
      last_include.reset();
    }
  }

#if LLVM_VERSION_MAJOR >= 11
  void FileSkipped(
      const clang::FileEntryRef& entry,
      const clang::Token&,
      clang::SrcMgr::CharacteristicKind) override {
    db.skipFile(last_include, &entry.getFileEntry());
    last_include.reset();
  }
#else
  void FileSkipped(
      const clang::FileEntry& entry,
      const clang::Token&,
      clang::SrcMgr::CharacteristicKind) override {
    db.skipFile(last_include, &entry);
    last_include.reset();
  }
#endif

  void InclusionDirective(
      clang::SourceLocation hashLoc,
      const clang::Token&,
      clang::StringRef,
      bool,
      clang::CharSourceRange filenameRange,
#if LLVM_VERSION_MAJOR >= 17
      clang::OptionalFileEntryRef file,
#elif LLVM_VERSION_MAJOR >= 15
      llvm::Optional<clang::FileEntryRef> file,
#else
      const clang::FileEntry *file,
#endif
      clang::StringRef,
      clang::StringRef,
      const clang::Module *
#if LLVM_VERSION_MAJOR >= 8
      ,clang::SrcMgr::CharacteristicKind
#endif
      ) override {
#if LLVM_VERSION_MAJOR >= 15
    // file may be empty if it was not found (a preprocessor error)
    if (file) {
      last_include = ClangDB::Include{hashLoc, filenameRange, &file->getFileEntry()};
    }
#else
    last_include = ClangDB::Include{hashLoc, filenameRange, file};
#endif
  }

  void Ifdef(
      clang::SourceLocation,
      const clang::Token& name,
    const clang::MacroDefinition& def) override {
    clang::SourceRange range(name.getLocation(), name.getEndLoc());
    macroUsed(name, def, range, false);
  }

  void Ifndef(
      clang::SourceLocation,
      const clang::Token& name,
    const clang::MacroDefinition& def) override {
    clang::SourceRange range(name.getLocation(), name.getEndLoc());
    macroUsed(name, def, range, false);
  }

  void MacroDefined(
      const clang::Token& name,
      const clang::MacroDirective *) override {
    auto loc = name.getLocation();
    // Skip predefined macros such as `__cplusplus`.
    // Built-in macros such as `__LINE__` aren't defined.
    if (db.sourceManager().getFileID(loc) ==
        db.preprocessor().getPredefinesFileID()) {
      return;
    }
    auto src = db.srcRange(loc);
    auto def = db.fact<Pp::Define>(macro(name), src.range);
    db.ppevent(Cxx::PPEvent::define(def), src);
  }

  void MacroUndefined(
      const clang::Token& name,
      const clang::MacroDefinition&,
      const clang::MacroDirective *) override {
    auto src = db.srcRange(name.getLocation());
    auto undef = db.fact<Pp::Undef>(macro(name), src.range);
    db.ppevent(Cxx::PPEvent::undef(undef), src);
  }

  void macroUsed(
      const clang::Token& name,
      const clang::MacroDefinition& def,
      clang::SourceRange range,
      bool expand) {

#define PROFILE_macroUsed 0

#if PROFILE_macroUsed
    using Clock = std::chrono::steady_clock;
    static std::chrono::microseconds time = std::chrono::microseconds::zero();
    static size_t count = 0;

    const auto start = Clock::now();
#endif


    // Getting the location is expensive and there are a lot fewer macro
    // definition sites than there are macro expansions so let's cache those
    // locations.
    folly::Optional<Src::Loc> defloc;
    if (auto info = def.getMacroInfo()) {
      if (info->isBuiltinMacro()) { // e.g. __LINE__
        return;
      }
      if (auto entry = folly::get_optional(macros, info)) {
        const auto& loc = *entry;
        // If an entry exists but is none, it means it's a predefined macro.
        if (!loc.has_value()) {
          return;
        }
        defloc = loc.value();
      } else {
        auto loc = info->getDefinitionLoc();
        if (db.sourceManager().getFileID(loc) ==
            db.preprocessor().getPredefinesFileID()) { // e.g. __cplusplus
          // Cache predefined macro.
          macros.insert({info, folly::none});
          return;
        }
        defloc = db.srcLoc(loc);
        macros.insert({info, defloc.value()});
      }
    }

    // We absolutely don't want to let Clang resolve nested macro expansion
    // ranges here (via db.srcRange -> getExpansionRange) as doing so turned out
    // to be horrendously expensive. Instead, when we see a top-level expansion
    // (the range isn't isMacroID) we resolve it ourselves and store it in
    // 'expansion'. Subsequent nested expansions must be part of this top-level
    // expansion so we just use that range. There is a subtlety with how we
    // handle expansions in macro arguments, see comments below. This is a
    // massive win in performance - at one time, this function accounted for
    // >40% of the running time in Strobelight (cf. T59197014).
    const ClangDB::SourceRange src =
      (range.getBegin().isMacroID() && expansion.has_value())

      // This is part of the current top-level expansion, just use its
      // range.
      ? expansion.value()

      // Manually convert this to a CharSourceRange and call the more
      // efficient immediateSourceRange rather than srcRange.
      : db.immediateSrcRange(
          clang::CharSourceRange::getCharRange(
            { // getBegin points at the start of the first token
              range.getBegin(),

              // getEnd points at the start of the last token.
              range.getEnd().getLocWithOffset(
                // Skip over the last token to make this an proper (exclusive)
                // char range.
                range.getBegin() == range.getEnd()
                  // The macro expansion range is only one token which must be
                  // the macro name.
                  ? name.getLength()

                  // Multiple tokens which means the last token must be the
                  // closing parenthesis.
                  : 1)
            }));

    if (range.getBegin().isMacroID() && !expansion.has_value()) {
      // This really shouldn't happen.
      LOG(ERROR)
        << "Unexpected nested macro expansion at "
        << range.printToString(db.sourceManager());
    }

    // Don't update expansion if this is an expansion of a macro argument.
    // Consider:
    //
    //   #define ONE 1
    //   #define TWO 2
    //   #define FOO ONE
    //   #define MACRO(x) x+TWO
    //   int y = MACRO(FOO);
    //
    // Here, we get the following calls:
    //
    //   MACRO(...) - the outer expansion (not isMacroID)
    //   FOO - argument (not isMacroID)
    //   ONE - definition of FOO (isMacroID)
    //   TWO - definition of MACRO(x) (isMacroID)
    //
    // We set expansion in the MACRO(...) call but if we then update it in
    // the FOO call, we'd assign the (nested) expansion of TWO to the range
    // of FOO. Instead, we don't update and assign the expansions of both
    // ONE and TWO (but not FOO!) to the range of MACRO(FOO). This is arguably
    // slightly less wrong. There doesn't seem to be an easy way to do better
    // without actually resolving the MacroID ranges. Perhaps we could hook
    // into the lexer somehow.
    if (!range.getBegin().isMacroID() &&
        (!expansion.has_value()
          || src.file != expansion->file
          || src.span.start + src.span.length
              > expansion->span.start + expansion->span.length)) {
      expansion = src;
    }

    // Resolve the name range manually, too. For top-level expansions, it's
    // the (inclusive for now) range of the name token. For nested expansions
    // the current schema is broken anyway - we assign the range of the
    // top-level expansion (it should be spelling file + range).
    const auto name_r = name.getLocation().isMacroID()
      ? src
      : db.immediateSrcRange(
          clang::CharSourceRange::getCharRange({
            name.getLocation(),
            name.getEndLoc()
          }));

    auto use = db.fact<Pp::Use>(
      macro(name),
      Src::ByteRange{name_r.span.start, name_r.span.start + name_r.span.length},
      maybe(defloc),
      expand,
      src.range,
      Src::ByteSpan{name_r.span.start, name_r.span.length});
    db.ppevent(Cxx::PPEvent::use(use), src);

#if PROFILE_macroUsed
    const auto end = Clock::now();

    time += std::chrono::duration_cast<std::chrono::microseconds>(end - start);
    ++count;
    if ((count % 10000) == 0) {
      LOG(INFO) << "macroUsed " << time.count() << "us (" << count << ")";
    }
#endif
  }

  void MacroExpands(
      const clang::Token& name,
      const clang::MacroDefinition& def,
      clang::SourceRange range,
      const clang::MacroArgs *) override {
    macroUsed(name, def, range, true);
  }

  void Defined(
      const clang::Token& name,
      const clang::MacroDefinition& def,
      clang::SourceRange range) override {
    macroUsed(name, def, range, false);
  }

  ClangDB& db;
  folly::Optional<ClangDB::Include> last_include;

  // The range of the current top-level macro expansion (see comments in
  // macroUsed).
  folly::Optional<ClangDB::SourceRange> expansion;

  // Cached locations of macro definitions (see macroUsed).
  folly::F14FastMap<const clang::MacroInfo *, folly::Optional<Src::Loc>> macros;
};

}

namespace facebook {
namespace glean {
namespace clangx {

std::unique_ptr<clang::PPCallbacks> newPPCallbacks(ClangDB* db) {
  return std::make_unique<PPCallbacks>(db);
}

}
}
}
