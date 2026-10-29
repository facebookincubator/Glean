/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#if GLEAN_FACEBOOK
#include "glean/lang/clang/preprocessor.h"
#else
#include "preprocessor.h"
#endif

#include <vector>

#include <llvm/Config/llvm-config.h>

namespace {

using namespace facebook::glean::clangx;
using namespace facebook::glean::cpp;

/// Records what the preprocessor does in a `ClangDB`: the files it enters or
/// skips, and the macros defined, undefined and used, as `pp1` facts and as
/// events of each file's `cxx1.PPTrace`.
struct PPCallbacks final : public clang::PPCallbacks {
  explicit PPCallbacks(ClangDB* d) : db(*d) {}

  Fact<Pp::Macro> macro(const clang::Token& name) {
    return db.fact<Pp::Macro>(
        static_cast<std::string>(name.getIdentifierInfo()->getName()));
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
    db.skipFile(last_include, entry);
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
#if LLVM_VERSION_MAJOR >= 17 || \
    (LLVM_VERSION_MAJOR >= 16 && defined(CAST_TARGET_REPO_FBANDROID))
      clang::OptionalFileEntryRef file,
#elif LLVM_VERSION_MAJOR >= 15
      llvm::Optional<clang::FileEntryRef> file,
#else
      const clang::FileEntry* file,
#endif
      clang::StringRef,
      clang::StringRef,
      const clang::Module*,
#if LLVM_VERSION_MAJOR >= 19 || defined(CAST_COMPILER_TOOLCHAIN_FBOBJC_16)
      bool,
#endif
      clang::SrcMgr::CharacteristicKind) override {
#if LLVM_VERSION_MAJOR >= 15
    // file may be empty if it was not found (a preprocessor error)
    if (file) {
      last_include =
          ClangDB::Include{hashLoc, filenameRange, &file->getFileEntry()};
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

  void MacroDefined(const clang::Token& name, const clang::MacroDirective*)
      override {
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
      const clang::MacroDirective*) override {
    auto src = db.srcRange(name.getLocation());
    auto undef = db.fact<Pp::Undef>(macro(name), src.range);
    db.ppevent(Cxx::PPEvent::undef(undef), src);
  }

  /// A macro use that we record.
  struct MacroUse {
    /// The macro name, as reported by the preprocessor callback.
    const clang::Token* name;
    /// The definition the name resolves to, or null if it isn't a defined
    /// macro (e.g. in `#ifdef`).
    const clang::MacroInfo* FOLLY_NULLABLE info;
    /// The location of `info`, or none if `info` is null.
    folly::Optional<Src::Loc> defloc;
    /// Whether the macro is expanded, as opposed to tested by `#ifdef`,
    /// `#ifndef` or `defined`.
    bool expand;
  };

  /// Records a use of the macro `name` over `range`, the source range the
  /// preprocessor reports for it. The macro is expanded if `expand`, or else
  /// tested by `#ifdef`, `#ifndef` or `defined`. `def` is the definition the
  /// name resolves to, if any. Uses of built-in macros such as `__LINE__` and
  /// of predefined macros such as `__cplusplus` aren't recorded.
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

    const auto* info = def.getMacroInfo();
    if (info && info->isBuiltinMacro()) { // e.g. __LINE__
      return;
    }

    if (isTopLevel(range)) {
      topLevelMacroUsed(name, info, range, expand);
    } else {
      nestedMacroUsed(name, info, expand);
    }

#if PROFILE_macroUsed
    const auto end = Clock::now();

    time += std::chrono::duration_cast<std::chrono::microseconds>(end - start);
    ++count;
    if ((count % 10000) == 0) {
      LOG(INFO) << "macroUsed " << time.count() << "us (" << count << ")";
    }
#endif
  }

  /// Whether a macro use whose expansion range is `range` is top-level:
  /// written in a file, possibly in the arguments of another macro, rather
  /// than produced by expanding another macro.
  static bool isTopLevel(clang::SourceRange range) {
    return !range.getBegin().isMacroID();
  }

  /// Records a top-level use. It is located at the macro name plus its
  /// arguments, if any.
  void topLevelMacroUsed(
      const clang::Token& name,
      const clang::MacroInfo* FOLLY_NULLABLE info,
      clang::SourceRange range,
      bool expand) {
    const auto src = topLevelSrcRange(name, range);
    if (expand) {
      // Even for predefined macros, whose uses we don't record, so that the
      // nested uses they produce are never attributed to an earlier expansion.
      enterTopLevelExpansion(src);
    }
    const auto use = recordedUse(name, info, expand);
    if (!use.has_value()) {
      return;
    }
    const auto name_range = db.immediateSrcRange(
        clang::CharSourceRange::getCharRange(
            {name.getLocation(), name.getEndLoc()}));
    db.ppevent(Cxx::PPEvent::use(useFact(*use, src, name_range)), src);
  }

  /// The range of a top-level use: the macro name, plus its arguments if any.
  ClangDB::SourceRange topLevelSrcRange(
      const clang::Token& name,
      clang::SourceRange range) {
    // Manually convert this to a CharSourceRange and call the more efficient
    // immediateSrcRange rather than srcRange.
    return db.immediateSrcRange(
        clang::CharSourceRange::getCharRange(
            {// getBegin points at the start of the first token
             range.getBegin(),

             // getEnd points at the start of the last token.
             range.getEnd().getLocWithOffset(
                 // Skip over the last token to make this a proper
                 // (exclusive) char range.
                 range.getBegin() == range.getEnd()
                     // The macro expansion range is only one token which
                     // must be the macro name.
                     ? name.getLength()

                     // Multiple tokens which means the last token must be
                     // the closing parenthesis.
                     : 1)}));
  }

  /// Makes `src` the current top-level expansion, unless `src` is in the same
  /// file and doesn't end after it, as with a macro argument. Consider:
  ///
  ///   #define ONE 1
  ///   #define TWO 2
  ///   #define FOO ONE
  ///   #define MACRO(x) x+TWO
  ///   int y = MACRO(FOO);
  ///
  /// The preprocessor reports the top-level expansions of `MACRO(FOO)` and of
  /// its argument `FOO`, then the nested expansions of `ONE` and `TWO`. As
  /// `MACRO(FOO)` stays the current top-level expansion, both nested
  /// expansions happen within it.
  void enterTopLevelExpansion(const ClangDB::SourceRange& src) {
    if (!expansion.has_value() || src.file != expansion->file ||
        endOffset(src.span) > endOffset(expansion->span)) {
      expansion = src;
    }
  }

  /// Records a use produced by expanding another macro, such as `N` in
  ///
  ///   #define N 1
  ///   #define M N
  ///   int x = M;
  ///
  /// in the trace of the file containing the top-level expansion (`M`). The use
  /// is located at that top-level expansion.
  void nestedMacroUsed(
      const clang::Token& name,
      const clang::MacroInfo* FOLLY_NULLABLE info,
      bool expand) {
    const auto use = recordedUse(name, info, expand);
    if (!use.has_value()) {
      return;
    }
    // Don't let Clang resolve nested macro expansion ranges (via db.srcRange
    // -> getExpansionRange): that turned out to be horrendously expensive
    // (cf. T59197014). The memoized start of the top-level expansion is cheap,
    // and its range is usually the current top-level expansion.
    const auto& sm = db.sourceManager();
    const auto root = expansionRoot(name.getLocation());
    const auto [root_id, root_offset] = sm.getDecomposedLoc(root);
    auto* file = db.fileData(root_id);
    if (file == nullptr) {
      return;
    }
    listExpandedUse(*use, file, root, root_offset);
  }

  /// The file location where the top-level macro expansion containing `loc`
  /// starts. Memoized for each macro expansion it walks through.
  ///
  /// This doesn't rely on the most recent top-level `MacroExpands` callback,
  /// because some expansions don't start with one: `_Pragma`, built-in and
  /// predefined macros, and macro arguments read across the end of a file.
  clang::SourceLocation expansionRoot(clang::SourceLocation loc) {
    const auto& sm = db.sourceManager();
    std::vector<unsigned> unresolved_ids;
    while (loc.isMacroID()) {
      const auto id = sm.getFileID(loc);
      if (auto root = folly::get_optional(expansion_roots, id.getHashValue())) {
        loc = *root;
        break;
      }
      unresolved_ids.push_back(id.getHashValue());
      loc = sm.getSLocEntry(id).getExpansion().getExpansionLocStart();
    }
    for (const auto id : unresolved_ids) {
      expansion_roots.insert({id, loc});
    }
    return loc;
  }

  /// Lists a nested use in the trace of `file`, located at the top-level
  /// macro use that starts at `root`.
  void listExpandedUse(
      const MacroUse& use,
      ClangDB::FileData* file,
      clang::SourceLocation root,
      unsigned root_offset) {
    const auto site = topLevelSite(root, file, root_offset);
    db.ppevent(Cxx::PPEvent::use(useFact(use, site, site)), file);
  }

  /// The range of the top-level macro use that starts at `root`, which is at
  /// `root_offset` in `file`: the current top-level expansion (arguments
  /// included) if it contains `root`, or else just the token at `root`.
  ClangDB::SourceRange topLevelSite(
      clang::SourceLocation root,
      ClangDB::FileData* file,
      unsigned root_offset) {
    if (expansion.has_value() && expansion->file == file &&
        expansion->span.start <= root_offset &&
        root_offset < endOffset(expansion->span)) {
      return expansion.value();
    }
    return db.immediateSrcRange(
        clang::CharSourceRange::getTokenRange(root, root));
  }

  /// The use of the macro `name` that resolves to `info` (null if `name`
  /// isn't a defined macro), or none if we don't record it because the macro
  /// is predefined, such as `__cplusplus`.
  folly::Optional<MacroUse> recordedUse(
      const clang::Token& name,
      const clang::MacroInfo* FOLLY_NULLABLE info,
      bool expand) {
    if (info == nullptr) {
      return MacroUse{&name, nullptr, folly::none, expand};
    }
    auto defloc = definitionLoc(info);
    if (!defloc.has_value()) {
      return folly::none;
    }
    return MacroUse{&name, info, std::move(defloc), expand};
  }

  /// The location of the macro definition `info`, or none for a predefined
  /// macro, whose definition isn't in a file.
  ///
  /// Cached, because getting the location is expensive and there are far
  /// fewer macro definitions than macro uses.
  folly::Optional<Src::Loc> definitionLoc(const clang::MacroInfo* info) {
    if (auto entry = folly::get_optional(macros, info)) {
      return *entry;
    }
    const auto loc = info->getDefinitionLoc();
    folly::Optional<Src::Loc> result;
    if (db.sourceManager().getFileID(loc) !=
        db.preprocessor().getPredefinesFileID()) {
      result = db.srcLoc(loc);
    }
    macros.insert({info, result});
    return result;
  }

  /// A `pp1.Use` fact for `use` in `range`, with the macro name at
  /// `name_range`.
  Fact<Pp::Use> useFact(
      const MacroUse& use,
      const ClangDB::SourceRange& range,
      const ClangDB::SourceRange& name_range) {
    return db.fact<Pp::Use>(
        macro(*use.name),
        maybe(use.defloc),
        use.expand,
        range.range,
        name_range.span);
  }

  /// The offset just past the end of `span`.
  static uint64_t endOffset(const Src::ByteSpan& span) {
    return span.start + span.length;
  }

  void MacroExpands(
      const clang::Token& name,
      const clang::MacroDefinition& def,
      clang::SourceRange range,
      const clang::MacroArgs*) override {
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

  /// The range of the current top-level macro expansion (see
  /// `enterTopLevelExpansion`).
  folly::Optional<ClangDB::SourceRange> expansion;

  /// Memoized results of `expansionRoot`, by the hash value of the `FileID` of
  /// each macro expansion.
  folly::F14FastMap<unsigned, clang::SourceLocation> expansion_roots;

  /// Cached results of `definitionLoc`.
  folly::F14FastMap<const clang::MacroInfo*, folly::Optional<Src::Loc>> macros;
};

} // namespace

namespace facebook::glean::clangx {

std::unique_ptr<clang::PPCallbacks> newPPCallbacks(ClangDB* db) {
  return std::make_unique<PPCallbacks>(db);
}

} // namespace facebook::glean::clangx
