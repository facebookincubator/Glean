/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lang/clang/db.h"
#include <llvm/Support/SHA1.h>

#include <folly/Overload.h>

#include "glean/lang/clang/path.h"
#if GLEAN_FACEBOOK
#include "glean/facebook/lang/clang/path.h"
#endif

namespace facebook {
namespace glean {
namespace clangx {

namespace {

std::filesystem::path subpath(
    const folly::Optional<std::string>& subdir,
    std::filesystem::path p) {
  // This returns 'path' if it is absolute or if 'subdir' is empty and
  // 'subdir'/'path' otherwise.
  return p.is_absolute() || !subdir
    ? p
    : std::filesystem::path(subdir.value()) / p;
}

}

//
// To get a good filename we need to deal with the cases below:
//
// 1. Name is relative, e.g. "folly/File.h". In this case, prepending
//    subdir (if set) gives us a path relative to root, and goodPath()
//    removes any "../" components.
//
// 2. Name is absolute and inside root,
//    e.g. "/home/smarlow/code/folly/File.h" where root is
//    "/home/smarlow/code". In this case we strip root using
//    goodPath(), giving us "folly/File.h".
//
// 3. Name is a symlink (absolute or relative) to a file inside
//    root. Use canonical() to get the symlink target, and goodPath()
//    to strip root.
//
// 4. Name is relative, but is a symlink to a file *outside* root. In
//    this case we want to just keep the original relative path.
//
// 5. (META only) Name is relative and inside buck-out/v2,
//      e.g. "buck-out/v2/gen/fbcode/<hash>/...".
//    Replace the buck-out hash with the file contents hash to eliminate dupes
//
std::optional<std::pair<Fact<Src::File>, std::filesystem::path>>
ClangDB::fileFromEntry(const clang::FileEntry& entry) {
  auto path = goodPath(root,
                        std::filesystem::canonical(entry.getName().str()));
  if (path.is_absolute()) {
    path = goodPath(root, subpath(subdir, entry.getName().str()));
  }
  if (path_prefix.has_value()) {
    path = std::filesystem::path(path_prefix.value()) / path;
  }
  const auto buffer = [&] {
#if LLVM_VERSION_MAJOR >= 12
    return sourceManager().getMemoryBufferForFileOrNone(&entry);
#else
    bool invalid = false;
    auto buffer = sourceManager().getMemoryBufferForFile(&entry, &invalid);
    return !invalid ? buffer : nullptr;
#endif
  }();
  if (!buffer) {
    LOG(WARNING) << "Couldn't get MemoryBuffer for " << path.native();
#if GLEAN_FACEBOOK
    if (isBuckOutPath(path)) {
      return {};
    }
#endif
    const auto file = batch.fact<Src::File>(path.native());
    return std::pair{file, std::move(path)};
  }
  // compute the SHA1 digest of the file content and get the file size
  auto hash = llvm::toHex(
      llvm::SHA1::hash(llvm::arrayRefFromStringRef(buffer->getBuffer())),
      true /* LowerCase */);
#if GLEAN_FACEBOOK
  if (isBuckOutPath(path)) {
    path = replaceBuckOutHash(path, hash);
  }
#endif
  const auto file = batch.fact<Src::File>(path.native());
  const uint64_t size = entry.getSize();
  batch.fact<Digest::FileDigest>(file, Digest::Digest{hash, size});

  // compute the line endings and unicode status
  std::vector<uint64_t> lengths;
  bool hasUnicodeOrTabs = false;
  auto p = buffer->getBufferStart();
  const auto n = buffer->getBufferSize();
  uint64_t len = 0;
  for (size_t i = 0; i < n; ++i) {
    const auto c = *p;
    ++p;
    ++len;
    if (c == '\n') {
      // NOTE: We include the terminating '\n' in the length to ensure that
      // sum(lengths) == file size.
      lengths.push_back(len);
      len = 0;
    } else if (c == '\t' || (c & 0x80) != 0) {
      hasUnicodeOrTabs = true;
    }
  }
  if (len != 0) {
    lengths.push_back(len);
  }
  batch.fact<Src::FileLines>(file, lengths, len == 0, hasUnicodeOrTabs);
  return std::pair{file, std::move(path)};
}

void ClangDB::ppevent(
    PrePPEvent event,
    SourceRange range) {
  if (range.file) {
    range.file->events.push_back(std::move(event));
  }
}

void ClangDB::include(
    const Include& inc,
    Fact<Src::File> file,
    folly::Optional<clang::FileID> id) {
  auto full_range = inc.name;
  full_range.setBegin(inc.hash);
  const auto range = srcRange(full_range);
  const auto name_range = srcRange(inc.name);
  const auto include = fact<Pp::Include>(
    file,
    range.range,
    Src::ByteSpan{
      name_range.span.start,
      name_range.span.length
    });
  ppevent(PreInclude{include, id}, range);
}

void ClangDB::enterFile(
    clang::SourceLocation loc, folly::Optional<Include> inc) {
  auto id = sourceManager().getFileID(loc);
  if (auto r = physicalFile(id)) {
    auto& [file, path] = *r;
    file_data.push_back({id, std::move(path), file, {}, {}, {}, {}, {}});
    files.emplace(id, &file_data.back());
    if (inc && inc->entry != nullptr &&
          sourceManager().getFileEntryForID(id) == inc->entry) {
      include(inc.value(), file, id);
    }
  }
}

void ClangDB::skipFile(
    folly::Optional<Include> inc, const clang::FileEntry *entry) {
  if (inc && inc->entry != nullptr && inc->entry == entry) {
    if (auto file = fileFromEntry(*entry)) {
      include(inc.value(), file->first, folly::none);
    }
  }
}

void ClangDB::FileData::xref(
    Src::ByteSpan span,
    CrossRef::Spans CrossRef::*get_spans,
    Cxx::XRefTarget target,
    CrossRef::SortID sort_id,
    bool local) {
  auto [iter, inserted] = xrefs.lookup.try_emplace(target);
  auto& xref = iter->second;
  if (inserted) {
    auto& xrefs_ = local ? xrefs.fixed : xrefs.variable;
    xrefs_.push_back({target, std::move(sort_id), {}, {}, {}});
    xref = &xrefs_.back();
  }
  (xref->*get_spans).push_back(span);
}

void ClangDB::xref(
    clang::SourceRange r,
    Cxx::XRefTarget target,
    CrossRef::SortID sort_id,
    bool local) {
  auto file_xref = [&](SourceRange range,
                       CrossRef::Spans CrossRef::*get_spans) {
    if (range.file) {
      auto* target_range = std::get_if<SourceRange>(&sort_id);
      range.file->xref(
          range.span,
          get_spans,
          target,
          sort_id,
          local && (!target_range || target_range->file == range.file));
    }
  };
  folly::variant_match(
      fullSrcRange(r),
      [&](const SourceRange& range) {
        file_xref(range, &CrossRef::spans);
      },
      [&](const auto& range) {
        const auto& [expansion, spelling] = range;
        file_xref(expansion, &CrossRef::expansions);
        if (!spelling || !spelling->file) {
          return;
        }
        if (spelling->file == expansion.file) {
          file_xref(*spelling, &CrossRef::spellings);
        } else {
          fact<Cxx::SpellingXRef>(
              Src::FileLocation{spelling->file->fact, spelling->span}, target);
        }
      });
}

// This function is used specifically to retrieve the verbatim text of
// an attribute. The logic is currently a bit fragile, since using
// the spelling locations of begin and end doesn't necessarily produce
// the right answer.
//
//   #define CK_RENDER __attribute__((annotate("OnRender")))
//                                    ^^^^^^^^^^^^^^^^^^^^
//   CK_RENDER int foo() { return 42; }
//
// It works for an example like this since the spelling locations end up
// where we expect. However, it breaks down on more complex cases.
//
//   #define ANNOTATE annotate
//                    ^ begin
//   #define CK_RENDER __attribute__((ANNOTATE("OnRender")))
//                                                       ^ end
//
//   CK_RENDER int foo() { return 42; }
//
// In this example, the `a` in the macro definition of `ANNOTATE` is the begin,
// producing a span that goes from "begin" to "end" as marked in the code.
//
// We'll keep it for now though, since it serves most of the use cases we need.
clang::StringRef ClangDB::srcText(clang::SourceRange range) const {
  if (range.getBegin().isMacroID() && range.getEnd().isMacroID()) {
    // look for the text of a macro at the macro definition (spelling location)
    range.setBegin(sourceManager().getSpellingLoc(range.getBegin()));
    range.setEnd(sourceManager().getSpellingLoc(range.getEnd()));
  }
  return clang::Lexer::getSourceText(
      clang::CharSourceRange::getTokenRange(range),
      sourceManager(),
      compilerInstance.getLangOpts());
}

Src::Loc ClangDB::srcLoc(clang::SourceLocation loc) {
  auto range = srcRange(loc);
  return Src::Loc{
    range.range.file,
    range.range.lineBegin,
    range.range.columnBegin
  };
}

ClangDB::FullSourceRange ClangDB::fullSrcRange(clang::SourceRange range) {
  const auto& sm = sourceManager();
  auto loc = range.getBegin();
  auto expansion = immediateSrcRange(sm.getExpansionRange(range));
  if (loc.isFileID()) {
    return expansion;
  }
  std::pair<SourceRange, std::optional<SourceRange>> result = {expansion, {}};
  if (loc != range.getEnd()) {
    // If the range is not a single token, we keep the range as-is.
    //
    //   #define TYPE int
    //   #define VAR_x TYPE x
    //
    //   VAR_x;
    //
    // The range of the expanded `int x` spans multiple tokens. The spelling
    // location of the location of `i` is within the definition of `TYPE`,
    // and the spelling location of `x` is within the definition of `VAR_x`.
    //
    // The spelling range of multiple tokens, especially involving macro
    // arguments gets to be a rather complex problem. We also don't have
    // a compelling use case yet, so we punt for now.
    return result;
  }
  auto spelling = immediateSrcRange([&sm, loc] {
    if (!sm.isWrittenInScratchSpace(sm.getSpellingLoc(loc))) {
      return sm.getExpansionRange(sm.getSpellingLoc(loc));
    }
    // If spelling location is in scratch space, this is a result of a paste.
    //
    //   #define PASTE(x,y) x##y
    //   #define MACRO(x) x
    //
    //   MACRO(PASTE(foo,bar))
    //         ^^^^^^^^^^^^^^
    //
    // We walk up the parent context since there can be several levels of
    // pasting.
    auto l = loc;
    do {
      l = sm.getImmediateMacroCallerLoc(l);
    } while (sm.isWrittenInScratchSpace(sm.getSpellingLoc(l)));
    return sm.getExpansionRange(l);
  }());
  if (expansion.file != spelling.file || expansion.span != spelling.span) {
    result.second = spelling;
  }
  return result;
}

ClangDB::SourceRange ClangDB::immediateSrcRange(
    clang::CharSourceRange range) {
  const auto [file_id, begin_offset] =
    sourceManager().getDecomposedLoc(range.getBegin());

  auto end_loc =
    range.isTokenRange()
        // In token ranges, getEnd points to the first character of the last
        // token so skip it.
      ? clang::Lexer::getLocForEndOfToken(
          range.getEnd(),
          0,
          sourceManager(),
          compilerInstance.getLangOpts())

        // In char ranges, it already points past the end of the range.
      : range.getEnd();

  // TODO: What should we do if it's invalid?
  if (!end_loc.isValid()) {
    end_loc = range.getBegin();
  }

  unsigned end_offset;
  if (!sourceManager().isInFileID(end_loc, file_id, &end_offset)) {
    // FIXME: what *should* we do if the end of the range is in a different
    // file?
    end_offset = begin_offset;
  }
  assert(end_offset >= begin_offset);

  const auto data = folly::get_default(files, file_id, nullptr);
  const unsigned last_char_offset =
    end_offset > begin_offset ? end_offset - 1 : begin_offset;
  return SourceRange{
    data,
    Src::ByteSpan{begin_offset, end_offset - begin_offset},
    Src::Range {
      data ? data->fact : file(file_id),
      // FIXME: This is quite expensive and not always used. We should do this
      // on demand.
      sourceManager().getLineNumber(file_id, begin_offset),
      sourceManager().getColumnNumber(file_id, begin_offset),
      sourceManager().getLineNumber(file_id, last_char_offset),
      sourceManager().getColumnNumber(file_id, last_char_offset)
    }
  };
}

namespace {

Src::PackedByteSpans packByteSpans(std::vector<Src::ByteSpan>&& v) {
  auto spans = std::move(v);
  size_t offset = 0;
  for (auto& span : spans) {
    assert(span.start >= offset);
    auto start = span.start;
    span.start -= offset;
    offset = start;
  }
  Src::PackedByteSpans result;
  for (const auto& span : spans) {
    if (result.empty() || span.length != result.back().length) {
      result.push_back({span.length, {}});
    }
    result.back().offsets.push_back(span.start);
  }
  return result;
}

std::vector<Cxx::FixedXRef> finishRefs(std::deque<ClangDB::CrossRef>&& v) {
  auto xrefs = std::move(v);
  for (auto& xref : xrefs) {
    for (auto* spans : {&xref.spans, &xref.expansions, &xref.spellings}) {
      std::sort(spans->begin(), spans->end());
      spans->erase(std::unique(spans->begin(), spans->end()), spans->end());
    }
  }
  std::stable_sort(
      xrefs.begin(),
      xrefs.end(),
      [&](const ClangDB::CrossRef& x, const ClangDB::CrossRef& y) {
        auto x_uses = std::tie(x.spans, x.expansions, x.spellings);
        auto y_uses = std::tie(y.spans, y.expansions, y.spellings);
        if (x_uses != y_uses) {
          return x_uses < y_uses;
        }
        if (x.sort_id.index() != y.sort_id.index()) {
          return x.sort_id.index() < y.sort_id.index();
        }
        return folly::variant_match(
            x.sort_id,
            [&y_id = y.sort_id](const ClangDB::SourceRange& x) {
              const auto& y = std::get<ClangDB::SourceRange>(y_id);
              if (bool(x.file) != bool(y.file)) {
                return bool(x.file) < bool(y.file);
              }
              return x.file && y.file && x.file->path != y.file->path
                  ? x.file->path < y.file->path
                  : x.span < y.span;
            },
            [&y_id = y.sort_id](const std::vector<std::string>& x) {
              return x < std::get<std::vector<std::string>>(y_id);
            });
      });
  std::vector<Cxx::FixedXRef> fixed;
  fixed.reserve(xrefs.size());
  for (auto&& xref : std::move(xrefs)) {
    fixed.push_back(
        {std::move(xref.target),
         {packByteSpans(std::move(xref.spans)),
          packByteSpans(std::move(xref.expansions)),
          packByteSpans(std::move(xref.spellings))}});
  }
  return fixed;
}
}

void ClangDB::finish() {
  auto release = [](auto& x) { auto tmp = std::move(x); };

  const auto main_id = sourceManager().getMainFileID();
  auto tunit = fact<Buck::TranslationUnit>(
    file(main_id),
    locator,
    maybe(platform)
  );

  std::vector<Fact<Cxx::FileXRefs>> tunitXRefs;

  for (auto& file : folly::range(file_data.rbegin(), file_data.rend())) {
    auto& xrefs = file.xrefs;
    if (!xrefs.lookup.empty()) {
      auto fixed = finishRefs(std::move(xrefs.fixed));
      auto external_refs = finishRefs(std::move(xrefs.variable));

      struct VariableXRef {
        Cxx::From from;
        std::vector<Cxx::XRefTarget> group;
      };
      std::vector<VariableXRef> variable;
      for (auto&& [target, from] : std::move(external_refs)) {
        if (variable.empty() || from != variable.back().from) {
          variable.push_back({std::move(from), {}});
        }
        variable.back().group.push_back(std::move(target));
      }
      release(external_refs);

      std::vector<Cxx::From> froms;
      std::vector<Fact<Cxx::XRefTargets>> targets;
      froms.reserve(variable.size());
      targets.reserve(variable.size());
      for (auto&& [from, group] : std::move(variable)) {
        froms.push_back(std::move(from));
        targets.push_back(fact<Cxx::XRefTargets>(std::move(group)));
      }
      release(variable);

      auto xmap = fact<Cxx::FileXRefMap>(
        file.fact,
        std::move(fixed),
        std::move(froms));
      auto fileXRefs = fact<Cxx::FileXRefs>(
        xmap,
        std::move(targets));
      tunitXRefs.push_back(fileXRefs);
    }
    release(xrefs);

    auto& decls = file.declarations;
    // We call `stable_sort` here only the `Src::ByteSpan` portion such that the
    // order of `Cxx::Declaration` among ties are maintained. Since declarations
    // are added in lexical order, this should produce a deterministic ordering.
    std::stable_sort(
        decls.begin(), decls.end(), [](const auto& x, const auto& y) {
          return x.first < y.first;
        });
    auto decl_trace = fact<Cxx::Declarations>(
      folly::gen::from(decls)
        | folly::gen::mapped([](const auto& x) { return x.second; })
        | folly::gen::as<std::vector>());
    release(decls);

    auto resolve = [&](const auto& x) {
      return folly::variant_match(
          x,
          [](const Cxx::PPEvent& x) { return x; },
          [](const PreInclude& x) {
            return Cxx::PPEvent::include_(
                Cxx::IncludeTrace{x.include, nothing()});
          });
    };
    auto pp_trace = fact<Cxx::PPTrace>(
      file.fact,
      folly::gen::from(file.events)
        | folly::gen::mapped(resolve)
        | folly::gen::as<std::vector>());
    file.trace = fact<Cxx::Trace>(file.fact, decl_trace, pp_trace);
    file.include_tree = fact<Cxx::IncludeTree>(file.trace.value(), [&] {
      std::vector<Cxx::MaybeIncludeTree> trees;
      for (const auto& event : file.events) {
        if (auto pre = std::get_if<PreInclude>(&event)) {
          Cxx::MaybeIncludeTree tree{nothing()};
          if (pre->file) {
            tree = {maybe(files.at(pre->file.value())->include_tree)};
          }
          trees.push_back(tree);
        }
      }
      return trees;
    }());
    release(file.events);
  }

  fact<Cxx::TranslationUnitXRefs>(tunit, std::move(tunitXRefs));

  if (auto p = folly::get_default(files, main_id, nullptr)) {
    if (p->trace) {
      fact<Cxx::TranslationUnitTrace>(tunit, p->trace.value());
    } else {
      LOG(WARNING) << "translation unit has no trace";
    }
    if (p->include_tree) {
      fact<Cxx::TranslationUnitIncludeTree>(tunit, p->include_tree.value());
    } else {
      LOG(WARNING) << "translation unit has no include tree";
    }
  } else {
    LOG(WARNING) << "translation unit has no file data";
  }
}


}
}
}
