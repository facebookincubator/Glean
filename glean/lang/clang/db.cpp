/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

#include "glean/lang/clang/db.h"

#include <folly/Overload.h>

#include "glean/lang/clang/path.h"

namespace facebook {
namespace glean {
namespace clangx {

namespace {

std::filesystem::path subpath(
    const folly::Optional<std::string>& subdir,
    clang::StringRef path) {
  // This returns 'path' if it is absolute or if 'subdir' is empty and
  // 'subdir'/'path' otherwise.
  std::filesystem::path p(static_cast<std::string>(path));
  return p.is_absolute() || !subdir
    ? p
    : std::filesystem::path(subdir.value()) / p;
}

}

Fact<Src::File> ClangDB::fileFromEntry(
    const clang::FileEntry& entry) {
  // Clang files have a Name and *maybe* a RealPathName (which seems to be
  // Name with symlinks resolved). For fbcode sources, RealPathName tends to
  // be what we want for sources (RealPathName could be "folly/File.h" and
  // Name would be the symlink under "buck-out"). For other things, we tend
  // to want Name (e.g., "third-party-buck/.../basic_ios.h" rather than
  // "/mnt/gvfs/.../basic_ios.h").
  //
  // TODO: Do we want to resolve symlinks ourselves instead of using
  // tryGetRealPathName?
  auto path = goodPath(root, subpath(subdir, entry.getName()));
  auto real = entry.tryGetRealPathName();
  if (!real.empty()) {
    path = betterPath(goodPath(root, subpath(subdir, real)), path);
  }
  if (path_prefix.has_value()) {
     path = std::filesystem::path(path_prefix.value()) / path;
  }
  const auto file = batch.fact<Src::File>(path.native());

  // define FileLines
  #if LLVM_VERSION_MAJOR >= 12
  auto bufferOpt = sourceManager().getMemoryBufferForFileOrNone(&entry);
  if (bufferOpt.hasValue()) {
    auto buffer = &(bufferOpt.getValue());
  #else
  bool invalid = false;
  auto buffer = sourceManager().getMemoryBufferForFile(&entry, &invalid);
  if (buffer != nullptr && !invalid) {
  #endif
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
      } else if (c == '\t' || (c&0x80) != 0) {
        hasUnicodeOrTabs = true;
      }
    }
    if (len != 0) {
      lengths.push_back(len);
    }
    batch.fact<Src::FileLines>(file, lengths, len==0, hasUnicodeOrTabs);
  } else {
    LOG(WARNING) << "couldn't get MemoryBuffer for " << path.native();
  }

  return file;
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
    Src::ByteRange{
      name_range.span.start,
      name_range.span.start + name_range.span.length},
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
    file_data.push_back(FileData{id, r.value(), {}, {}, {}, folly::none});
    files.insert({id, &file_data.back()});
    if (inc && inc->entry != nullptr &&
          sourceManager().getFileEntryForID(id) == inc->entry) {
      include(inc.value(), r.value(), id);
    }
  }
}

void ClangDB::skipFile(
    folly::Optional<Include> inc, const clang::FileEntry *entry) {
  if (inc && inc->entry != nullptr && inc->entry == entry) {
    include(inc.value(), fileFromEntry(*entry), folly::none);
  }
}

void ClangDB::xref(
    clang::SourceRange r,
    folly::Optional<clang::SourceLocation> loc,
    Cxx::XRefTarget target) {
  auto range = srcRange(r);
  if (range.file) {
    range.file->xrefs.push_back(CrossRef{
      range.span,
      loc && srcRange(loc.value()).file == range.file,
      target});
  }
}

clang::SourceRange ClangDB::rangeOfToken(clang::SourceRange range) const {
  auto start = range.getBegin();
  if (!start.isMacroID()) {
    auto end = clang::Lexer::getLocForEndOfToken(
      start,
      1,
      sourceManager(),
      compilerInstance.getLangOpts());
    return clang::SourceRange(start, end);
  } else {
    return range;
  }
}

clang::SourceRange ClangDB::spellingRange(clang::SourceRange range) const {
  return clang::SourceRange(
    sourceManager().getSpellingLoc(range.getBegin()),
    sourceManager().getSpellingLoc(range.getEnd())
  );
}

clang::StringRef ClangDB::srcText(clang::SourceRange range) const {
  if (range.getBegin().isMacroID()) {
    // look for the text of a macro at the macro definition (spelling location)
    range = spellingRange(range);
  }
  auto token_range = clang::CharSourceRange::getTokenRange(range);
  return clang::Lexer::getSourceText(
    token_range,
    sourceManager(),
    compilerInstance.getLangOpts()
  );
}

Src::Loc ClangDB::srcLoc(clang::SourceLocation loc) {
  auto range = srcRange(loc);
  return Src::Loc{
    range.range.file,
    range.range.lineBegin,
    range.range.columnBegin
  };
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

Src::ByteSpans byteSpans(std::vector<Src::ByteSpan> v) {
  std::sort(v.begin(), v.end());
  v.erase(std::unique(v.begin(), v.end()), v.end());
  std::vector<Src::RelByteSpan> spans;
  spans.reserve(v.size());
  size_t offset = 0;
  for (const auto& span : v) {
    assert(span.start >= offset);
    spans.push_back(Src::RelByteSpan{span.start - offset, span.length});
    offset = span.start;
  }
  return spans;
}

using RefMap = std::map<Cxx::XRefTarget, std::vector<Src::ByteSpan>>;

std::vector<Cxx::FixedXRef> finishRefs(RefMap&& map) {
  std::vector<Cxx::FixedXRef> refs;
  refs.reserve(map.size());
  for (auto& x : map) {
    refs.push_back(Cxx::FixedXRef{
      x.first,
      byteSpans(std::move(x.second))});
  }
  map.clear();
  std::sort(refs.begin(), refs.end(),
    [](const auto& x, const auto& y) { return x.ranges < y.ranges; });
  return refs;
}


}

void ClangDB::finish() {
  auto release = [](auto& vec) {
    typename std::decay<decltype(vec)>::type tmp;
    tmp.swap(vec);
  };

  const auto main_id = sourceManager().getMainFileID();
  auto tunit = fact<Buck::TranslationUnit>(
    file(main_id),
    locator,
    maybe(platform)
  );

  std::vector<Fact<Cxx::FileXRefs>> tunitXRefs;

  for (auto& file : folly::range(file_data.rbegin(), file_data.rend())) {
    auto& xrefs = file.xrefs;
    if (!xrefs.empty()) {
      RefMap locals;
      RefMap externals;

      for (const auto& xref : xrefs) {
        if (xref.local) {
          locals[xref.target].push_back(xref.span);
        } else {
          externals[xref.target].push_back(xref.span);
        }
      }

      auto local_refs = finishRefs(std::move(locals));
      auto external_refs = finishRefs(std::move(externals));

      std::vector<Cxx::XRefTarget> external_targets;
      std::vector<Src::ByteSpans> external_spans;
      external_targets.reserve(external_refs.size());
      external_spans.reserve(external_refs.size());
      for (auto& ext : external_refs) {
        external_targets.push_back(ext.target);
        external_spans.push_back(std::move(ext.ranges));
      }

      auto xmap = fact<Cxx::FileXRefMap>(
        file.fact,
        std::move(local_refs),
        std::move(external_spans));
      auto fileXRefs = fact<Cxx::FileXRefs>(
        xmap,
        std::move(external_targets));
      tunitXRefs.push_back(fileXRefs);
    }
    release(xrefs);

    auto& decls = file.declarations;
    std::sort(decls.begin(), decls.end());
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
