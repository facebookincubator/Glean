/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::path::Component;
use std::path::Path;

// Use ahash instead of std hashmap for a slight performance gain.
use ahash::AHashMap as HashMap;
use anyhow::Result;
#[cfg(feature = "facebook")]
use proto_rust::scip::Document;
#[cfg(feature = "facebook")]
use proto_rust::scip::Metadata;
#[cfg(feature = "facebook")]
use proto_rust::scip::Occurrence;
#[cfg(feature = "facebook")]
use proto_rust::scip::SymbolInformation;
#[cfg(feature = "facebook")]
use proto_rust::scip::symbol_information;
use scip_symbol::Descriptor;
use scip_symbol::DescriptorKind;
use scip_symbol::ScipSymbol;
use scip_symbol::parse_scip_symbol;
use serde::Serialize;

use crate::GleanRange;
use crate::ToolInfo;
use crate::decode_scip_range;
use crate::lsif::LanguageId;
use crate::lsif::SymbolKind;
use crate::output::GleanJSONOutput;
#[cfg(not(feature = "facebook"))]
use crate::proto::scip::Document;
#[cfg(not(feature = "facebook"))]
use crate::proto::scip::Metadata;
#[cfg(not(feature = "facebook"))]
use crate::proto::scip::Occurrence;
#[cfg(not(feature = "facebook"))]
use crate::proto::scip::SymbolInformation;
#[cfg(not(feature = "facebook"))]
use crate::proto::scip::symbol_information;

// Key used to distinguish different fact hashmaps in Env.
// Would probably be more efficient to just use an array of hashmaps,
// but stick to the same design as the Haskell version for simplicity.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum StringPredicate {
    Symbol,
    LocalName,
    File,
    DisplayName,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Serialize)]
#[serde(transparent)]
pub struct ScipId(u64);
impl fmt::Display for ScipId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

pub struct Env {
    unique: u64,
    fact_id: HashMap<StringPredicate, HashMap<Box<str>, ScipId>>,
    out: GleanJSONOutput,
    /// Qualified-symbol → `SymbolKind` overrides sourced from
    /// `SymbolInformation.kind` across every document in the SCIP index.
    /// Populated up front by `register_kind_overrides_for_doc` so that
    /// occurrences in document B can see overrides for symbols defined in
    /// document A — without this, references to e.g. an `impl`-side `const`
    /// fall back to the descriptor-derived kind and the same symbol ends up
    /// with contradictory `scip.SymbolKind` facts.
    kind_overrides: HashMap<Box<str>, SymbolKind>,
    go_line_directive_maps: HashMap<ScipId, GoLineDirectiveMap>,
}

/// Normalize a filepath by removing .. and . components
/// Returns None if the path cannot be properly normalized (e.g., too many .. components)
fn normalize_filepath(path: &str) -> Option<String> {
    let path = Path::new(path);
    let max_len = path.as_os_str().len();
    let mut ret = std::path::PathBuf::with_capacity(max_len);

    for component in path.components() {
        match component {
            Component::Normal(_) | Component::RootDir => ret.push(component),
            Component::ParentDir => {
                // Go up a directory by popping the last component
                if let Some(Component::Normal(_)) = ret.components().next_back() {
                    ret.pop();
                } else {
                    // Cannot resolve .. properly, return None
                    return None;
                }
            }
            Component::CurDir => {} // Skip "."
            Component::Prefix(prefix) => ret.push(prefix.as_os_str()),
        }
    }

    Some(ret.to_string_lossy().to_string())
}

/// Compute src.FileLines data from raw file bytes.
/// Returns (lengths, ends_in_newline, has_unicode_or_tabs) matching the
/// src.FileLines Glean schema. Each entry in `lengths` is the byte length
/// of a line including its terminating newline (if any).
fn compute_file_lines(bytes: &[u8]) -> (Vec<u64>, bool, bool) {
    let mut lengths = Vec::new();
    let mut has_unicode_or_tabs = false;
    let mut line_len: u64 = 0;

    for &b in bytes {
        line_len += 1;
        if b == b'\n' {
            lengths.push(line_len);
            line_len = 0;
        } else if b == b'\t' || (b & 0x80) != 0 {
            has_unicode_or_tabs = true;
        }
    }

    let ends_in_newline = line_len == 0 && !bytes.is_empty();
    if line_len > 0 {
        lengths.push(line_len);
    }

    (lengths, ends_in_newline, has_unicode_or_tabs)
}

#[derive(Clone)]
struct GoLineDirectiveMap {
    source: Vec<u8>,
    line_starts: Vec<usize>,
    segments: Vec<GoLineDirectiveSegment>,
}

#[derive(Clone, Copy)]
struct GoLineDirectiveSegment {
    physical_start_line: u64,
    physical_end_line: u64,
    virtual_start_line: u64,
}

impl GoLineDirectiveMap {
    fn from_source(bytes: &[u8]) -> Option<Self> {
        let line_starts = Self::compute_line_starts(bytes);
        let line_count = line_starts.len().saturating_sub(1) as u64;
        let mut segments = Vec::new();
        let mut open_segment = None;

        for physical_line in 1..=line_count {
            let line = Self::line_text_from(bytes, &line_starts, physical_line);
            if let Some(virtual_start_line) = parse_go_line_directive(line) {
                let prior_segment = open_segment.take();
                if let Some((physical_start_line, prior_virtual_start_line)) = prior_segment {
                    if physical_start_line < physical_line {
                        segments.push(GoLineDirectiveSegment {
                            physical_start_line,
                            physical_end_line: physical_line,
                            virtual_start_line: prior_virtual_start_line,
                        });
                    }
                }
                open_segment = Some((physical_line + 1, virtual_start_line));
            }
        }

        if let Some((physical_start_line, virtual_start_line)) = open_segment {
            if physical_start_line <= line_count {
                segments.push(GoLineDirectiveSegment {
                    physical_start_line,
                    physical_end_line: line_count + 1,
                    virtual_start_line,
                });
            }
        }

        if segments.is_empty() {
            return None;
        }

        Some(Self {
            source: bytes.to_vec(),
            line_starts,
            segments,
        })
    }

    fn compute_line_starts(bytes: &[u8]) -> Vec<usize> {
        if bytes.is_empty() {
            return vec![0];
        }

        let mut line_starts = vec![0];
        line_starts.extend(bytes.iter().enumerate().filter_map(|(idx, byte)| {
            if *byte == b'\n' && idx + 1 < bytes.len() {
                Some(idx + 1)
            } else {
                None
            }
        }));
        line_starts.push(bytes.len());
        line_starts
    }

    fn remap_range(&self, range: GleanRange, symbol_hint: Option<&str>) -> GleanRange {
        let Some(segment) = self.choose_segment(range.line_begin, symbol_hint) else {
            return range;
        };
        let Some(line_begin) = segment.physical_line_for(range.line_begin) else {
            return range;
        };
        let Some(line_end) = segment.physical_line_for(range.line_end) else {
            return range;
        };

        GleanRange {
            line_begin,
            line_end,
            ..range
        }
    }

    fn choose_segment(
        &self,
        virtual_line: u64,
        symbol_hint: Option<&str>,
    ) -> Option<GoLineDirectiveSegment> {
        let candidates: Vec<_> = self
            .segments
            .iter()
            .copied()
            .filter(|segment| segment.contains_virtual_line(virtual_line))
            .collect();

        match candidates.as_slice() {
            [] => None,
            [segment] => Some(*segment),
            _ => symbol_hint.and_then(|hint| {
                candidates.into_iter().find(|segment| {
                    segment
                        .physical_line_for(virtual_line)
                        .is_some_and(|line| self.physical_line_contains(line, hint))
                })
            }),
        }
    }

    fn physical_line_contains(&self, physical_line: u64, needle: &str) -> bool {
        if needle.is_empty() {
            return false;
        }

        std::str::from_utf8(Self::line_text_from(
            &self.source,
            &self.line_starts,
            physical_line,
        ))
        .is_ok_and(|line| line.contains(needle))
    }

    fn line_text_from<'a>(bytes: &'a [u8], line_starts: &[usize], line: u64) -> &'a [u8] {
        if line == 0 {
            return &[];
        }

        let line_idx = (line - 1) as usize;
        if line_idx + 1 >= line_starts.len() {
            return &[];
        }

        let start = line_starts[line_idx];
        let mut end = line_starts[line_idx + 1];
        while end > start && matches!(bytes[end - 1], b'\n' | b'\r') {
            end -= 1;
        }
        &bytes[start..end]
    }
}

impl GoLineDirectiveSegment {
    fn contains_virtual_line(&self, virtual_line: u64) -> bool {
        virtual_line >= self.virtual_start_line
            && virtual_line < self.virtual_start_line + self.physical_line_count()
    }

    fn physical_line_for(&self, virtual_line: u64) -> Option<u64> {
        if !self.contains_virtual_line(virtual_line) {
            return None;
        }
        Some(self.physical_start_line + virtual_line - self.virtual_start_line)
    }

    fn physical_line_count(&self) -> u64 {
        self.physical_end_line - self.physical_start_line
    }
}

fn parse_go_line_directive(line: &[u8]) -> Option<u64> {
    let line = trim_ascii(line);
    let directive = if let Some(rest) = line.strip_prefix(b"//line ") {
        rest
    } else if let Some(rest) = line.strip_prefix(b"/*line ") {
        trim_ascii(rest).strip_suffix(b"*/")?
    } else {
        return None;
    };

    let directive = std::str::from_utf8(trim_ascii(directive)).ok()?;
    let mut parts = directive.rsplit(':');
    let last = parts.next()?;
    let before_last = parts.next()?;
    let line_number = if last.parse::<u64>().is_ok() && before_last.parse::<u64>().is_ok() {
        before_last
    } else {
        last
    };

    let line_number = line_number.parse::<u64>().ok()?;
    (line_number > 0).then_some(line_number)
}

fn trim_ascii(bytes: &[u8]) -> &[u8] {
    let mut start = 0;
    let mut end = bytes.len();

    while start < end && matches!(bytes[start], b' ' | b'\t' | b'\n' | b'\r') {
        start += 1;
    }
    while end > start && matches!(bytes[end - 1], b' ' | b'\t' | b'\n' | b'\r') {
        end -= 1;
    }

    &bytes[start..end]
}

impl Env {
    pub fn new() -> Self {
        Self {
            unique: 1,
            fact_id: HashMap::new(),
            out: GleanJSONOutput::default(),
            kind_overrides: HashMap::new(),
            go_line_directive_maps: HashMap::new(),
        }
    }

    pub fn output(self) -> GleanJSONOutput {
        self.out
    }

    fn next_id(&mut self) -> ScipId {
        let id = ScipId(self.unique);
        self.unique += 1;
        id
    }

    fn set_def_fact(&mut self, kind: StringPredicate, key: Box<str>, val: ScipId) {
        self.fact_id.entry(kind).or_default().insert(key, val);
    }

    fn get_def_fact_id(&mut self, kind: StringPredicate, key: &str) -> Option<ScipId> {
        self.fact_id
            .get(&kind)
            .and_then(|map| map.get(key))
            .copied()
    }

    fn get_or_set_fact(&mut self, kind: StringPredicate, key: Box<str>) -> (ScipId, bool) {
        match self.get_def_fact_id(kind, &key) {
            Some(id) => (id, true),
            None => {
                let id = self.next_id();
                self.set_def_fact(kind, key, id);
                (id, false)
            }
        }
    }

    pub fn decode_scip_metadata(&mut self, metadata: Metadata) {
        let tool_info = metadata.tool_info.0.map(|tool_info| ToolInfo {
            tool_name: tool_info.name,
            tool_args: tool_info.arguments,
            version: tool_info.version,
        });

        let version = match metadata.version.value() {
            1 => {
                tracing::warn!(
                    "metadata.version:1 is not supported, using version:0. This is a known issue for scip-php"
                );
                0 // scip-php emits version:1, which isn't a supported ProtocolVersion; treat it as 0
            }
            v => v,
        };
        self.out
            .metadata(version, metadata.text_document_encoding.value(), tool_info);
    }

    /// Determine a document's language: prefer the explicit `doc.language`,
    /// then optionally infer from the file extension, then fall back to the
    /// caller-supplied default. The filepath used for extension matching is
    /// `doc.relative_path` — path-prefix adjustments performed elsewhere do
    /// not affect extensions, so they are not relevant here.
    fn infer_lang_for_doc(
        &self,
        default_lang: Option<LanguageId>,
        infer_language: bool,
        doc: &Document,
    ) -> LanguageId {
        LanguageId::new(&doc.language)
            .known()
            .or_else(|| {
                if infer_language {
                    self.file_language_of(&doc.relative_path)
                } else {
                    None
                }
            })
            .or(default_lang)
            .unwrap_or_default()
    }

    /// Compute the qualified filepath for a document: apply `strip_prefix` /
    /// `path_prefix` adjustments, then normalize `..` components for
    /// TypeScript paths. Returns `None` when a TypeScript path cannot be
    /// normalized (e.g., too many `..` components) so the caller can skip
    /// the document with the same semantics as before extraction.
    fn qualified_filepath_for_doc(
        lang: LanguageId,
        path_prefix: Option<&str>,
        strip_prefix: Option<&str>,
        doc: &Document,
    ) -> Option<Box<str>> {
        let mut filepath = doc.relative_path.to_owned();
        if let Some(strip_prefix) = strip_prefix {
            filepath = filepath
                .strip_prefix(strip_prefix)
                .unwrap_or(&filepath)
                .to_owned();
        }
        if let Some(path_prefix) = path_prefix {
            filepath = format!("{}{}", path_prefix, filepath);
        }

        // Normalize paths for TypeScript files only
        // TODO T240234639: Remove once SCIP stops returning paths with ../
        if matches!(lang, LanguageId::TypeScript | LanguageId::TypeScriptReact) {
            match normalize_filepath(&filepath) {
                Some(normalized) => filepath = normalized,
                // Cannot normalize path properly (e.g., too many .. components);
                // signal to the caller to skip this document.
                None => return None,
            }
        }

        Some(filepath.into_boxed_str())
    }

    /// Pre-pass: register the `SymbolInformation.kind` overrides for a single
    /// document into `self.kind_overrides`. Must be called for every document
    /// in the SCIP index *before* any call to `decode_scip_doc`, so that
    /// occurrences in any document can see overrides for symbols defined in
    /// any other document. See the field doc on `Env::kind_overrides`.
    pub fn register_kind_overrides_for_doc(
        &mut self,
        default_lang: Option<LanguageId>,
        infer_language: bool,
        path_prefix: Option<&str>,
        strip_prefix: Option<&str>,
        doc: &Document,
    ) {
        let lang = self.infer_lang_for_doc(default_lang, infer_language, doc);
        let Some(filepath) = Self::qualified_filepath_for_doc(lang, path_prefix, strip_prefix, doc)
        else {
            return;
        };
        for info in &doc.symbols {
            if info.symbol.is_empty() {
                continue;
            }
            let Ok(scip_kind) = info.kind.enum_value() else {
                continue;
            };
            if let Some(kind) = scip_kind_to_symbol_kind(scip_kind) {
                let key = qualify_scip_symbol(&info.symbol, &filepath);
                self.kind_overrides.insert(key, kind);
            }
        }
    }

    pub fn decode_scip_doc(
        &mut self,
        default_lang: Option<LanguageId>,
        infer_language: bool,
        path_prefix: Option<&str>,
        strip_prefix: Option<&str>,
        source_root: Option<&Path>,
        mut doc: Document,
    ) -> Result<()> {
        let lang = self.infer_lang_for_doc(default_lang, infer_language, &doc);
        let Some(filepath) =
            Self::qualified_filepath_for_doc(lang, path_prefix, strip_prefix, &doc)
        else {
            // Cannot normalize path properly (e.g., too many .. components)
            // Log error and skip this document
            tracing::warn!(
                "Cannot normalize filepath '{}', will most likely break browser clients",
                doc.relative_path,
            );
            return Ok(());
        };

        // SCIP allows multiple Documents to share the same `relative_path`:
        // the Index proto comment in third-party/scip/scip.proto says
        // "Complementary information can be merged together from multiple
        // sources to provide a unified code intelligence experience". This is
        // load-bearing for indexers that split a single file's data across
        // multiple Documents to stay under protobuf's 2 GB serialization limit
        // -- in particular the Swift SCIP exporter
        // (writeScipIndexChunks in fbcode/swift_devx/glean-indexer/scip/SCIPExporter.cpp)
        // emits multiple Documents for the same file when its accumulated
        // occurrences/symbols approach the limit.
        //
        // The previous behavior dropped every Document past the first for a
        // given path, which silently truncated the resulting Glean facts.
        // Now we emit the per-file facts (src.File, src.FileLines, file_lang)
        // only on first sight but always extend the per-file occurrences and
        // symbols.
        let (src_file_id, already_seen) =
            self.get_or_set_fact(StringPredicate::File, filepath.clone());
        let doc_text = std::mem::take(&mut doc.text);
        let file_bytes: Option<Vec<u8>> = if !doc_text.is_empty() {
            Some(doc_text.into_bytes())
        } else if let Some(source_root) = source_root {
            std::fs::read(source_root.join(&doc.relative_path)).ok()
        } else {
            None
        };
        if !already_seen {
            self.out.src_file(src_file_id, filepath.clone());

            // Emit src.FileLines: prefer inline document text, fall back to
            // disk read. FileLines is per-file metadata; emit it once.
            if let Some(bytes) = file_bytes.as_deref() {
                let (lengths, ends_in_newline, has_unicode_or_tabs) = compute_file_lines(bytes);
                self.out
                    .file_lines(src_file_id, lengths, ends_in_newline, has_unicode_or_tabs);
            }

            // file_lang is also per-file metadata; emit it once.
            let lang_file_id = self.next_id();
            self.out.file_lang(lang_file_id, src_file_id, lang);
        }
        let has_no_go_line_directive_map = !self.go_line_directive_maps.contains_key(&src_file_id);
        let needs_go_line_directive_map =
            matches!(lang, LanguageId::Go) && has_no_go_line_directive_map;
        if needs_go_line_directive_map {
            if let Some(bytes) = file_bytes.as_deref() {
                if let Some(map) = GoLineDirectiveMap::from_source(bytes) {
                    self.go_line_directive_maps.insert(src_file_id, map);
                }
            }
        }

        // Occurrences and SymbolInformation are additive across same-path
        // Documents; always process them.
        let mut empty_occ_count = 0;
        for occ in doc.occurrences {
            if occ.symbol.is_empty() {
                // scip-go emits empty occurrences, skip them.
                empty_occ_count += 1;
                continue;
            }
            self.decode_scip_occurrence(src_file_id, &filepath, occ)?;
        }
        if empty_occ_count > 0 {
            tracing::warn!(
                "{} scip.Occurrence skipped in file {}, due to symbol being empty",
                empty_occ_count,
                filepath,
            );
        }

        for info in doc.symbols {
            self.decode_scip_info(&filepath, info)?;
        }

        Ok(())
    }

    fn file_language_of(&self, filepath: &str) -> Option<LanguageId> {
        if filepath.ends_with(".kt") {
            return Some(LanguageId::Kotlin);
        } else if filepath.ends_with(".java") {
            return Some(LanguageId::Java);
        } else if filepath.ends_with(".ts") {
            return Some(LanguageId::TypeScript);
        } else if filepath.ends_with(".tsx") {
            return Some(LanguageId::TypeScriptReact);
        } else if filepath.ends_with(".js") {
            return Some(LanguageId::JavaScript);
        } else if filepath.ends_with(".jsx") {
            return Some(LanguageId::JavaScriptReact);
        } else if filepath.ends_with(".rs") {
            return Some(LanguageId::Rust);
        } else if filepath.ends_with(".go") {
            return Some(LanguageId::Go);
        } else if filepath.ends_with(".py") {
            return Some(LanguageId::Python);
        } else if filepath.ends_with(".cpp") || filepath.ends_with(".h") {
            return Some(LanguageId::Cpp);
        } else if filepath.ends_with(".c") {
            return Some(LanguageId::C);
        } else if filepath.ends_with(".cs") {
            return Some(LanguageId::CSharp);
        } else if filepath.ends_with(".m") {
            return Some(LanguageId::ObjectiveC);
        } else if filepath.ends_with(".mm") {
            return Some(LanguageId::ObjectiveCpp);
        } else if filepath.ends_with(".php") {
            return Some(LanguageId::PHP);
        } else if filepath.ends_with(".rb") {
            return Some(LanguageId::Ruby);
        } else if filepath.ends_with(".dart") {
            return Some(LanguageId::Dart);
        }
        None
    }

    /// Process an external symbol from `Index.external_symbols`.
    ///
    /// Ensures the symbol fact exists (unlike `decode_scip_info` which only
    /// attaches metadata to symbols already seen via occurrences), then
    /// delegates to `decode_scip_info` for documentation, display names,
    /// and relationships.
    pub fn decode_external_symbol(&mut self, info: SymbolInformation) -> Result<()> {
        if info.symbol.is_empty() {
            return Ok(());
        }

        let scip_symbol = parse_scip_symbol(&info.symbol);

        // External symbols are always global; skip locals.
        let descriptors = match scip_symbol {
            ScipSymbol::Global { descriptors, .. } => descriptors,
            ScipSymbol::Local { .. } => return Ok(()),
        };

        // Create the symbol fact if it doesn't already exist from an occurrence.
        let scip_symbol_str = info.symbol.clone().into_boxed_str();
        let (symbol_id, seen_symbol) =
            self.get_or_set_fact(StringPredicate::Symbol, scip_symbol_str.clone());
        if !seen_symbol {
            self.out.symbol(symbol_id, scip_symbol_str);

            let local_name = descriptors
                .last()
                .map(|d| d.name.to_owned().into_boxed_str())
                .unwrap_or_else(|| "".to_owned().into_boxed_str());
            let (name_id, seen_name) =
                self.get_or_set_fact(StringPredicate::LocalName, local_name.clone());
            if !seen_name {
                self.out.local_name(name_id, local_name);
            }
            self.out.symbol_name(symbol_id, name_id);

            // Prefer SymbolInformation.kind if it carries a specific value;
            // fall back to the descriptor-derived kind.
            let kind_from_info = info
                .kind
                .enum_value()
                .ok()
                .and_then(scip_kind_to_symbol_kind);
            let kind = kind_from_info
                .or_else(|| descriptors.last().map(|d| SymbolKind::new(d.kind.clone())));
            if let Some(kind) = kind {
                if kind != SymbolKind::SkUnknown {
                    self.out.symbol_kind(symbol_id, kind);
                }
            }
        }

        // Reuse decode_scip_info for metadata (docs, display name, relationships).
        // External symbols are always global, so filepath is not used for lookup.
        self.decode_scip_info("", info)
    }

    fn decode_scip_info(&mut self, filepath: &str, info: SymbolInformation) -> Result<()> {
        let sym_id = self.get_symbol_id(&info.symbol, filepath);

        for document in info.documentation {
            let doc_id = self.next_id();
            let doc_text: String = document.trim().to_string();
            self.out.documentation(doc_id, doc_text.into_boxed_str());
            if let Some(sym_id) = sym_id {
                self.out.symbol_documentation(sym_id, doc_id);
            }
        }
        if let Some(sym_id) = sym_id {
            if !info.display_name.is_empty() {
                self.display_name_facts(info.display_name, sym_id);
            }
            if !info.enclosing_symbol.is_empty() {
                let enclosing_symbol_id = self.get_symbol_id(&info.enclosing_symbol, filepath);
                if let Some(enclosing_symbol_id) = enclosing_symbol_id {
                    self.out.enclosing_symbol(sym_id, enclosing_symbol_id);
                }
            }
            info.relationships.iter().for_each(|rel| {
                if rel.is_implementation {
                    let implemented_symbol = &rel.symbol;
                    let implemented_symbol_id = self.get_symbol_id(implemented_symbol, filepath);
                    if let Some(implemented_symbol_id) = implemented_symbol_id {
                        self.out.is_implementation(sym_id, implemented_symbol_id);
                    }
                }
            });
        }

        Ok(())
    }

    fn get_symbol_id(&mut self, symbol: &str, filepath: &str) -> Option<ScipId> {
        let qualified_symbol = qualify_scip_symbol(symbol, filepath);
        self.get_def_fact_id(StringPredicate::Symbol, &qualified_symbol)
    }

    fn display_name_facts(&mut self, display_name: String, sym_id: ScipId) {
        let display_name = display_name.into_boxed_str();
        let (display_name_id, seen) =
            self.get_or_set_fact(StringPredicate::DisplayName, display_name.clone());
        if !seen {
            self.out.display_name(display_name_id, display_name);
        }
        self.out.display_name_symbol(sym_id, display_name_id);
    }

    fn decode_scip_occurrence(
        &mut self,
        file_id: ScipId,
        filepath: &str,
        occ: Occurrence,
    ) -> Result<()> {
        let symbol = parse_scip_symbol(&occ.symbol);
        let symbol_hint = symbol_range_hint(&symbol);

        let file_range_id = self.next_id();
        let range = self
            .decode_range_for_file(file_id, &occ.range, symbol_hint.as_deref())?
            .unwrap();
        self.out.file_range(file_range_id, file_id, range);
        let enclosing_range =
            self.decode_range_for_file(file_id, &occ.enclosing_range, symbol_hint.as_deref())?;
        match enclosing_range {
            None => {}
            Some(enclosing_range) => {
                let enclosing_file_range_id = self.next_id();
                self.out
                    .file_range(enclosing_file_range_id, file_id, enclosing_range);
                let enclosing_range_id = self.next_id();
                self.out.enclosing_range(
                    enclosing_range_id,
                    file_range_id,
                    enclosing_file_range_id,
                );
            }
        }

        match symbol {
            ScipSymbol::Local { id } => {
                self.decode_local_occurrence(
                    format!("local {}", id),
                    SymbolRoleSet(occ.symbol_roles),
                    file_range_id,
                    filepath,
                );
            }
            ScipSymbol::Global { descriptors, .. } => {
                self.decode_global_occurrence(
                    occ.symbol,
                    SymbolRoleSet(occ.symbol_roles),
                    file_range_id,
                    descriptors,
                );
            }
        }

        Ok(())
    }

    fn decode_range_for_file(
        &self,
        file_id: ScipId,
        range: &[i32],
        symbol_hint: Option<&str>,
    ) -> Result<Option<GleanRange>> {
        let Some(range) = decode_scip_range(range)? else {
            return Ok(None);
        };

        Ok(Some(
            self.go_line_directive_maps
                .get(&file_id)
                .map_or(range.clone(), |map| map.remap_range(range, symbol_hint)),
        ))
    }

    fn decode_local_occurrence(
        &mut self,
        local_symbol: String,
        sym_roles: SymbolRoleSet,
        file_range_id: ScipId,
        filepath: &str,
    ) {
        let qualified_symbol = format!("{}/{}", filepath, local_symbol).into_boxed_str();
        let kind_override = self.kind_overrides.get(&qualified_symbol).copied();
        let (symbol_id, seen_symbol) =
            self.get_or_set_fact(StringPredicate::Symbol, qualified_symbol.clone());
        if !seen_symbol {
            self.out.symbol(symbol_id, qualified_symbol.clone());
        }
        if sym_roles.has_def() {
            self.out.definition(symbol_id, file_range_id);
        } else {
            self.out.reference(symbol_id, file_range_id);
        }

        let local_symbol = local_symbol.into_boxed_str();
        let (name_id, seen_name) =
            self.get_or_set_fact(StringPredicate::LocalName, local_symbol.clone());
        if !seen_name {
            self.out.local_name(name_id, local_symbol.clone());
        }
        if !seen_symbol {
            self.out.symbol_name(symbol_id, name_id);
            // Prefer SymbolInformation.kind from doc.symbols when present;
            // otherwise the local has no descriptor-derived kind to fall back
            // to, so default to SkVariable as before.
            let kind = kind_override.unwrap_or(SymbolKind::SkVariable);
            self.out.symbol_kind(symbol_id, kind);
        }
    }

    fn decode_global_occurrence(
        &mut self,
        scip_symbol: String,
        sym_roles: SymbolRoleSet,
        file_range_id: ScipId,
        descriptors: Vec<Descriptor>,
    ) {
        let scip_symbol = scip_symbol.into_boxed_str();
        let kind_override = self.kind_overrides.get(&scip_symbol).copied();
        let (symbol_id, seen_symbol) =
            self.get_or_set_fact(StringPredicate::Symbol, scip_symbol.clone());
        if !seen_symbol {
            self.out.symbol(symbol_id, scip_symbol.clone());
        }
        if sym_roles.has_def() {
            self.out.definition(symbol_id, file_range_id);
        } else {
            self.out.reference(symbol_id, file_range_id);
        }

        // Use the last descriptor's name for the local name
        let local_name = descriptors
            .last()
            .map(|d| d.name.to_owned().into_boxed_str())
            .unwrap_or_else(|| "".to_owned().into_boxed_str());

        let (name_id, seen_name) =
            self.get_or_set_fact(StringPredicate::LocalName, local_name.clone());
        if !seen_name {
            self.out.local_name(name_id, local_name);
        }
        if !seen_symbol {
            self.out.symbol_name(symbol_id, name_id);
        }

        // Prefer SymbolInformation.kind when set; fall back to the
        // descriptor-derived kind. This is what lets us distinguish e.g. Go
        // `const` from `var` — both share the Term descriptor suffix and would
        // otherwise both map to SkVariable.
        let kind =
            kind_override.or_else(|| descriptors.last().map(|d| SymbolKind::new(d.kind.clone())));
        if let Some(kind) = kind {
            if kind != SymbolKind::SkUnknown {
                self.out.symbol_kind(symbol_id, kind);
            }
        }
    }
}

struct SymbolRoleSet(i32);
impl SymbolRoleSet {
    fn has_def(&self) -> bool {
        self.0 & (1 << 0) != 0
    }
}

/// Convert a SCIP `SymbolInformation.Kind` to the local `SymbolKind`.
///
/// This override is intentionally **narrow** due to big blast radius (all scip DBs).
/// We should eventually onboard all Kinds here, such that:
///   if SymbolInformation.kind is set, it will override any symbol string suffix
fn scip_kind_to_symbol_kind(kind: symbol_information::Kind) -> Option<SymbolKind> {
    use SymbolKind::*;
    use symbol_information::Kind;
    Some(match kind {
        Kind::Variable | Kind::StaticVariable | Kind::Value => SkVariable,
        Kind::Constant => SkConstant,
        Kind::Class => SkClass,
        Kind::Function => SkFunction,
        Kind::Constructor => SkConstructor,
        Kind::Method | Kind::StaticMethod => SkMethod,
        Kind::Module => SkModule,
        Kind::Interface => SkInterface,
        Kind::Enum => SkEnum,

        // No override yet, defer to the descriptor-derived (symbol string suffix) kind.
        _ => return None,
    })
}

/// Build the lookup key for a SCIP symbol fact. Local symbols are namespaced
/// by their containing file (mirroring how `scip_symbol::parse_scip_symbol`
/// distinguishes `Local` from `Global`); global symbols are used as-is.
///
/// Centralized here so every site that needs to look up or insert a symbol
/// fact uses the same key shape — divergence here silently breaks lookups.
fn qualify_scip_symbol(symbol: &str, filepath: &str) -> Box<str> {
    match parse_scip_symbol(symbol) {
        ScipSymbol::Local { .. } => format!("{}/{}", filepath, symbol).into_boxed_str(),
        ScipSymbol::Global { .. } => symbol.to_owned().into_boxed_str(),
    }
}

fn symbol_range_hint(symbol: &ScipSymbol) -> Option<String> {
    match symbol {
        ScipSymbol::Local { .. } => None,
        ScipSymbol::Global { descriptors, .. } => descriptors
            .iter()
            .rev()
            .find_map(|descriptor| match &descriptor.kind {
                DescriptorKind::Namespace | DescriptorKind::Meta => None,
                _ => Some(descriptor.name.as_str()),
            })
            .filter(|name| !name.is_empty() && *name != "_")
            .map(str::to_owned),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compute_file_lines_empty() {
        let (lengths, ends_in_newline, has_unicode_or_tabs) = compute_file_lines(b"");
        assert_eq!(lengths, Vec::<u64>::new());
        assert!(!ends_in_newline);
        assert!(!has_unicode_or_tabs);
    }

    #[test]
    fn test_compute_file_lines_single_newline() {
        let (lengths, ends_in_newline, _) = compute_file_lines(b"\n");
        assert_eq!(lengths, vec![1]);
        assert!(ends_in_newline);
    }

    #[test]
    fn test_compute_file_lines_single_line_with_newline() {
        let (lengths, ends_in_newline, has_unicode_or_tabs) = compute_file_lines(b"hello\n");
        assert_eq!(lengths, vec![6]); // 5 chars + 1 newline
        assert!(ends_in_newline);
        assert!(!has_unicode_or_tabs);
    }

    #[test]
    fn test_compute_file_lines_no_trailing_newline() {
        let (lengths, ends_in_newline, _) = compute_file_lines(b"hello");
        assert_eq!(lengths, vec![5]);
        assert!(!ends_in_newline);
    }

    #[test]
    fn test_compute_file_lines_multiple_lines() {
        let (lengths, ends_in_newline, _) =
            compute_file_lines(b"package main\n\nfunc main() {\n}\n");
        // "package main\n" = 13, "\n" = 1, "func main() {\n" = 14, "}\n" = 2
        assert_eq!(lengths, vec![13, 1, 14, 2]);
        assert!(ends_in_newline);
    }

    #[test]
    fn test_compute_file_lines_with_tabs() {
        let (lengths, _, has_unicode_or_tabs) = compute_file_lines(b"func() {\n\treturn\n}\n");
        assert_eq!(lengths, vec![9, 8, 2]);
        assert!(has_unicode_or_tabs);
    }

    #[test]
    fn test_compute_file_lines_with_unicode() {
        // UTF-8 encoded "é" is 2 bytes (0xC3 0xA9)
        let (lengths, ends_in_newline, has_unicode_or_tabs) =
            compute_file_lines("café\n".as_bytes());
        // c(1) + a(1) + f(1) + é(2) + \n(1) = 6 bytes
        assert_eq!(lengths, vec![6]);
        assert!(ends_in_newline);
        assert!(has_unicode_or_tabs);
    }

    #[test]
    fn test_compute_file_lines_multiple_no_trailing_newline() {
        let (lengths, ends_in_newline, _) = compute_file_lines(b"line1\nline2");
        assert_eq!(lengths, vec![6, 5]); // "line1\n" = 6, "line2" = 5
        assert!(!ends_in_newline);
    }

    #[test]
    fn test_normalize_filepath_no_dots() {
        assert_eq!(
            normalize_filepath("foo/bar/baz.go"),
            Some("foo/bar/baz.go".to_string())
        );
    }

    #[test]
    fn test_normalize_filepath_with_parent() {
        assert_eq!(
            normalize_filepath("foo/bar/../baz.go"),
            Some("foo/baz.go".to_string())
        );
    }

    #[test]
    fn test_normalize_filepath_with_curdir() {
        assert_eq!(
            normalize_filepath("foo/./bar/baz.go"),
            Some("foo/bar/baz.go".to_string())
        );
    }

    #[test]
    fn test_normalize_filepath_too_many_parents() {
        assert_eq!(normalize_filepath("foo/../../baz.go"), None);
    }
}
