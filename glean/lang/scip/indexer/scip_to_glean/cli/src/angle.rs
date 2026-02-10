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
use proto_rust::scip::Document;
use proto_rust::scip::Metadata;
use proto_rust::scip::Occurrence;
use proto_rust::scip::SymbolInformation;
use scip_symbol::Descriptor;
use scip_symbol::ScipSymbol;
use scip_symbol::parse_scip_symbol;
use serde::Serialize;

use crate::ToolInfo;
use crate::decode_scip_range;
use crate::lsif::LanguageId;
use crate::lsif::SymbolKind;
use crate::output::GleanJSONOutput;

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

impl Env {
    pub fn new() -> Self {
        Self {
            unique: 1,
            fact_id: HashMap::new(),
            out: GleanJSONOutput::default(),
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
        self.out.metadata(
            metadata.version.value(),
            metadata.text_document_encoding.value(),
            tool_info,
        );
    }

    pub fn decode_scip_doc(
        &mut self,
        default_lang: Option<LanguageId>,
        infer_language: bool,
        path_prefix: Option<&str>,
        strip_prefix: Option<&str>,
        doc: Document,
    ) -> Result<()> {
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

        // Determine language early to conditionally normalize TypeScript paths
        let lang = LanguageId::new(&doc.language)
            .known()
            .or_else(|| {
                if infer_language {
                    self.file_language_of(&filepath)
                } else {
                    None
                }
            })
            .or(default_lang)
            .unwrap_or_default();

        // Normalize paths for TypeScript files only
        // TODO T240234639: Remove once SCIP stops returning paths with ../
        if matches!(lang, LanguageId::TypeScript | LanguageId::TypeScriptReact) {
            match normalize_filepath(&filepath) {
                Some(normalized) => filepath = normalized,
                None => {
                    // Cannot normalize path properly (e.g., too many .. components)
                    // Log error and skip this document
                    tracing::warn!(
                        "Cannot normalize filepath '{}', will most likely break browser clients",
                        filepath
                    );
                    return Ok(());
                }
            }
        }

        let filepath = filepath.into_boxed_str();

        // Skip files if the same file has already been seen.
        // Note that this differs from the Haskell version, which does not have this check.
        let (src_file_id, already_seen) =
            self.get_or_set_fact(StringPredicate::File, filepath.clone());
        if already_seen {
            return Ok(());
        }

        self.out.src_file(src_file_id, filepath.clone());
        let lang_file_id = self.next_id();

        self.out.file_lang(lang_file_id, src_file_id, lang);

        for occ in doc.occurrences {
            self.decode_scip_occurrence(src_file_id, &filepath, occ)?;
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
        }
        None
    }

    fn decode_scip_info(&mut self, filepath: &str, info: SymbolInformation) -> Result<()> {
        let sym_id = self.get_symbol_id(&info.symbol, filepath);

        for document in info.documentation {
            let doc_id = self.next_id();
            self.out
                .documentation(doc_id, document.trim().to_string().into_boxed_str());
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

    fn get_symbol_id(&mut self, symbol: &String, filepath: &str) -> Option<ScipId> {
        let scip_symbol = parse_scip_symbol(symbol);
        let qualified_symbol = match scip_symbol {
            ScipSymbol::Local { .. } => format!("{}/{}", filepath, symbol),
            ScipSymbol::Global { .. } => symbol.clone(),
        }
        .into_boxed_str();

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
        let file_range_id = self.next_id();
        self.out.file_range(
            file_range_id,
            file_id,
            decode_scip_range(&occ.range)?.unwrap(),
        );
        match decode_scip_range(&occ.enclosing_range)? {
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

        let symbol = parse_scip_symbol(&occ.symbol);

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

    fn decode_local_occurrence(
        &mut self,
        local_symbol: String,
        sym_roles: SymbolRoleSet,
        file_range_id: ScipId,
        filepath: &str,
    ) {
        let qualified_symbol = format!("{}/{}", filepath, local_symbol).into_boxed_str();
        let (symbol_id, seen_symbol) =
            self.get_or_set_fact(StringPredicate::Symbol, qualified_symbol.clone());
        if !seen_symbol {
            self.out.symbol(symbol_id, qualified_symbol);
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
            // TODO: this could be any SymbolInformation.kind
            self.out.symbol_kind(symbol_id, SymbolKind::SkVariable);
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
        let (symbol_id, seen_symbol) =
            self.get_or_set_fact(StringPredicate::Symbol, scip_symbol.clone());
        if !seen_symbol {
            self.out.symbol(symbol_id, scip_symbol);
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

        // Use the last descriptor's kind for the symbol kind
        if let Some(last_descriptor) = descriptors.last() {
            let kind = SymbolKind::new(last_descriptor.kind.clone());
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
