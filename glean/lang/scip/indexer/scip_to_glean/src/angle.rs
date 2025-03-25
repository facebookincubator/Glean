/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

// Use ahash instead of std hashmap for a slight performance gain.
use ahash::AHashMap as HashMap;
use anyhow::Result;
use proto_rust::scip::Document;
use proto_rust::scip::Metadata;
use proto_rust::scip::Occurrence;
use proto_rust::scip::SymbolInformation;
use serde::Serialize;

use crate::decode_scip_range;
use crate::lsif::LanguageId;
use crate::lsif::SymbolKind;
use crate::output::GleanJSONOutput;
use crate::Suffix;
use crate::ToolInfo;

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
    fact_id: HashMap<Box<str>, ScipId>,
    out: GleanJSONOutput,
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

    fn get_or_set_fact(&mut self, key: Box<str>) -> (ScipId, bool) {
        match self.fact_id.get(&key) {
            Some(id) => (*id, false),
            None => {
                let id = self.next_id();
                self.fact_id.insert(key, id);
                (id, true)
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
        doc: Document,
    ) -> Result<()> {
        let src_file_id = self.next_id();
        // todo - adjust filepath with prefix and suffix
        let mut filepath = doc.relative_path.to_owned();
        if let Some(path_prefix) = path_prefix {
            filepath = format!("{}{}", path_prefix, filepath);
        }
        let filepath = filepath.into_boxed_str();

        self.fact_id.insert(filepath.clone(), src_file_id);

        self.out.src_file(src_file_id, filepath.clone());
        let lang_file_id = self.next_id();

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
        }
        None
    }

    fn decode_scip_info(&mut self, filepath: &str, info: SymbolInformation) -> Result<()> {
        let symbol = info.symbol.replace("  ", "_");
        let symbol = symbol_from_string(&symbol);
        let qualified_symbol = match symbol {
            ScipSymbol::Local { .. } => {
                format!("{}/{}", filepath, info.symbol)
            }
            ScipSymbol::Global { .. } => info.symbol,
        }
        .into_boxed_str();

        let sym_id = self.fact_id.get(&qualified_symbol).copied();

        for document in info.documentation {
            let doc_id = self.next_id();
            self.out
                .documentation(doc_id, document.trim().to_string().into_boxed_str());
            if let Some(sym_id) = sym_id {
                self.out.symbol_documentation(sym_id, doc_id);
            }
        }

        Ok(())
    }

    fn decode_scip_occurrence(
        &mut self,
        file_id: ScipId,
        filepath: &str,
        occ: Occurrence,
    ) -> Result<()> {
        let file_range_id = self.next_id();
        self.out
            .file_range(file_range_id, file_id, decode_scip_range(&occ.range)?);

        let symbol = occ.symbol.replace("  ", "_");
        let symbol = symbol_from_string(&symbol);

        match symbol {
            ScipSymbol::Local { id } => {
                self.decode_local_occurrence(
                    format!("local {}", id),
                    SymbolRoleSet(occ.symbol_roles),
                    file_range_id,
                    filepath,
                );
            }
            ScipSymbol::Global { descriptor, .. } => {
                self.decode_global_occurrence(
                    occ.symbol,
                    SymbolRoleSet(occ.symbol_roles),
                    file_range_id,
                    descriptor,
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
        let (symbol_id, seen_symbol) = self.get_or_set_fact(qualified_symbol.clone());
        if seen_symbol {
            self.out.symbol(symbol_id, qualified_symbol);
        }
        if sym_roles.has_def() {
            self.out.definition(symbol_id, file_range_id);
        } else {
            self.out.reference(symbol_id, file_range_id);
        }

        let local_symbol = local_symbol.into_boxed_str();
        let (name_id, seen_name) = self.get_or_set_fact(local_symbol.clone());
        if seen_name {
            self.out.local_name(name_id, local_symbol.clone());
        }
        if seen_symbol {
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
        descriptor: Descriptor,
    ) {
        let scip_symbol = scip_symbol.into_boxed_str();
        let (symbol_id, seen_symbol) = self.get_or_set_fact(scip_symbol.clone());
        if seen_symbol {
            self.out.symbol(symbol_id, scip_symbol);
        }
        if sym_roles.has_def() {
            self.out.definition(symbol_id, file_range_id);
        } else {
            self.out.reference(symbol_id, file_range_id);
        }

        let local_name = descriptor.text.to_owned().into_boxed_str();
        let (name_id, seen_name) = self.get_or_set_fact(local_name.clone());
        if seen_name {
            self.out.local_name(name_id, local_name);
        }
        if seen_symbol {
            self.out.symbol_name(symbol_id, name_id);
        }

        let kind = SymbolKind::new(descriptor.suffix);
        if kind != SymbolKind::SkUnknown {
            self.out.symbol_kind(symbol_id, kind);
        }
    }
}

enum ScipSymbol<'a> {
    Local {
        id: &'a str,
    },
    Global {
        // scheme: &'a str,
        // manager: &'a str,
        // pkgname: &'a str,
        // version: &'a str,
        descriptor: Descriptor<'a>,
    },
}
struct Descriptor<'a> {
    text: &'a str,
    suffix: Suffix,
}

fn symbol_from_string<'a>(symbol: &'a str) -> ScipSymbol<'a> {
    if let Some(("local", rest)) = symbol.split_once(' ') {
        return ScipSymbol::Local { id: rest };
    }

    let (_scheme, rest) = symbol.split_once(' ').unwrap_or((symbol, ""));
    let (_manager, rest) = rest.split_once(' ').unwrap_or((rest, ""));
    let (_pkgname, rest) = rest.split_once(' ').unwrap_or((rest, ""));
    let (_version, sym_str) = rest.split_once(' ').unwrap_or((rest, ""));
    let descriptor = parse_suffix(sym_str);
    ScipSymbol::Global { descriptor }
}

fn parse_suffix<'a>(sym_str: &'a str) -> Descriptor<'a> {
    if sym_str.is_empty() {
        return Descriptor {
            text: "",
            suffix: Suffix::SymUnspecifiedSuffix,
        };
    }

    let (text, _) = sym_str.split_at(sym_str.len() - 1);
    let last = sym_str.as_bytes().last().unwrap();
    let suffix = match last {
        b'/' => Suffix::SymPackage,
        b'#' => Suffix::SymType,
        b'.' => {
            let last2 = text.as_bytes().last().copied();
            if last2 == Some(b')') {
                Suffix::SymMethod
            } else {
                Suffix::SymTerm
            }
        }
        b':' => Suffix::SymMeta,
        b']' => Suffix::SymTypeParameter,
        b')' => Suffix::SymParameter,
        _ => {
            return Descriptor {
                text: sym_str,
                suffix: Suffix::SymUnspecifiedSuffix,
            };
        }
    };

    Descriptor { text, suffix }
}

struct SymbolRoleSet(i32);
impl SymbolRoleSet {
    fn has_def(&self) -> bool {
        self.0 & (1 << 0) != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_from_string_global_type() {
        let symbol = "rust-analyzer cargo std https://github.com/rust-lang/rust/library/std io/stdio/IsTerminal#";
        let symbol = symbol_from_string(symbol);
        let ScipSymbol::Global { descriptor } = symbol else {
            panic!("expected global symbol");
        };
        assert_eq!(descriptor.text, "io/stdio/IsTerminal");
        assert_eq!(descriptor.suffix, Suffix::SymType);
    }

    #[test]
    fn test_symbol_from_string_global_unspecified() {
        let symbol =
            "rust-analyzer cargo std https://github.com/rust-lang/rust/library/std macros/println!";
        let symbol = symbol_from_string(symbol);
        let ScipSymbol::Global { descriptor } = symbol else {
            panic!("expected global symbol");
        };
        assert_eq!(descriptor.text, "macros/println!");
        assert_eq!(descriptor.suffix, Suffix::SymUnspecifiedSuffix);
    }
}
