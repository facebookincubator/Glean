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

        self.out.src_file(src_file_id, filepath);
        let lang_file_id = self.next_id();

        let lang = LanguageId::new(&doc.language)
            .known()
            .or(default_lang)
            .unwrap_or_default();
        self.out.file_lang(lang_file_id, src_file_id, lang);

        for occ in doc.occurrences {
            self.decode_scip_occurrence(src_file_id, occ)?;
        }

        for info in doc.symbols {
            self.decode_scip_info(info);
        }

        Ok(())
    }

    fn decode_scip_info(&mut self, info: SymbolInformation) {
        let sym_id = self.fact_id.get(&info.symbol.into_boxed_str()).copied();
        for document in info.documentation {
            let doc_id = self.next_id();
            self.out
                .documentation(doc_id, document.trim().to_string().into_boxed_str());
            if let Some(sym_id) = sym_id {
                self.out.symbol_documentation(sym_id, doc_id);
            }
        }
    }

    fn decode_scip_occurrence(&mut self, file_id: ScipId, occ: Occurrence) -> Result<()> {
        let file_range_id = self.next_id();
        self.out
            .file_range(file_range_id, file_id, decode_scip_range(&occ.range)?);

        let symbol = occ.symbol.replace("  ", "_");
        let symbol = symbol_from_string(&symbol)
            .ok_or_else(|| anyhow::Error::msg(format!("failed to parse symbol: {}", symbol)))?;

        if let ScipSymbol::Global { descriptor, .. } = symbol {
            self.decode_global_occurrence(
                occ.symbol,
                SymbolRoleSet(occ.symbol_roles),
                file_range_id,
                descriptor,
            )?;
        }

        Ok(())
    }

    fn decode_global_occurrence(
        &mut self,
        scip_symbol: String,
        sym_roles: SymbolRoleSet,
        file_range_id: ScipId,
        descriptor: Descriptor,
    ) -> Result<()> {
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
        Ok(())
    }
}

enum ScipSymbol<'a> {
    Local,
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

fn symbol_from_string<'a>(symbol: &'a str) -> Option<ScipSymbol<'a>> {
    let (scheme, rest) = symbol.split_once(' ')?;
    if scheme == "local" {
        // Haskell code does nothing with local symbols, so don't bother fully parsing them
        return Some(ScipSymbol::Local);
    }

    let (_manager, rest) = rest.split_once(' ')?;
    let (_pkgname, rest) = rest.split_once(' ')?;
    let (_version, sym_str) = rest.split_once(' ')?;
    let descriptor = parse_suffix(sym_str);
    Some(ScipSymbol::Global { descriptor })
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
        _ => Suffix::SymUnspecifiedSuffix,
    };

    Descriptor { text, suffix }
}

struct SymbolRoleSet(i32);
impl SymbolRoleSet {
    fn has_def(&self) -> bool {
        self.0 & (1 << 0) != 0
    }
}
