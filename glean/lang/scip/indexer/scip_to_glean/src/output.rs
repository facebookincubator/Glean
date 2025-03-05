/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde::Serialize;

use crate::angle::ScipId;
use crate::lsif::LanguageId;
use crate::lsif::SymbolKind;
use crate::GleanRange;
use crate::ToolInfo;

#[derive(Serialize)]
struct IdKey<T> {
    id: ScipId,
    key: T,
}
#[derive(Serialize)]
struct Key<T> {
    key: T,
}

#[derive(Serialize)]
struct FileLang {
    file: ScipId,
    language: u8,
}
#[derive(Serialize)]
struct FileRange {
    file: ScipId,
    range: GleanRange,
}
#[derive(Serialize)]
struct SymbolLocation {
    location: ScipId,
    symbol: ScipId,
}
#[derive(Serialize)]
struct SymbolDocs {
    docs: ScipId,
    symbol: ScipId,
}
#[derive(Serialize)]
struct SymbolName {
    name: ScipId,
    symbol: ScipId,
}
#[derive(Serialize)]
struct SymbolAndKind {
    kind: u8,
    symbol: ScipId,
}
#[derive(Serialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct Metadata {
    text_encoding: i32,
    tool_info: Option<ToolInfo>,
    version: i32,
}

#[derive(Default)]
pub struct GleanJSONOutput {
    src_files: Vec<IdKey<Box<str>>>,
    file_langs: Vec<IdKey<FileLang>>,
    documentation: Vec<IdKey<Box<str>>>,
    symbol_documentation: Vec<IdKey<SymbolDocs>>,
    file_ranges: Vec<IdKey<FileRange>>,
    symbols: Vec<IdKey<Box<str>>>,
    definitions: Vec<Key<SymbolLocation>>,
    references: Vec<Key<SymbolLocation>>,
    local_names: Vec<IdKey<Box<str>>>,
    symbol_names: Vec<Key<SymbolName>>,
    symbol_kinds: Vec<Key<SymbolAndKind>>,
    metadata: Vec<Key<Metadata>>,
}
impl GleanJSONOutput {
    pub fn src_file(&mut self, src_file_id: ScipId, path: Box<str>) {
        self.src_files.push(IdKey {
            id: src_file_id,
            key: path,
        })
    }
    pub fn file_lang(&mut self, lang_file_id: ScipId, src_file_id: ScipId, lang: LanguageId) {
        self.file_langs.push(IdKey {
            id: lang_file_id,
            key: FileLang {
                file: src_file_id,
                language: lang as u8,
            },
        })
    }
    pub fn documentation(&mut self, doc_id: ScipId, text: Box<str>) {
        self.documentation.push(IdKey {
            id: doc_id,
            key: text,
        })
    }
    pub fn symbol_documentation(&mut self, symbol_id: ScipId, doc_id: ScipId) {
        self.symbol_documentation.push(IdKey {
            id: doc_id,
            key: SymbolDocs {
                symbol: symbol_id,
                docs: doc_id,
            },
        })
    }

    pub fn file_range(&mut self, file_range_id: ScipId, file_id: ScipId, range: GleanRange) {
        self.file_ranges.push(IdKey {
            id: file_range_id,
            key: FileRange {
                file: file_id,
                range,
            },
        })
    }
    pub fn symbol(&mut self, symbol_id: ScipId, symbol: Box<str>) {
        self.symbols.push(IdKey {
            id: symbol_id,
            key: symbol,
        })
    }
    pub fn definition(&mut self, symbol_id: ScipId, file_range_id: ScipId) {
        self.definitions.push(Key {
            key: SymbolLocation {
                symbol: symbol_id,
                location: file_range_id,
            },
        })
    }
    pub fn reference(&mut self, symbol_id: ScipId, file_range_id: ScipId) {
        self.references.push(Key {
            key: SymbolLocation {
                symbol: symbol_id,
                location: file_range_id,
            },
        })
    }
    pub fn local_name(&mut self, name_id: ScipId, text: Box<str>) {
        self.local_names.push(IdKey {
            id: name_id,
            key: text,
        })
    }
    pub fn symbol_name(&mut self, symbol_id: ScipId, name_id: ScipId) {
        self.symbol_names.push(Key {
            key: SymbolName {
                symbol: symbol_id,
                name: name_id,
            },
        })
    }
    pub fn symbol_kind(&mut self, symbol_id: ScipId, kind: SymbolKind) {
        self.symbol_kinds.push(Key {
            key: SymbolAndKind {
                symbol: symbol_id,
                kind: kind as u8,
            },
        })
    }
    pub fn metadata(&mut self, version: i32, text_encoding: i32, tool_info: Option<ToolInfo>) {
        self.metadata.push(Key {
            key: Metadata {
                version,
                text_encoding,
                tool_info,
            },
        })
    }

    pub fn write(self, mut w: impl std::io::Write) -> std::io::Result<()> {
        fn sub(
            mut w: impl std::io::Write,
            name: &str,
            mut items: Vec<impl Serialize>,
            include_trailing_comma: bool,
        ) -> std::io::Result<()> {
            if items.is_empty() {
                return Ok(());
            }

            // Reverse item list to match behavior of Haskell code, which puts the last entries first
            items.reverse();

            // Chunk items into groups of 10k to match behavior of Haskell code.
            for chunk in items.chunks(10000) {
                w.write_all(br#"{"facts":"#)?;
                serde_json::to_writer(&mut w, &chunk)?;
                write!(w, r#","predicate":"{}.1"}}"#, name)?;
                if include_trailing_comma {
                    w.write_all(b",\n")?;
                }
            }

            Ok(())
        }

        w.write_all(b"[")?;
        // Match the ordering in scipDependencyOrder
        sub(&mut w, "src.File", self.src_files, true)?;
        sub(&mut w, "scip.Symbol", self.symbols, true)?;
        sub(&mut w, "scip.LocalName", self.local_names, true)?;
        sub(&mut w, "scip.Documentation", self.documentation, true)?;
        sub(&mut w, "scip.FileLanguage", self.file_langs, true)?;
        sub(&mut w, "scip.FileRange", self.file_ranges, true)?;
        sub(&mut w, "scip.Definition", self.definitions, true)?;
        sub(&mut w, "scip.Reference", self.references, true)?;
        sub(
            &mut w,
            "scip.SymbolDocumentation",
            self.symbol_documentation,
            true,
        )?;
        sub(&mut w, "scip.SymbolName", self.symbol_names, true)?;
        sub(&mut w, "scip.SymbolKind", self.symbol_kinds, true)?;
        sub(&mut w, "scip.Metadata", self.metadata, false)?;
        w.write_all(b"]")?;

        Ok(())
    }
}
