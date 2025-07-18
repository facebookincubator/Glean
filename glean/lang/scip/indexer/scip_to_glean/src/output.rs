/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::HashMap;
use std::collections::HashSet;

use serde::Serialize;

use crate::GleanRange;
use crate::ToolInfo;
use crate::angle::ScipId;
use crate::lsif::LanguageId;
use crate::lsif::SymbolKind;

#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct IdKey<T> {
    id: ScipId,
    key: T,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct Key<T> {
    key: T,
}

#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct FileLang {
    file: ScipId,
    language: u8,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct FileRange {
    file: ScipId,
    range: GleanRange,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct SymbolLocation {
    location: ScipId,
    symbol: ScipId,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct SymbolDocs {
    docs: ScipId,
    symbol: ScipId,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct SymbolName {
    name: ScipId,
    symbol: ScipId,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct IsImplementation {
    symbol: ScipId,
    implemented: ScipId,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct SymbolAndKind {
    kind: u8,
    symbol: ScipId,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct Metadata {
    text_encoding: i32,
    tool_info: Option<ToolInfo>,
    version: i32,
}
#[derive(Serialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct DisplayNameSymbol {
    display_name: ScipId,
    symbol: ScipId,
}

#[derive(Eq, Hash, PartialEq, Clone)]
enum Node {
    SymbolName(Key<SymbolName>),
    IsImplementation(Key<IsImplementation>),
    FileLanguage(IdKey<FileLang>),
    SymbolKind(Key<SymbolAndKind>),
    Definition(Key<SymbolLocation>),
    Reference(Key<SymbolLocation>),
    SymbolDocumentation(IdKey<SymbolDocs>),
    File(IdKey<Box<str>>),
    FileRange(IdKey<FileRange>),
    LocalName(IdKey<Box<str>>),
    Symbol(IdKey<Box<str>>),
    Documentation(IdKey<Box<str>>),
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
    is_implementation: Vec<Key<IsImplementation>>,
    symbol_kinds: Vec<Key<SymbolAndKind>>,
    metadata: Vec<Key<Metadata>>,
    display_names: Vec<IdKey<Box<str>>>,
    display_name_symbols: Vec<Key<DisplayNameSymbol>>,
}

impl<I> From<I> for GleanJSONOutput
where
    I: IntoIterator<Item = Node>,
{
    fn from(nodes: I) -> Self {
        let mut output = GleanJSONOutput::default();
        for node in nodes {
            match node {
                Node::SymbolName(node) => output.symbol_names.push(node),
                Node::IsImplementation(node) => output.is_implementation.push(node),
                Node::FileLanguage(node) => output.file_langs.push(node),
                Node::File(node) => output.src_files.push(node),
                Node::FileRange(node) => output.file_ranges.push(node),
                Node::SymbolKind(node) => output.symbol_kinds.push(node),
                Node::Definition(node) => output.definitions.push(node),
                Node::Reference(node) => output.references.push(node),
                Node::SymbolDocumentation(node) => output.symbol_documentation.push(node),
                Node::LocalName(node) => output.local_names.push(node),
                Node::Symbol(node) => output.symbols.push(node),
                Node::Documentation(node) => output.documentation.push(node),
            }
        }

        output
    }
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
    pub fn is_implementation(&mut self, symbol_id: ScipId, implemented_id: ScipId) {
        self.is_implementation.push(Key {
            key: IsImplementation {
                symbol: symbol_id,
                implemented: implemented_id,
            },
        });
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
    pub fn display_name(&mut self, fact_id: ScipId, name: Box<str>) {
        self.display_names.push(IdKey {
            id: fact_id,
            key: name,
        })
    }
    pub fn display_name_symbol(&mut self, symbol_id: ScipId, name_id: ScipId) {
        self.display_name_symbols.push(Key {
            key: DisplayNameSymbol {
                symbol: symbol_id,
                display_name: name_id,
            },
        })
    }

    pub fn total_facts_count(&self) -> usize {
        self.src_files.len()
            + self.file_langs.len()
            + self.documentation.len()
            + self.symbol_documentation.len()
            + self.file_ranges.len()
            + self.symbols.len()
            + self.definitions.len()
            + self.references.len()
            + self.local_names.len()
            + self.symbol_names.len()
            + self.is_implementation.len()
            + self.symbol_kinds.len()
            + self.metadata.len()
            + self.display_names.len()
            + self.display_name_symbols.len()
    }

    /// Consumes self, returns a list of GleanJSONOutput shards that are approximately of size `shard_size`
    /// The shards are complete SCIP subgraphs, per the SCIP schema definition
    /// This facilitates smaller writes to Glean without global, stateful keys
    pub fn shard(self, shard_size: usize) -> Vec<Self> {
        // Lookup tables, inline to avoid annoying lifetime specifiers
        let files = self
            .src_files
            .iter()
            .map(|x| (x.id, x))
            .collect::<HashMap<_, _>>();
        let documentation = self
            .documentation
            .iter()
            .map(|x| (x.id, x))
            .collect::<HashMap<_, _>>();
        let file_ranges = self
            .file_ranges
            .iter()
            .map(|x| (x.id, x))
            .collect::<HashMap<_, _>>();
        let symbols = self
            .symbols
            .iter()
            .map(|x| (x.id, x))
            .collect::<HashMap<_, _>>();
        let local_names = self
            .local_names
            .iter()
            .map(|x| (x.id, x))
            .collect::<HashMap<_, _>>();

        let mut source_nodes: Vec<Node> = Vec::new();
        source_nodes.extend(self.symbol_names.into_iter().map(Node::SymbolName));
        source_nodes.extend(
            self.is_implementation
                .into_iter()
                .map(Node::IsImplementation),
        );
        source_nodes.extend(self.file_langs.into_iter().map(Node::FileLanguage));
        source_nodes.extend(self.symbol_kinds.into_iter().map(Node::SymbolKind));
        source_nodes.extend(self.definitions.into_iter().map(Node::Definition));
        source_nodes.extend(self.references.into_iter().map(Node::Reference));
        source_nodes.extend(
            self.symbol_documentation
                .into_iter()
                .map(Node::SymbolDocumentation),
        );

        let mut shards = Vec::new();

        let mut current_graph: HashSet<Node> = HashSet::new();

        // source nodes are our entry into each subgraph
        for node in source_nodes {
            if current_graph.len() >= shard_size {
                shards.push(current_graph.into());

                current_graph = HashSet::new();
            }

            let mut to_visit = vec![node.clone()];

            while let Some(node) = to_visit.pop() {
                if !current_graph.contains(&node) {
                    match &node {
                        Node::SymbolName(symbol_name) => {
                            let localname = *local_names.get(&symbol_name.key.name).unwrap();
                            let symbol = *symbols.get(&symbol_name.key.symbol).unwrap();
                            to_visit.push(Node::LocalName(localname.clone()));
                            to_visit.push(Node::Symbol(symbol.clone()));
                        }
                        Node::IsImplementation(is_implementation) => {
                            let symbol = *symbols.get(&is_implementation.key.symbol).unwrap();
                            let implemented =
                                *symbols.get(&is_implementation.key.implemented).unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                            to_visit.push(Node::Symbol(implemented.clone()));
                        }
                        Node::FileLanguage(file_language) => {
                            let file = *files.get(&file_language.key.file).unwrap();
                            to_visit.push(Node::File(file.clone()));
                        }
                        Node::FileRange(file_range) => {
                            let file = *files.get(&file_range.key.file).unwrap();
                            to_visit.push(Node::File(file.clone()));
                        }
                        Node::SymbolKind(symbol_kind) => {
                            let symbol = *symbols.get(&symbol_kind.key.symbol).unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                        }
                        Node::Definition(loc) | Node::Reference(loc) => {
                            let location = *file_ranges.get(&loc.key.location).unwrap();
                            let symbol = *symbols.get(&loc.key.symbol).unwrap();
                            to_visit.push(Node::FileRange(location.clone()));
                            to_visit.push(Node::Symbol(symbol.clone()));
                        }
                        Node::SymbolDocumentation(symbol_documentation) => {
                            let symbol = *symbols.get(&symbol_documentation.key.symbol).unwrap();
                            let doc = *documentation.get(&symbol_documentation.key.docs).unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                            to_visit.push(Node::Documentation(doc.clone()));
                        }
                        // sink nodes:
                        Node::LocalName(_) => {}
                        Node::Symbol(_) => {}
                        Node::Documentation(_) => {}
                        Node::File(_) => {}
                    }
                    current_graph.insert(node);
                }
            }
        }

        shards.push(current_graph.into());

        shards
    }

    pub fn write(self, mut w: impl std::io::Write) -> std::io::Result<()> {
        fn sub(
            mut w: impl std::io::Write,
            name: &str,
            mut items: Vec<impl Serialize>,
            is_first_line: &mut bool,
        ) -> std::io::Result<()> {
            if items.is_empty() {
                return Ok(());
            }

            // Reverse item list to match behavior of Haskell code, which puts the last entries first
            items.reverse();

            // Chunk items into groups of 10k to match behavior of Haskell code.
            for chunk in items.chunks(10000) {
                // If this isn't the first line, include the trailing comma for the previous line
                if !*is_first_line {
                    w.write_all(b",\n")?;
                }

                w.write_all(br#"{"facts":"#)?;
                serde_json::to_writer(&mut w, &chunk)?;
                write!(w, r#","predicate":"{}.1"}}"#, name)?;
                *is_first_line = false;
            }

            Ok(())
        }

        // Track whether we're on the first line of the JSON output
        // so we can add a trailing comma to the previous line
        // This will be passed by mutable reference to sub()
        let mut is_first_line = true;
        let ifl = &mut is_first_line;

        w.write_all(b"[")?;
        // Match the ordering in scipDependencyOrder
        sub(&mut w, "src.File", self.src_files, ifl)?;
        sub(&mut w, "scip.Symbol", self.symbols, ifl)?;
        sub(&mut w, "scip.LocalName", self.local_names, ifl)?;
        sub(&mut w, "scip.Documentation", self.documentation, ifl)?;
        sub(&mut w, "scip.FileLanguage", self.file_langs, ifl)?;
        sub(&mut w, "scip.FileRange", self.file_ranges, ifl)?;
        sub(&mut w, "scip.Definition", self.definitions, ifl)?;
        sub(&mut w, "scip.Reference", self.references, ifl)?;
        sub(
            &mut w,
            "scip.SymbolDocumentation",
            self.symbol_documentation,
            ifl,
        )?;
        sub(&mut w, "scip.SymbolName", self.symbol_names, ifl)?;
        sub(&mut w, "scip.IsImplementation", self.is_implementation, ifl)?;
        sub(&mut w, "scip.SymbolKind", self.symbol_kinds, ifl)?;
        sub(&mut w, "scip.Metadata", self.metadata, ifl)?;
        sub(&mut w, "scip.DisplayName", self.display_names, ifl)?;
        sub(
            &mut w,
            "scip.DisplayNameSymbol",
            self.display_name_symbols,
            ifl,
        )?;
        w.write_all(b"]\n")?;

        Ok(())
    }
}
