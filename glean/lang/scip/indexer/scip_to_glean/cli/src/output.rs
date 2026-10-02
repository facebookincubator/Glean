/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::rc::Rc;

use ahash::AHashMap as HashMap;
use ahash::AHashSet as HashSet;
use serde::Serialize;

use crate::GleanRange;
use crate::ToolInfo;
use crate::angle::ScipId;
use crate::lsif::LanguageId;
use crate::lsif::SymbolKind;

/// Index into `GleanJSONOutput::units`.
#[derive(Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
struct UnitId(u32);

/// A fact together with the Glean ownership unit it is attributed to, if any.
#[derive(Clone, Eq, PartialEq, Hash)]
struct Owned<T> {
    unit: Option<UnitId>,
    fact: T,
}

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
#[serde(rename_all = "camelCase")]
struct EnclosingRange {
    range: ScipId,
    enclosing_range: ScipId,
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
struct EnclosingSymbol {
    symbol: ScipId,
    enclosing: ScipId,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
struct SymbolAndKind {
    kind: u8,
    symbol: ScipId,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
#[serde(rename_all = "camelCase")]
struct FileLines {
    file: ScipId,
    lengths: Vec<u64>,
    ends_in_newline: bool,
    has_unicode_or_tabs: bool,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct Metadata {
    text_encoding: i32,
    tool_info: Option<ToolInfo>,
    version: i32,
}
#[derive(Serialize, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct DisplayNameSymbol {
    display_name: ScipId,
    symbol: ScipId,
}

/// A node in the fact graph.
#[derive(Eq, Hash, PartialEq, Clone)]
enum Node {
    SymbolName(Owned<Key<SymbolName>>),
    IsImplementation(Owned<Key<IsImplementation>>),
    EnclosingSymbol(Owned<Key<EnclosingSymbol>>),
    FileLanguage(Owned<IdKey<FileLang>>),
    SymbolKind(Owned<Key<SymbolAndKind>>),
    Definition(Owned<Key<SymbolLocation>>),
    Reference(Owned<Key<SymbolLocation>>),
    SymbolDocumentation(Owned<IdKey<SymbolDocs>>),
    File(Owned<IdKey<Box<str>>>),
    FileRange(Owned<IdKey<FileRange>>),
    EnclosingRange(Owned<IdKey<EnclosingRange>>),
    LocalName(Owned<IdKey<Box<str>>>),
    Symbol(Owned<IdKey<Box<str>>>),
    Documentation(Owned<IdKey<Box<str>>>),
    FileLines(Owned<Key<FileLines>>),
    DisplayNameSymbol(Owned<Key<DisplayNameSymbol>>),
    DisplayName(Owned<IdKey<Box<str>>>),
}

/// The JSON output we will generate, suitable for Glean to import.
#[derive(Default)]
pub struct GleanJSONOutput {
    /// Names of the ownership units, shared with the shards of this output.
    units: Rc<Vec<Box<str>>>,
    unit_ids: HashMap<Box<str>, UnitId>,
    current_unit: Option<UnitId>,
    src_files: Vec<Owned<IdKey<Box<str>>>>,
    file_langs: Vec<Owned<IdKey<FileLang>>>,
    documentation: Vec<Owned<IdKey<Box<str>>>>,
    symbol_documentation: Vec<Owned<IdKey<SymbolDocs>>>,
    file_ranges: Vec<Owned<IdKey<FileRange>>>,
    enclosing_ranges: Vec<Owned<IdKey<EnclosingRange>>>,
    symbols: Vec<Owned<IdKey<Box<str>>>>,
    definitions: Vec<Owned<Key<SymbolLocation>>>,
    references: Vec<Owned<Key<SymbolLocation>>>,
    local_names: Vec<Owned<IdKey<Box<str>>>>,
    symbol_names: Vec<Owned<Key<SymbolName>>>,
    is_implementation: Vec<Owned<Key<IsImplementation>>>,
    enclosing_symbols: Vec<Owned<Key<EnclosingSymbol>>>,
    symbol_kinds: Vec<Owned<Key<SymbolAndKind>>>,
    metadata: Vec<Owned<Key<Metadata>>>,
    display_names: Vec<Owned<IdKey<Box<str>>>>,
    display_name_symbols: Vec<Owned<Key<DisplayNameSymbol>>>,
    file_lines: Vec<Owned<Key<FileLines>>>,
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
                Node::EnclosingSymbol(node) => output.enclosing_symbols.push(node),
                Node::FileLanguage(node) => output.file_langs.push(node),
                Node::File(node) => output.src_files.push(node),
                Node::FileRange(node) => output.file_ranges.push(node),
                Node::EnclosingRange(node) => output.enclosing_ranges.push(node),
                Node::SymbolKind(node) => output.symbol_kinds.push(node),
                Node::Definition(node) => output.definitions.push(node),
                Node::Reference(node) => output.references.push(node),
                Node::SymbolDocumentation(node) => output.symbol_documentation.push(node),
                Node::LocalName(node) => output.local_names.push(node),
                Node::Symbol(node) => output.symbols.push(node),
                Node::Documentation(node) => output.documentation.push(node),
                Node::FileLines(node) => output.file_lines.push(node),
                Node::DisplayNameSymbol(node) => output.display_name_symbols.push(node),
                Node::DisplayName(node) => output.display_names.push(node),
            }
        }

        output
    }
}

impl GleanJSONOutput {
    /// Attributes the facts emitted from now on to the ownership unit `unit`,
    /// or to no unit if `None`.
    pub fn set_unit(&mut self, unit: Option<&str>) {
        let unit_id = unit.map(|name| self.intern_unit(name));
        self.current_unit = unit_id;
    }

    fn intern_unit(&mut self, name: &str) -> UnitId {
        if let Some(unit_id) = self.unit_ids.get(name) {
            return *unit_id;
        }
        let units = Rc::make_mut(&mut self.units);
        let unit_id = UnitId(
            u32::try_from(units.len()).expect("should have fewer than 2^32 ownership units"),
        );
        units.push(name.into());
        self.unit_ids.insert(name.into(), unit_id);
        unit_id
    }

    fn owned<T>(&self, fact: T) -> Owned<T> {
        Owned {
            unit: self.current_unit,
            fact,
        }
    }

    pub fn src_file(&mut self, src_file_id: ScipId, path: Box<str>) {
        self.src_files.push(self.owned(IdKey {
            id: src_file_id,
            key: path,
        }));
    }
    pub fn file_lang(&mut self, lang_file_id: ScipId, src_file_id: ScipId, lang: LanguageId) {
        self.file_langs.push(self.owned(IdKey {
            id: lang_file_id,
            key: FileLang {
                file: src_file_id,
                language: lang as u8,
            },
        }));
    }
    pub fn documentation(&mut self, doc_id: ScipId, text: Box<str>) {
        self.documentation.push(self.owned(IdKey {
            id: doc_id,
            key: text,
        }));
    }
    pub fn symbol_documentation(&mut self, symbol_id: ScipId, doc_id: ScipId) {
        self.symbol_documentation.push(self.owned(IdKey {
            id: doc_id,
            key: SymbolDocs {
                symbol: symbol_id,
                docs: doc_id,
            },
        }));
    }

    pub fn file_range(&mut self, file_range_id: ScipId, file_id: ScipId, range: GleanRange) {
        self.file_ranges.push(self.owned(IdKey {
            id: file_range_id,
            key: FileRange {
                file: file_id,
                range,
            },
        }));
    }

    pub fn enclosing_range(&mut self, id: ScipId, range: ScipId, enclosing_range: ScipId) {
        self.enclosing_ranges.push(self.owned(IdKey {
            id,
            key: EnclosingRange {
                range,
                enclosing_range,
            },
        }));
    }
    pub fn symbol(&mut self, symbol_id: ScipId, symbol: Box<str>) {
        self.symbols.push(self.owned(IdKey {
            id: symbol_id,
            key: symbol,
        }));
    }
    pub fn definition(&mut self, symbol_id: ScipId, file_range_id: ScipId) {
        self.definitions.push(self.owned(Key {
            key: SymbolLocation {
                symbol: symbol_id,
                location: file_range_id,
            },
        }));
    }
    pub fn reference(&mut self, symbol_id: ScipId, file_range_id: ScipId) {
        self.references.push(self.owned(Key {
            key: SymbolLocation {
                symbol: symbol_id,
                location: file_range_id,
            },
        }));
    }
    pub fn local_name(&mut self, name_id: ScipId, text: Box<str>) {
        self.local_names.push(self.owned(IdKey {
            id: name_id,
            key: text,
        }));
    }
    pub fn symbol_name(&mut self, symbol_id: ScipId, name_id: ScipId) {
        self.symbol_names.push(self.owned(Key {
            key: SymbolName {
                symbol: symbol_id,
                name: name_id,
            },
        }));
    }
    pub fn is_implementation(&mut self, symbol_id: ScipId, implemented_id: ScipId) {
        self.is_implementation.push(self.owned(Key {
            key: IsImplementation {
                symbol: symbol_id,
                implemented: implemented_id,
            },
        }));
    }
    pub fn enclosing_symbol(&mut self, symbol_id: ScipId, enclosing_id: ScipId) {
        self.enclosing_symbols.push(self.owned(Key {
            key: EnclosingSymbol {
                symbol: symbol_id,
                enclosing: enclosing_id,
            },
        }));
    }
    pub fn symbol_kind(&mut self, symbol_id: ScipId, kind: SymbolKind) {
        self.symbol_kinds.push(self.owned(Key {
            key: SymbolAndKind {
                symbol: symbol_id,
                kind: kind as u8,
            },
        }));
    }
    pub fn metadata(&mut self, version: i32, text_encoding: i32, tool_info: Option<ToolInfo>) {
        self.metadata.push(self.owned(Key {
            key: Metadata {
                version,
                text_encoding,
                tool_info,
            },
        }));
    }
    pub fn display_name(&mut self, fact_id: ScipId, name: Box<str>) {
        self.display_names.push(self.owned(IdKey {
            id: fact_id,
            key: name,
        }));
    }
    pub fn display_name_symbol(&mut self, symbol_id: ScipId, name_id: ScipId) {
        self.display_name_symbols.push(self.owned(Key {
            key: DisplayNameSymbol {
                symbol: symbol_id,
                display_name: name_id,
            },
        }));
    }
    pub fn file_lines(
        &mut self,
        file_id: ScipId,
        lengths: Vec<u64>,
        ends_in_newline: bool,
        has_unicode_or_tabs: bool,
    ) {
        self.file_lines.push(self.owned(Key {
            key: FileLines {
                file: file_id,
                lengths,
                ends_in_newline,
                has_unicode_or_tabs,
            },
        }));
    }

    pub fn total_facts_count(&self) -> usize {
        self.src_files.len()
            + self.file_langs.len()
            + self.documentation.len()
            + self.symbol_documentation.len()
            + self.file_ranges.len()
            + self.enclosing_ranges.len()
            + self.symbols.len()
            + self.definitions.len()
            + self.references.len()
            + self.local_names.len()
            + self.symbol_names.len()
            + self.is_implementation.len()
            + self.enclosing_symbols.len()
            + self.symbol_kinds.len()
            + self.metadata.len()
            + self.display_names.len()
            + self.display_name_symbols.len()
            + self.file_lines.len()
    }

    /// Split this `GleanJSONOutput` value into a vec of smaller GleanJSONOutput
    /// values that each contain roughly `shard_size` items.
    ///
    /// Each smaller value is a self-contained SCIP subgraph, per the SCIP
    /// schema definition. This enables us to do multiple smaller writes to
    /// Glean without needing any tracking state.
    pub fn shard(self, shard_size: usize) -> Vec<Self> {
        let shard_capacity = shard_size.min(self.total_facts_count());

        // Exhaustive so that a new field can't be silently left out of every shard.
        let GleanJSONOutput {
            units,
            unit_ids: _,
            current_unit: _,
            src_files,
            file_langs,
            documentation,
            symbol_documentation,
            file_ranges,
            enclosing_ranges,
            symbols,
            definitions,
            references,
            local_names,
            symbol_names,
            is_implementation,
            enclosing_symbols,
            symbol_kinds,
            metadata,
            display_names,
            display_name_symbols,
            file_lines,
        } = self;

        // Lookup tables, inline to avoid annoying lifetime specifiers
        let files = src_files
            .iter()
            .map(|x| (x.fact.id, x))
            .collect::<HashMap<_, _>>();
        let documentation = documentation
            .iter()
            .map(|x| (x.fact.id, x))
            .collect::<HashMap<_, _>>();
        let file_ranges = file_ranges
            .iter()
            .map(|x| (x.fact.id, x))
            .collect::<HashMap<_, _>>();
        let symbols = symbols
            .iter()
            .map(|x| (x.fact.id, x))
            .collect::<HashMap<_, _>>();
        let local_names = local_names
            .iter()
            .map(|x| (x.fact.id, x))
            .collect::<HashMap<_, _>>();
        let display_names = display_names
            .iter()
            .map(|x| (x.fact.id, x))
            .collect::<HashMap<_, _>>();

        let mut source_nodes: Vec<Node> = Vec::with_capacity(
            symbol_names.len()
                + is_implementation.len()
                + enclosing_symbols.len()
                + file_langs.len()
                + symbol_kinds.len()
                + definitions.len()
                + references.len()
                + enclosing_ranges.len()
                + symbol_documentation.len()
                + file_lines.len()
                + display_name_symbols.len(),
        );
        source_nodes.extend(symbol_names.into_iter().map(Node::SymbolName));
        source_nodes.extend(is_implementation.into_iter().map(Node::IsImplementation));
        source_nodes.extend(enclosing_symbols.into_iter().map(Node::EnclosingSymbol));
        source_nodes.extend(file_langs.into_iter().map(Node::FileLanguage));
        source_nodes.extend(symbol_kinds.into_iter().map(Node::SymbolKind));
        source_nodes.extend(definitions.into_iter().map(Node::Definition));
        source_nodes.extend(references.into_iter().map(Node::Reference));
        source_nodes.extend(enclosing_ranges.into_iter().map(Node::EnclosingRange));
        source_nodes.extend(
            symbol_documentation
                .into_iter()
                .map(Node::SymbolDocumentation),
        );
        source_nodes.extend(file_lines.into_iter().map(Node::FileLines));
        source_nodes.extend(
            display_name_symbols
                .into_iter()
                .map(Node::DisplayNameSymbol),
        );

        let mut shards: Vec<Self> = Vec::new();

        let mut current_graph: HashSet<Node> = HashSet::with_capacity(shard_capacity);

        let mut to_visit: Vec<Node> = Vec::new();

        // source nodes are our entry into each subgraph
        for node in source_nodes {
            if current_graph.len() >= shard_size {
                shards.push(current_graph.into());

                current_graph = HashSet::with_capacity(shard_capacity);
            }

            to_visit.push(node);

            while let Some(node) = to_visit.pop() {
                if !current_graph.contains(&node) {
                    match &node {
                        Node::SymbolName(symbol_name) => {
                            let localname = *local_names.get(&symbol_name.fact.key.name).unwrap();
                            let symbol = *symbols.get(&symbol_name.fact.key.symbol).unwrap();
                            to_visit.push(Node::LocalName(localname.clone()));
                            to_visit.push(Node::Symbol(symbol.clone()));
                        }
                        Node::IsImplementation(is_implementation) => {
                            let symbol = *symbols.get(&is_implementation.fact.key.symbol).unwrap();
                            let implemented = *symbols
                                .get(&is_implementation.fact.key.implemented)
                                .unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                            to_visit.push(Node::Symbol(implemented.clone()));
                        }
                        Node::EnclosingSymbol(enclosing_symbol) => {
                            let symbol = *symbols.get(&enclosing_symbol.fact.key.symbol).unwrap();
                            let enclosing =
                                *symbols.get(&enclosing_symbol.fact.key.enclosing).unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                            to_visit.push(Node::Symbol(enclosing.clone()));
                        }
                        Node::FileLanguage(file_language) => {
                            let file = *files.get(&file_language.fact.key.file).unwrap();
                            to_visit.push(Node::File(file.clone()));
                        }
                        Node::FileRange(file_range) => {
                            let file = *files.get(&file_range.fact.key.file).unwrap();
                            to_visit.push(Node::File(file.clone()));
                        }
                        Node::EnclosingRange(enclosing_range) => {
                            let EnclosingRange {
                                range,
                                enclosing_range,
                            } = &enclosing_range.fact.key;
                            let range_idkey = *file_ranges.get(range).unwrap();
                            let enclosing_range_idkey = *file_ranges.get(enclosing_range).unwrap();
                            to_visit.push(Node::FileRange(range_idkey.clone()));
                            to_visit.push(Node::FileRange(enclosing_range_idkey.clone()));
                        }
                        Node::SymbolKind(symbol_kind) => {
                            let symbol = *symbols.get(&symbol_kind.fact.key.symbol).unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                        }
                        Node::Definition(loc) | Node::Reference(loc) => {
                            let location = *file_ranges.get(&loc.fact.key.location).unwrap();
                            let symbol = *symbols.get(&loc.fact.key.symbol).unwrap();
                            to_visit.push(Node::FileRange(location.clone()));
                            to_visit.push(Node::Symbol(symbol.clone()));
                        }
                        Node::SymbolDocumentation(symbol_documentation) => {
                            let symbol =
                                *symbols.get(&symbol_documentation.fact.key.symbol).unwrap();
                            let doc = *documentation
                                .get(&symbol_documentation.fact.key.docs)
                                .unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                            to_visit.push(Node::Documentation(doc.clone()));
                        }
                        Node::FileLines(file_lines) => {
                            let file = *files.get(&file_lines.fact.key.file).unwrap();
                            to_visit.push(Node::File(file.clone()));
                        }
                        Node::DisplayNameSymbol(display_name_symbol) => {
                            let symbol =
                                *symbols.get(&display_name_symbol.fact.key.symbol).unwrap();
                            let display_name = *display_names
                                .get(&display_name_symbol.fact.key.display_name)
                                .unwrap();
                            to_visit.push(Node::Symbol(symbol.clone()));
                            to_visit.push(Node::DisplayName(display_name.clone()));
                        }
                        // sink nodes:
                        Node::LocalName(_) => {}
                        Node::Symbol(_) => {}
                        Node::Documentation(_) => {}
                        Node::File(_) => {}
                        Node::DisplayName(_) => {}
                    }
                    current_graph.insert(node);
                }
            }
        }

        shards.push(current_graph.into());

        // Metadata is one global fact, so every shard carries it and stays a complete DB input.
        for shard in &mut shards {
            shard.metadata = metadata.clone();
            shard.units = Rc::clone(&units);
        }

        shards
    }

    pub fn write(self, mut w: impl std::io::Write) -> std::io::Result<()> {
        /// State shared by the batches written for every predicate.
        struct Batches<'a> {
            units: &'a [Box<str>],
            /// Whether no batch has been written yet, so that the next one
            /// needs no leading comma.
            is_first: bool,
        }

        fn sub<T: Serialize>(
            mut w: impl std::io::Write,
            name: &str,
            mut items: Vec<Owned<T>>,
            batches: &mut Batches<'_>,
        ) -> std::io::Result<()> {
            if items.is_empty() {
                return Ok(());
            }

            // Reverse item list to match behavior of Haskell code, which puts the last entries first
            items.reverse();

            // Glean attributes all the facts of a batch to the batch's unit.
            let units: Vec<Option<UnitId>> = items.iter().map(|item| item.unit).collect();
            for (unit, indices) in group_by_unit(&units) {
                let unit = unit.map(|UnitId(index)| &*batches.units[index as usize]);

                // Chunk items into groups of 10k to match behavior of Haskell code.
                for chunk in indices.chunks(10000) {
                    // If this isn't the first line, include the trailing comma for the previous line
                    if !batches.is_first {
                        w.write_all(b",\n")?;
                    }

                    let facts: Vec<&T> = chunk.iter().map(|&i| &items[i].fact).collect();
                    w.write_all(br#"{"facts":"#)?;
                    serde_json::to_writer(&mut w, &facts)?;
                    write!(w, r#","predicate":"{}.1""#, name)?;
                    if let Some(unit) = unit {
                        w.write_all(br#","unit":"#)?;
                        serde_json::to_writer(&mut w, unit)?;
                    }
                    w.write_all(b"}")?;
                    batches.is_first = false;
                }
            }

            Ok(())
        }

        /// Groups the indices of `units` by unit, in unit order, each group
        /// keeping the original order of its indices.
        ///
        /// Takes the units rather than the facts so that the sort is compiled
        /// once, not once per predicate type.
        fn group_by_unit(units: &[Option<UnitId>]) -> Vec<(Option<UnitId>, Vec<usize>)> {
            let mut order: Vec<usize> = (0..units.len()).collect();
            order.sort_by_key(|&i| units[i]);
            order
                .chunk_by(|&a, &b| units[a] == units[b])
                .map(|group| (units[group[0]], group.to_vec()))
                .collect()
        }

        let batches = &mut Batches {
            units: &self.units,
            is_first: true,
        };

        w.write_all(b"[")?;
        // Match the ordering in scipDependencyOrder
        sub(&mut w, "src.File", self.src_files, batches)?;
        sub(&mut w, "src.FileLines", self.file_lines, batches)?;
        sub(&mut w, "scip.Symbol", self.symbols, batches)?;
        sub(&mut w, "scip.LocalName", self.local_names, batches)?;
        sub(&mut w, "scip.Documentation", self.documentation, batches)?;
        sub(&mut w, "scip.FileLanguage", self.file_langs, batches)?;
        sub(&mut w, "scip.FileRange", self.file_ranges, batches)?;
        sub(
            &mut w,
            "scip.EnclosingRange",
            self.enclosing_ranges,
            batches,
        )?;
        sub(&mut w, "scip.Definition", self.definitions, batches)?;
        sub(&mut w, "scip.Reference", self.references, batches)?;
        sub(
            &mut w,
            "scip.SymbolDocumentation",
            self.symbol_documentation,
            batches,
        )?;
        sub(&mut w, "scip.SymbolName", self.symbol_names, batches)?;
        sub(
            &mut w,
            "scip.IsImplementation",
            self.is_implementation,
            batches,
        )?;
        sub(
            &mut w,
            "scip.EnclosingSymbol",
            self.enclosing_symbols,
            batches,
        )?;
        sub(&mut w, "scip.SymbolKind", self.symbol_kinds, batches)?;
        sub(&mut w, "scip.Metadata", self.metadata, batches)?;
        sub(&mut w, "scip.DisplayName", self.display_names, batches)?;
        sub(
            &mut w,
            "scip.DisplayNameSymbol",
            self.display_name_symbols,
            batches,
        )?;
        w.write_all(b"]\n")?;

        // A buffered writer would otherwise flush on drop, which discards the
        // error, so a failed final write would look like success.
        w.flush()?;

        Ok(())
    }
}
