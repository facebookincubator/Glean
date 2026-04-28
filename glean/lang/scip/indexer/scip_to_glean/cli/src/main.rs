/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs::File;
use std::fs::OpenOptions;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use anyhow::Error;
use anyhow::Result;
use anyhow::anyhow;
use clap::Parser;
#[cfg(feature = "facebook")]
use fbinit::FacebookInit;
use protobuf::Message;
use serde::Deserialize;
use serde::Serialize;
use tracing::info;

#[cfg(not(feature = "facebook"))]
mod proto {
    include!(concat!(env!("OUT_DIR"), "/proto_gen/mod.rs"));
}
#[cfg(not(feature = "facebook"))]
use proto::scip::Index;
#[cfg(feature = "facebook")]
use proto_rust::scip::Index;

use crate::angle::Env;
use crate::lsif::LanguageId;

mod angle;
mod lsif;
mod output;

/// CLI for converting SCIP to Glean facts json
#[derive(Parser, Debug)]
#[command(
    author = "rl_code_authoring",
    about = "CLI for converting SCIP to Glean facts json"
)]
struct BuildJsonArgs {
    #[arg(short, long)]
    input: Vec<PathBuf>,
    #[arg(short, long)]
    output: PathBuf,

    #[arg(
        long,
        help = "Infer language for .java and .hk files when language is not set"
    )]
    infer_language: bool,

    #[arg(
        long,
        help = "The default language to use for files without a recognized extension."
    )]
    language: Option<String>,

    #[arg(long, help = "Prefix to prepend to filepaths.")]
    root_prefix: Option<String>,

    #[arg(long, help = "Strip this prefix from the filepaths")]
    strip_prefix: Option<String>,

    #[arg(
        long,
        help = "Root directory for source files. When set, enables src.FileLines emission by reading files from disk at <source-root>/<relative-path>"
    )]
    source_root: Option<PathBuf>,

    #[arg(
        long,
        help = "Shards the JSON graph into subgraphs. Subgraphs will be approximately the size specified. Uses the --output argument as a directory, and writes one file per shard"
    )]
    shard: Option<usize>,
}

#[cfg(feature = "facebook")]
#[cli::main("scip_to_glean", error_logging(user(default_level = "info")))]
async fn main(_fb: FacebookInit, args: BuildJsonArgs) -> Result<cli::ExitCode> {
    build_json(args)?;
    Ok(cli::ExitCode::SUCCESS)
}

#[cfg(not(feature = "facebook"))]
fn main() -> Result<()> {
    env_logger::init();
    let args = BuildJsonArgs::parse();
    build_json(args)
}

fn decode_scip_data(
    env: &mut Env,
    path: &Path,
    default_language: Option<LanguageId>,
    infer_language: bool,
    path_prefix: Option<&str>,
    strip_prefix: Option<&str>,
    source_root: Option<&Path>,
) -> Result<()> {
    info!("Loading documents from {}", path.display());
    let scip_index = read_scip_file(path)
        .with_context(|| format!("Error opening input file {}", path.display()))?;
    let num_docs = scip_index.documents.len();
    info!(
        "Loaded {} {}",
        num_docs,
        if num_docs == 1 {
            "document"
        } else {
            "documents"
        }
    );

    if let Some(metadata) = scip_index.metadata.into_option() {
        env.decode_scip_metadata(metadata);
    }

    // Pre-pass: register every document's `SymbolInformation.kind` overrides
    // before any document is decoded. Without this, an occurrence in document
    // B referencing a symbol whose kind lives in document A would fall back to
    // the descriptor-derived kind, which conflicts with the authoritative kind
    // emitted when document A is processed — yielding two contradictory
    // `scip.SymbolKind` facts for the same symbol.
    let documents = scip_index.documents;
    for doc in &documents {
        env.register_kind_overrides_for_doc(
            default_language,
            infer_language,
            path_prefix,
            strip_prefix,
            doc,
        );
    }
    for doc in documents {
        env.decode_scip_doc(
            default_language,
            infer_language,
            path_prefix,
            strip_prefix,
            source_root,
            doc,
        )?;
    }

    if !scip_index.external_symbols.is_empty() {
        info!(
            "Processing {} external symbols",
            scip_index.external_symbols.len()
        );
        for ext_sym in scip_index.external_symbols {
            env.decode_external_symbol(ext_sym)?;
        }
    }

    Ok(())
}

fn human_size(bytes: u64) -> String {
    const KIB: f64 = 1024.0;
    const MIB: f64 = 1024.0 * KIB;
    const GIB: f64 = 1024.0 * MIB;
    let bytes_f = bytes as f64;
    if bytes_f >= GIB {
        format!("{:.1} GiB", bytes_f / GIB)
    } else if bytes_f >= MIB {
        format!("{:.1} MiB", bytes_f / MIB)
    } else if bytes_f >= KIB {
        format!("{:.1} KiB", bytes_f / KIB)
    } else {
        format!("{} bytes", bytes)
    }
}

fn build_json(args: BuildJsonArgs) -> Result<()> {
    println!("{:?}", args);
    let default_language = args
        .language
        .as_ref()
        .and_then(|s| LanguageId::new(s).known());

    let mut env = Env::new();
    for input in &args.input {
        decode_scip_data(
            &mut env,
            input,
            default_language,
            args.infer_language,
            args.root_prefix.as_deref(),
            args.strip_prefix.as_deref(),
            args.source_root.as_deref(),
        )?;
    }

    let output_facts = env.output();
    let num_facts = output_facts.total_facts_count();
    info!(
        "Found {} {} total",
        num_facts,
        if num_facts == 1 { "fact" } else { "facts" }
    );
    let shards = if let Some(shard_size) = args.shard {
        let shards = output_facts.shard(shard_size);
        // pad the output files for correct numerical sorting
        let padding = shards.len().to_string().len();
        shards
            .into_iter()
            .enumerate()
            .map(|(i, shard)| {
                let output = args
                    .output
                    .join(format!("{:0width$}.json", i, width = padding));
                (output, shard)
            })
            .collect()
    } else {
        vec![(args.output, output_facts)]
    };

    let num_files = shards.len();
    let mut total_bytes: u64 = 0;
    for (file, shard) in shards {
        let write = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&file)
            .with_context(|| format!("Error creating output file {}", file.display()))?;
        let writer = std::io::BufWriter::new(write);

        shard.write(writer)?;
        total_bytes += std::fs::metadata(&file).map(|m| m.len()).unwrap_or(0);
    }
    info!(
        "Wrote {} {} ({})",
        num_files,
        if num_files == 1 { "file" } else { "files" },
        human_size(total_bytes)
    );

    Ok(())
}

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct ToolInfo {
    tool_args: Vec<String>,
    tool_name: String,
    version: String,
}

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct GleanRange {
    column_begin: u64,
    column_end: u64,
    line_begin: u64,
    line_end: u64,
}

fn decode_scip_range(range: &[i32]) -> Result<Option<GleanRange>> {
    // Some scip files have invalid ranges, the best we can do is to fix them up to 0 or 1
    // If we don't this will cause integer overflow when we convert to glean facts
    let mut range = match range {
        [] => None,
        [line_begin, column_begin, column_end] => Some(GleanRange {
            line_begin: std::cmp::max(*line_begin + 1, 1) as u64,
            column_begin: std::cmp::max(*column_begin + 1, 1) as u64,
            line_end: std::cmp::max(*line_begin + 1, 1) as u64,
            column_end: std::cmp::max(*column_end, 0) as u64,
        }),
        [line_begin, column_begin, line_end, column_end] => Some(GleanRange {
            line_begin: std::cmp::max(*line_begin + 1, 1) as u64,
            column_begin: std::cmp::max(*column_begin + 1, 1) as u64,
            line_end: std::cmp::max(*line_end + 1, 1) as u64,
            column_end: std::cmp::max(*column_end, 0) as u64,
        }),
        _ => {
            return Err(anyhow!("bad range: {:#?}", range));
        }
    };
    if let Some(ref mut r) = range {
        r.column_end = std::cmp::max(r.column_begin, r.column_end);
    }
    Ok(range)
}

fn read_scip_file(file: &Path) -> Result<Index, Error> {
    let scip_file = File::open(file)?;
    let mut reader = std::io::BufReader::new(scip_file);
    Index::parse_from_reader(&mut reader).context("Failed to deserialize scip file")
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "facebook")]
    use proto_rust::scip::Document;
    #[cfg(feature = "facebook")]
    use proto_rust::scip::Occurrence as ScipOccurrence;
    #[cfg(feature = "facebook")]
    use proto_rust::scip::SymbolInformation as ScipSymbolInformation;
    #[cfg(feature = "facebook")]
    use proto_rust::scip::symbol_information;
    use tempfile::NamedTempFile;

    #[cfg(not(feature = "facebook"))]
    use super::proto::scip::Document;
    #[cfg(not(feature = "facebook"))]
    use super::proto::scip::Occurrence as ScipOccurrence;
    #[cfg(not(feature = "facebook"))]
    use super::proto::scip::SymbolInformation as ScipSymbolInformation;
    #[cfg(not(feature = "facebook"))]
    use super::proto::scip::symbol_information;
    use super::*;
    use crate::lsif::SymbolKind;

    fn build_args(scip_path: PathBuf, output_path: PathBuf) -> BuildJsonArgs {
        BuildJsonArgs {
            input: vec![scip_path],
            output: output_path,
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        }
    }

    /// Find the kind value emitted for the symbol fact whose key string
    /// contains `symbol_substr`.
    fn find_kind_for_symbol(json: &str, symbol_substr: &str) -> Option<u64> {
        let symbols = find_predicate_facts(json, "scip.Symbol.1")?;
        let kinds = find_predicate_facts(json, "scip.SymbolKind.1")?;
        let symbol_id = symbols.as_array()?.iter().find_map(|f| {
            let key = f["key"].as_str()?;
            if key.contains(symbol_substr) {
                f["id"].as_u64()
            } else {
                None
            }
        })?;
        kinds.as_array()?.iter().find_map(|f| {
            if f["key"]["symbol"].as_u64() == Some(symbol_id) {
                f["key"]["kind"].as_u64()
            } else {
                None
            }
        })
    }

    #[test]
    fn test_write_blank_scip() {
        let scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: true,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };

        build_json(args).expect("failure building JSON");

        let output_json =
            std::fs::read_to_string(output_json.path()).expect("unable to read output");

        assert_eq!(output_json, "[]\n");
    }

    #[test]
    fn test_write_sharded_scip() {
        let scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json_dir = tempfile::TempDir::new().expect("Unable to create temp dir");

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json_dir.path().to_path_buf(),
            infer_language: true,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: Some(100),
        };

        build_json(args).expect("failure building JSON");

        let files_in_dir: Vec<_> = std::fs::read_dir(output_json_dir.path())
            .expect("unable to read output directory")
            .collect();
        let file_count = files_in_dir.len();
        assert_eq!(
            file_count, 1,
            "Expected exactly one file in the output directory"
        );

        let output_file_path = files_in_dir[0].as_ref().unwrap().path();
        let output_json = std::fs::read_to_string(output_file_path).expect("unable to read output");

        assert_eq!(output_json, "[]\n");
    }

    /// Helper to create a SCIP index file with a single document
    fn write_scip_index(scip_file: &mut impl std::io::Write, doc: Document) {
        let mut index = Index::new();
        index.documents.push(doc);
        index
            .write_to_writer(scip_file)
            .expect("failed to write SCIP index");
    }

    /// Helper to write a full Index (with documents and external_symbols)
    fn write_scip_index_full(scip_file: &mut impl std::io::Write, index: Index) {
        index
            .write_to_writer(scip_file)
            .expect("failed to write SCIP index");
    }

    /// Helper to find a predicate's facts array in the output JSON
    fn find_predicate_facts(json: &str, predicate: &str) -> Option<serde_json::Value> {
        let parsed: serde_json::Value =
            serde_json::from_str(json).expect("output is not valid JSON");
        let entries = parsed.as_array().expect("top-level should be array");
        entries
            .iter()
            .find(|entry| entry["predicate"].as_str() == Some(predicate))
            .map(|entry| entry["facts"].clone())
    }

    #[test]
    fn test_file_lines_from_document_text() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut doc = Document::new();
        doc.relative_path = "test.go".to_string();
        doc.language = "go".to_string();
        doc.text = "package main\n\nfunc main() {\n}\n".to_string();
        write_scip_index(&mut scip_file, doc);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // Verify src.FileLines predicate is present
        let file_lines_facts = find_predicate_facts(&output, "src.FileLines.1")
            .expect("src.FileLines.1 predicate not found in output");
        let facts = file_lines_facts.as_array().expect("facts should be array");
        assert_eq!(facts.len(), 1, "expected exactly one FileLines fact");

        let fact = &facts[0]["key"];
        // "package main\n" = 13, "\n" = 1, "func main() {\n" = 14, "}\n" = 2
        let lengths: Vec<u64> = fact["lengths"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(lengths, vec![13, 1, 14, 2]);
        assert_eq!(fact["endsInNewline"], true);
        assert_eq!(fact["hasUnicodeOrTabs"], false);
    }

    #[test]
    fn test_file_lines_from_source_root() {
        let source_dir = tempfile::TempDir::new().expect("unable to create temp dir");
        let source_file_path = source_dir.path().join("hello.go");
        std::fs::write(&source_file_path, "package hello\n\tfmt.Println()\n")
            .expect("failed to write source file");

        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        // Document with no text — FileLines should come from disk via source_root
        let mut doc = Document::new();
        doc.relative_path = "hello.go".to_string();
        doc.language = "go".to_string();
        write_scip_index(&mut scip_file, doc);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: Some(source_dir.path().to_path_buf()),
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let file_lines_facts = find_predicate_facts(&output, "src.FileLines.1")
            .expect("src.FileLines.1 predicate not found in output");
        let facts = file_lines_facts.as_array().expect("facts should be array");
        assert_eq!(facts.len(), 1, "expected exactly one FileLines fact");

        let fact = &facts[0]["key"];
        // "package hello\n" = 14, "\tfmt.Println()\n" = 15
        let lengths: Vec<u64> = fact["lengths"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(lengths, vec![14, 15]);
        assert_eq!(fact["endsInNewline"], true);
        assert_eq!(fact["hasUnicodeOrTabs"], true); // has tab character
    }

    #[test]
    fn test_no_file_lines_without_source_root_or_text() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        // Document with no text and no source_root — no FileLines should be emitted
        let mut doc = Document::new();
        doc.relative_path = "test.go".to_string();
        doc.language = "go".to_string();
        write_scip_index(&mut scip_file, doc);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // src.FileLines should NOT be present
        assert!(
            find_predicate_facts(&output, "src.FileLines.1").is_none(),
            "src.FileLines should not be emitted without source_root or document text"
        );
    }

    #[test]
    fn test_external_symbols_create_symbol_and_documentation() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();

        // Add a document with a reference to an external symbol
        let mut doc = Document::new();
        doc.relative_path = "test.java".to_string();
        doc.language = "java".to_string();
        index.documents.push(doc);

        // Add an external symbol with documentation
        let mut ext_sym = ScipSymbolInformation::new();
        ext_sym.symbol = "semanticdb maven . . android/os/PowerManager#".to_string();
        ext_sym.documentation = vec!["Controls the power state of the device.".to_string()];
        ext_sym.display_name = "PowerManager".to_string();
        index.external_symbols.push(ext_sym);

        write_scip_index_full(&mut scip_file, index);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // Verify the external symbol was created
        let symbol_facts = find_predicate_facts(&output, "scip.Symbol.1")
            .expect("scip.Symbol.1 predicate not found");
        let symbols = symbol_facts.as_array().expect("facts should be array");
        let has_pm = symbols.iter().any(|f| {
            f["key"]
                .as_str()
                .is_some_and(|s| s.contains("PowerManager"))
        });
        assert!(has_pm, "Expected PowerManager symbol fact");

        // Verify documentation was attached
        let doc_facts = find_predicate_facts(&output, "scip.Documentation.1")
            .expect("scip.Documentation.1 predicate not found");
        let docs = doc_facts.as_array().expect("facts should be array");
        let has_doc = docs
            .iter()
            .any(|f| f["key"].as_str().is_some_and(|s| s.contains("power state")));
        assert!(has_doc, "Expected documentation for external symbol");

        // Verify display name was created
        let dn_facts = find_predicate_facts(&output, "scip.DisplayName.1")
            .expect("scip.DisplayName.1 predicate not found");
        let dns = dn_facts.as_array().expect("facts should be array");
        let has_dn = dns
            .iter()
            .any(|f| f["key"].as_str().is_some_and(|s| s == "PowerManager"));
        assert!(has_dn, "Expected display name for external symbol");
    }

    #[test]
    fn test_external_symbols_empty_symbol_skipped() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();

        // Add an external symbol with an empty symbol string (should be skipped)
        let ext_sym = ScipSymbolInformation::new();
        index.external_symbols.push(ext_sym);

        write_scip_index_full(&mut scip_file, index);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // No symbols should have been created
        assert!(
            find_predicate_facts(&output, "scip.Symbol.1").is_none(),
            "No symbols should be created for empty external symbol"
        );
    }

    #[test]
    fn test_external_symbols_dedup_with_occurrence() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();

        // Add a document with an occurrence referencing an external symbol
        let mut doc = Document::new();
        doc.relative_path = "test.java".to_string();
        doc.language = "java".to_string();

        let mut occ = ScipOccurrence::new();
        occ.symbol = "semanticdb maven . . android/os/PowerManager#".to_string();
        occ.range = vec![10, 5, 17]; // line 10, col 5-17
        occ.symbol_roles = 0; // reference, not definition
        doc.occurrences.push(occ);

        index.documents.push(doc);

        // Also add the same symbol as an external symbol with documentation
        let mut ext_sym = ScipSymbolInformation::new();
        ext_sym.symbol = "semanticdb maven . . android/os/PowerManager#".to_string();
        ext_sym.documentation = vec!["Controls the power state.".to_string()];
        index.external_symbols.push(ext_sym);

        write_scip_index_full(&mut scip_file, index);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // The symbol should exist exactly once (deduped)
        let symbol_facts = find_predicate_facts(&output, "scip.Symbol.1")
            .expect("scip.Symbol.1 predicate not found");
        let symbols = symbol_facts.as_array().expect("facts should be array");
        let pm_count = symbols
            .iter()
            .filter(|f| {
                f["key"]
                    .as_str()
                    .is_some_and(|s| s.contains("PowerManager"))
            })
            .count();
        assert_eq!(
            pm_count, 1,
            "PowerManager symbol should appear exactly once"
        );

        // There should be a reference
        let ref_facts = find_predicate_facts(&output, "scip.Reference.1")
            .expect("scip.Reference.1 predicate not found");
        assert!(
            !ref_facts.as_array().unwrap().is_empty(),
            "Expected at least one reference"
        );

        // Documentation from external_symbols should still be attached
        let doc_facts = find_predicate_facts(&output, "scip.Documentation.1")
            .expect("scip.Documentation.1 predicate not found");
        let docs = doc_facts.as_array().expect("facts should be array");
        let has_doc = docs
            .iter()
            .any(|f| f["key"].as_str().is_some_and(|s| s.contains("power state")));
        assert!(has_doc, "Expected documentation from external_symbols");
    }

    /// A Go-style global symbol whose `info.kind = Constant` is emitted as
    /// `SkConstant`, overriding the descriptor-derived `SkVariable` that the
    /// Term suffix `.` would otherwise produce. This is the bug that
    /// previously caused all Go consts to be misclassified as variables.
    #[test]
    fn test_symbol_kind_global_const_overrides_variable() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();
        let mut doc = Document::new();
        doc.relative_path = "initial/toplevel_decls.go".to_string();
        doc.language = "go".to_string();

        let global_const = "scip-go gomod sg/initial 0.1.test `sg/initial`/MY_THING.";

        let mut occ = ScipOccurrence::new();
        occ.symbol = global_const.to_string();
        occ.range = vec![3, 6, 14];
        occ.symbol_roles = 1; // Definition
        doc.occurrences.push(occ);

        let mut info = ScipSymbolInformation::new();
        info.symbol = global_const.to_string();
        info.kind = symbol_information::Kind::Constant.into();
        doc.symbols.push(info);

        index.documents.push(doc);
        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kind = find_kind_for_symbol(&output, "MY_THING")
            .expect("expected scip.SymbolKind for MY_THING");
        assert_eq!(
            kind,
            SymbolKind::SkConstant as u64,
            "Go const should be SkConstant, got {kind}"
        );
    }

    /// Without `info.kind`, a global Term-suffix symbol falls back to the
    /// descriptor-derived `SkVariable`. Pins the existing behavior so a
    /// future change to the override map can't silently change it.
    #[test]
    fn test_symbol_kind_global_term_defaults_to_variable() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();
        let mut doc = Document::new();
        doc.relative_path = "initial/toplevel_decls.go".to_string();
        doc.language = "go".to_string();

        let global_var = "scip-go gomod sg/initial 0.1.test `sg/initial`/initFunctions.";

        let mut occ = ScipOccurrence::new();
        occ.symbol = global_var.to_string();
        occ.range = vec![10, 4, 17];
        occ.symbol_roles = 1; // Definition
        doc.occurrences.push(occ);

        index.documents.push(doc);
        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kind = find_kind_for_symbol(&output, "initFunctions")
            .expect("expected scip.SymbolKind for initFunctions");
        assert_eq!(
            kind,
            SymbolKind::SkVariable as u64,
            "Term-suffix global without info.kind should remain SkVariable, got {kind}"
        );
    }

    /// Local symbols have no descriptor-derived kind. The kind override map
    /// must still apply, so a local with `info.kind = Constant` becomes
    /// SkConstant rather than the SkVariable default.
    #[test]
    fn test_symbol_kind_local_const_override() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();
        let mut doc = Document::new();
        doc.relative_path = "initial/toplevel_decls.go".to_string();
        doc.language = "go".to_string();

        let local_sym = "local 0";

        let mut occ = ScipOccurrence::new();
        occ.symbol = local_sym.to_string();
        occ.range = vec![5, 8, 16];
        occ.symbol_roles = 1; // Definition
        doc.occurrences.push(occ);

        let mut info = ScipSymbolInformation::new();
        info.symbol = local_sym.to_string();
        info.kind = symbol_information::Kind::Constant.into();
        doc.symbols.push(info);

        index.documents.push(doc);
        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // Local symbols are namespaced by file path in the symbol fact key.
        let kind = find_kind_for_symbol(&output, "local 0")
            .expect("expected scip.SymbolKind for local symbol");
        assert_eq!(
            kind,
            SymbolKind::SkConstant as u64,
            "Local with info.kind=Constant should be SkConstant, got {kind}"
        );
    }

    /// Pin the prior behavior for locals: with no `info.kind`, the local
    /// path falls back to its `SkVariable` default. Pairs with
    /// `test_symbol_kind_local_const_override` so the diff between the two
    /// shows exactly what the override mechanism changes.
    #[test]
    fn test_symbol_kind_local_no_info_defaults_to_variable() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();
        let mut doc = Document::new();
        doc.relative_path = "initial/toplevel_decls.go".to_string();
        doc.language = "go".to_string();

        let local_sym = "local 0";

        let mut occ = ScipOccurrence::new();
        occ.symbol = local_sym.to_string();
        occ.range = vec![5, 8, 16];
        occ.symbol_roles = 1; // Definition
        doc.occurrences.push(occ);

        // No SymbolInformation entry — exercises the fallback path.

        index.documents.push(doc);
        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kind = find_kind_for_symbol(&output, "local 0")
            .expect("expected scip.SymbolKind for local symbol");
        assert_eq!(
            kind,
            SymbolKind::SkVariable as u64,
            "Local without info.kind should default to SkVariable, got {kind}"
        );
    }

    /// Symmetric "before" test for globals: when `info.kind = Variable` is set
    /// explicitly, the result still maps to `SkVariable` (same as the
    /// descriptor would have produced). Proves the override mechanism honors
    /// upstream agreement, not just upstream disagreement — i.e., we are
    /// reading `info.kind` rather than only triggering when it differs from
    /// the descriptor.
    #[test]
    fn test_symbol_kind_global_explicit_variable_kind() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();
        let mut doc = Document::new();
        doc.relative_path = "initial/toplevel_decls.go".to_string();
        doc.language = "go".to_string();

        let global_var = "scip-go gomod sg/initial 0.1.test `sg/initial`/initFunctions.";

        let mut occ = ScipOccurrence::new();
        occ.symbol = global_var.to_string();
        occ.range = vec![10, 4, 17];
        occ.symbol_roles = 1; // Definition
        doc.occurrences.push(occ);

        let mut info = ScipSymbolInformation::new();
        info.symbol = global_var.to_string();
        info.kind = symbol_information::Kind::Variable.into();
        doc.symbols.push(info);

        index.documents.push(doc);
        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kind = find_kind_for_symbol(&output, "initFunctions")
            .expect("expected scip.SymbolKind for initFunctions");
        assert_eq!(
            kind,
            SymbolKind::SkVariable as u64,
            "Term symbol with info.kind=Variable should be SkVariable, got {kind}"
        );
    }

    /// Conservative-mapping boundary: an `info.kind` value the ingestor does
    /// not map (e.g. `Function`) on a Term-suffix symbol must fall back to
    /// the descriptor-derived kind (`SkVariable`) rather than producing
    /// `SkUnknown` or refusing to emit a kind. Pins the design choice that
    /// the override is *additive*, not *replacing*.
    #[test]
    fn test_symbol_kind_unrecognized_kind_falls_back_to_descriptor() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();
        let mut doc = Document::new();
        doc.relative_path = "initial/toplevel_decls.go".to_string();
        doc.language = "go".to_string();

        let global_sym = "scip-go gomod sg/initial 0.1.test `sg/initial`/notMappedKind.";

        let mut occ = ScipOccurrence::new();
        occ.symbol = global_sym.to_string();
        occ.range = vec![20, 4, 17];
        occ.symbol_roles = 1; // Definition
        doc.occurrences.push(occ);

        let mut info = ScipSymbolInformation::new();
        info.symbol = global_sym.to_string();
        // `Function` is not in the conservative override map.
        info.kind = symbol_information::Kind::Function.into();
        doc.symbols.push(info);

        index.documents.push(doc);
        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kind = find_kind_for_symbol(&output, "notMappedKind")
            .expect("expected scip.SymbolKind for notMappedKind");
        assert_eq!(
            kind,
            SymbolKind::SkVariable as u64,
            "Unmapped info.kind should fall back to descriptor SkVariable, got {kind}"
        );
    }

    /// External symbols (no occurrence) with `info.kind = Constant` should
    /// be emitted as SkConstant. Covers the `decode_external_symbol` path,
    /// which has its own kind-resolution branch separate from occurrences.
    #[test]
    fn test_symbol_kind_external_const() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();

        // Need at least one document so processing runs.
        let mut doc = Document::new();
        doc.relative_path = "test.go".to_string();
        doc.language = "go".to_string();
        index.documents.push(doc);

        let external_const =
            "scip-go gomod golang.org/x/example 1.0.0 `golang.org/x/example`/SomeConst.";
        let mut ext = ScipSymbolInformation::new();
        ext.symbol = external_const.to_string();
        ext.kind = symbol_information::Kind::Constant.into();
        index.external_symbols.push(ext);

        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kind = find_kind_for_symbol(&output, "SomeConst")
            .expect("expected scip.SymbolKind for external SomeConst");
        assert_eq!(
            kind,
            SymbolKind::SkConstant as u64,
            "External const should be SkConstant, got {kind}"
        );
    }

    /// Pin the prior behavior for externals: with no `info.kind`, the
    /// external symbol path falls back to the descriptor-derived kind. Pairs
    /// with `test_symbol_kind_external_const` so the diff between the two
    /// shows exactly what the override mechanism changes for externals.
    #[test]
    fn test_symbol_kind_external_no_info_uses_descriptor() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut index = Index::new();

        // Need at least one document so processing runs.
        let mut doc = Document::new();
        doc.relative_path = "test.go".to_string();
        doc.language = "go".to_string();
        index.documents.push(doc);

        let external_term =
            "scip-go gomod golang.org/x/example 1.0.0 `golang.org/x/example`/SomeTerm.";
        let mut ext = ScipSymbolInformation::new();
        ext.symbol = external_term.to_string();
        // No `kind` set — exercise the fallback path.
        index.external_symbols.push(ext);

        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kind = find_kind_for_symbol(&output, "SomeTerm")
            .expect("expected scip.SymbolKind for external SomeTerm");
        assert_eq!(
            kind,
            SymbolKind::SkVariable as u64,
            "External Term symbol without info.kind should fall back to SkVariable, got {kind}"
        );
    }

    /// Collect every distinct kind value emitted for the symbol fact whose key
    /// string contains `symbol_substr`. Mirrors `find_kind_for_symbol` but
    /// returns all kinds, sorted and deduplicated, so tests can pin cases
    /// where the same symbol receives more than one kind value across the
    /// pipeline.
    fn find_all_kinds_for_symbol(json: &str, symbol_substr: &str) -> Vec<u64> {
        let symbols =
            find_predicate_facts(json, "scip.Symbol.1").expect("scip.Symbol.1 not found in output");
        let kinds = find_predicate_facts(json, "scip.SymbolKind.1")
            .expect("scip.SymbolKind.1 not found in output");
        let symbol_id = symbols
            .as_array()
            .expect("symbol facts should be array")
            .iter()
            .find_map(|f| {
                let key = f["key"].as_str()?;
                if key.contains(symbol_substr) {
                    f["id"].as_u64()
                } else {
                    None
                }
            })
            .unwrap_or_else(|| panic!("no Symbol fact whose key contains {symbol_substr:?}"));
        let mut out: Vec<u64> = kinds
            .as_array()
            .expect("symbolkind facts should be array")
            .iter()
            .filter_map(|f| {
                if f["key"]["symbol"].as_u64() == Some(symbol_id) {
                    f["key"]["kind"].as_u64()
                } else {
                    None
                }
            })
            .collect();
        out.sort_unstable();
        out.dedup();
        out
    }

    /// Cross-document `SymbolInformation.kind` overrides must apply uniformly:
    /// a global symbol defined in document A with `info.kind = Constant` and
    /// referenced from a second document B should produce a *single*
    /// `SymbolKind` fact (`SkConstant`) for that symbol, not one fact per
    /// document. Pre-`Env::kind_overrides`, doc B's reference fell back to the
    /// descriptor-derived `SkVariable` and the symbol ended up with two
    /// contradictory kind facts — see the matching pinning test that landed
    /// in D102345695 immediately below this commit, which expects the
    /// pre-override `vec![SkVariable]` and is flipped here to `vec![SkConstant]`
    /// in lockstep with the implementation change.
    #[test]
    fn test_symbol_kind_cross_doc_constant_uses_global_override() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let global_const = "scip-rust cargo crate_a 0.0.0 `crate_a`/SHARED.";

        let mut index = Index::new();

        // Doc A defines SHARED and registers info.kind = Constant.
        let mut doc_a = Document::new();
        doc_a.relative_path = "crate_a.rs".to_string();
        doc_a.language = "rust".to_string();
        let mut def_occ = ScipOccurrence::new();
        def_occ.symbol = global_const.to_string();
        def_occ.range = vec![1, 0, 12];
        def_occ.symbol_roles = 1; // Definition
        doc_a.occurrences.push(def_occ);
        let mut info = ScipSymbolInformation::new();
        info.symbol = global_const.to_string();
        info.kind = symbol_information::Kind::Constant.into();
        doc_a.symbols.push(info);
        index.documents.push(doc_a);

        // Doc B only references SHARED — no SymbolInformation entry. The
        // override must still apply to its reference because the pre-pass
        // populated `Env::kind_overrides` from every document up front.
        let mut doc_b = Document::new();
        doc_b.relative_path = "crate_b.rs".to_string();
        doc_b.language = "rust".to_string();
        let mut ref_occ = ScipOccurrence::new();
        ref_occ.symbol = global_const.to_string();
        ref_occ.range = vec![3, 4, 16];
        ref_occ.symbol_roles = 0; // Reference
        doc_b.occurrences.push(ref_occ);
        index.documents.push(doc_b);

        write_scip_index_full(&mut scip_file, index);

        build_json(build_args(
            scip_file.path().to_path_buf(),
            output_json.path().to_path_buf(),
        ))
        .expect("failure building JSON");
        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let kinds = find_all_kinds_for_symbol(&output, "SHARED");
        assert_eq!(
            kinds,
            vec![SymbolKind::SkConstant as u64],
            "Cross-doc constant should emit a single SkConstant fact: doc_b's \
             reference must see the kind override registered from doc_a, not \
             fall back to the descriptor-derived SkVariable."
        );
    }
}
